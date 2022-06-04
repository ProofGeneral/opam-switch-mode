;;; opam-mode.el --- select opam switches within emacs  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Hendrik Tews
;;
;; Authors: Hendrik Tews
;; Maintainer: Hendrik Tews <hendrik@askra.de>
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;; 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License in file COPYING in this or one of the parent
;; directories for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with "prooftree". If not, see <http://www.gnu.org/licenses/>.
;; 
;; 
;;; Commentary:
;;
;; Provide command `opam-set-switch' to change the opam switch of the
;; running emacs session and minor mode `opam-mode' to select the opam
;; switch via a menu bar menu.
;;
;; `opam-set-switch' reads the name of the switch in the minibuffer,
;; providing completion with all available switches. With no input
;; (i.e., leaving the minibuffer empty) the environment is reset to
;; the state before the first call of `opam-set-switch'.
;;
;; The menu is generated each time the minor mode is enabled and
;; contains the switches that are known at that time. If you create a
;; new switch, re-enable the minor mode to get it added to the menu.
;; The menu contains an additional entry "reset" to reset the
;; environment to the state when emacs was started.
;;
;; For obvious reasons, `opam-set-switch' does not change the switch
;; of any other shell.
;;
;; 

(require 'seq)

;;; User options and variables

(defgroup opam-mode ()
  "Customization for opam switch support in Emacs"
  :group 'external)

  
(defcustom opam-program-name "opam"
  "Name or path of the opam binary."
  :group 'opam-mode
  :type 'string)

(defcustom opam-common-options ()
  "Options to be supplied to every opam invocation.
This must be a list of strings, each member string an option
accepted by opam."
  :group 'opam-mode
  :type '(repeat string))

(defcustom opam-common-environment
  '("OPAMUTF8=never"
    "OPAMCOLOR=never"
    "LC_ALL=C")
  "Process environment to be set for every opam invocation.
List of strings compatible with `process-environment', i.e., each
element should have the form of ENVVARNAME=VALUE.

The process environment must ensure that output is plain ascii
without color, non-ascii arrow symbols and that it is in English.
Otherwise parsing the output of opam commands won't work."
  :group 'opam-mode
  :type '(repeat string))


;;; Code

(defun opam-run-command-without-stderr (sub-cmd
                                        &optional switch sexp
                                        &rest args)
  "Run opam SUB-CMD, without capturing error output.
Run opam SUB-CMD with additional arguments and insert the output
in the current buffer at point. Error output (stderr) is
discarded. If SWITCH is not nil, an option \"--swith=SWITCH\" is
added. If SEXP is t, option --sexep is added. All remaining
arguments ARGS are added as arguments.

Internally this function uses `process-file' internally and will
therfore respect file-name handlers specified via
`default-directory'."
  (let ((process-environment
         (append opam-common-environment process-environment))
        (options (append args opam-common-options)))
    (when switch
      (push (format "--switch=%s" switch) options))
    (when sexp
      (push "--sexp" options))
    ;; (message "run %s %s %s" opam-program-name sub-cmd options)
    (apply 'process-file opam-program-name
               nil '(t nil) nil sub-cmd options)))

(defun opam-command-as-string (sub-cmd &optional switch sexp &rest args)
  "Return output of opam SUB-CMD as string.
Same as `opam-run-command-without-stderr' but return all output
as string."
  (with-temp-buffer
    (apply 'opam-run-command-without-stderr sub-cmd switch sexp args)
    (buffer-string)))

(defun opam-get-root ()
  "Get the opam root directory.
This is the opam variable 'root'."
  (let ((root (opam-command-as-string "var" nil nil "root")))
    (when (eq (aref root (1- (length root))) ?\n)
      (setq root (substring root 0 -1)))
    root))

(defconst opam-root (opam-get-root)
  "The opam root directory.")

;; Example output of opam switch. The warning is output on stderr.
;;
;; OPAMUTF8=never OPAMCOLOR=never LC_ALL=C opam switch
;; #   switch        compiler                       description
;; ->  4112-coq-812  ocaml-variants.4.11.2+flambda  4112-coq-812
;;     44            ocaml-base-compiler.4.04.0
;;     450-coq-8.9   ocaml-base-compiler.4.05.0     450-coq-8.9
;;     471-no-coq    ocaml-base-compiler.4.07.1     471-no-coq
;;     system        ocaml-system.4.01.0
;;
;; #   switch   compiler      description
;; ->  default  ocaml.4.13.1  default
;;
;; [WARNING] The environment is not in sync with the current switch.
;;           You should run: eval $(opam env)

(defun opam-get-switches ()
  "Return all opam switches as list of strings."
  (let (opam-switches)
    (with-temp-buffer
      (opam-run-command-without-stderr "switch")
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "^.. *\\([^ ]*\\).*$" nil t)
        (push (match-string 1) opam-switches))
      opam-switches)))

(defvar opam-switch-history nil
  "Minibuffer history list for `opam-set-switch'.")

(defvar opam-saved-env nil
  "Saved environment variables, overwritten by an opam switch.
This is a list of saved environment variables. Each saved
variable is a list of two strings, the variable and the value.
Set when the first chosen opam switch overwrites the
environment.")

(defvar opam-saved-exec-path nil
  "Saved value of `exec-path'.
Set when the first chosen opam switch overwrites `exec-path'.")


(defun opam-save-current-env (opam-env)
  "Save the current environment values relevant to opam.
Argument OPAM-ENV, coming from calling `opam env', is only used
to find the environment variables to save. `exec-path' is saved
in addition to environment variables."
  (setq opam-saved-env
	(mapcar (lambda (x) (list (car x) (getenv (car x)))) opam-env))
  (setq opam-saved-exec-path exec-path))
  
(defun opam-set-env (opam-env)
  "Sets a new opam environment.
Environment variables in OPAM-ENV are put into the environment of
the current Emacs session. `exec-path' is changed to match the
environment PATH.

It is unclear which value in `exec-path' corresponds to a
previously set opam switch and also which entry in the PATH
environment variable in OPAM-ENV corresponds to the new switch.
Therefore this function uses the following heuristic. First all
entries in `exec-path' that match `opam-root' are deleted. Then,
the first entry for PATH that maches `opam-root' is added at the
front of `exec-path'."
  (let ((new-bin-dir
         (seq-find
          (lambda (dir) (string-prefix-p opam-root dir))
          (parse-colon-path (cadr (assoc "PATH" opam-env))))))
    (unless new-bin-dir
      (error "No opam-root directory in PATH"))
    (mapc (lambda (x) (setenv (car x) (cadr x))) opam-env)
    (setq exec-path
          (seq-remove (lambda (dir) (string-prefix-p opam-root dir)) exec-path))
    (push new-bin-dir exec-path)))
  
(defun opam-reset-env ()
  "Reset process environment to the state before setting the first opam switch.
Reset all environment variables and `exec-path' to the values
they had in this emacs session before the first chosen opam
switch overwrote them."
  (mapc (lambda (x) (setenv (car x) (cadr x))) opam-saved-env)
  (setq exec-path opam-saved-exec-path)
  (setq opam-saved-env nil)
  (setq opam-saved-exec-path nil))


(defun opam-set-switch (switch-name)
  "Chose and set an opam switch.
Set opam swith SWITCH-NAME, which must be a valid opam switch
name. When called interactively, the switch name must be entered
in the minibuffer, which forces completion to a valid switch name
or the empty string.

Setting the opam switch for the first time inside emacs will save
the current environment. Using the empty string for SWITCH-NAME
will reset the environment to the saved values.

The switch is set such that all process invocations from
emacs respect the newly set opam switch. In addition to setting
environment variables such as PATH and CAML_LD_LIBRARY_PATH, this
also sets `exec-path', which controls emacs'
subprocesses (`call-process', `make-process' and similar
functions).

For obvious resons, `opam-set-switch' will only affect emacs and
not any other shells outside emacs."
  (interactive
   (let* ((switches (opam-get-switches))
          (default (car switches))
          (current-switch (getenv "OPAM_SWITCH_PREFIX")))
     (if current-switch
         (setq current-switch (file-name-nondirectory current-switch))
       (setq current-switch "<none>"))
     (list
      (completing-read
       (format "current switch %s; switch to (empty to reset): " current-switch)
       switches nil t "" 'opam-switch-history nil))))
  (when (and (equal switch-name "") (not opam-saved-env))
    (error "No saved opam environment, cannot reset."))
  (if (equal switch-name "")
      (opam-reset-env)
    (let ((opam-env
           (car (read-from-string
                 (opam-command-as-string "env" switch-name t)))))
      (unless opam-saved-env
        (opam-save-current-env opam-env))
      (opam-set-env opam-env))))


;;; minor mode, keymap and menu

(defvar opam-mode-keymap (make-sparse-keymap)
  "Keymap for `opam-mode'")

(defun opam-menu-items ()
  "Create list or opam switches as menu items for `easy-menu'."
  (nconc
   ;; first the list with all the real opam switches
   (mapcar
    (lambda (switch)
      (vconcat
       `(,switch
         (opam-set-switch ,switch)
         :active t
         :help ,(concat "select opam switch \"" switch "\""))))
    (opam-get-switches))
   ;; now reset as last element
   '(
     ["reset" (opam-set-switch "")
      :active opam-saved-env
      :help "reset to state when emacs was started"]
     )))

(defun opam-setup-opam-mode ()
  "Re-define menu when for `opam-mode'.
This function runs when `opam-mode' is enabled to setup
`opam-mode'. Currently it only redefines the menu.

Note that the code for setting up the keymap and running the hook
is automatically created by `define-minor-mode'."
  (easy-menu-define
    opam-mode-menu
    opam-mode-keymap
    "opam mode menu"
    (cons "opam"
          (opam-menu-items))))

(define-minor-mode opam-mode
  "Toggle opam mode"
  ;; init value - should be nil
  nil
  ;; lighter
  " OP"
  ;; keymap
  opam-mode-keymap
  :group 'opam-mode
  ;; body
  (when opam-mode
    (opam-setup-opam-mode)))

(provide 'opam-mode)
