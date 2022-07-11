;;; opam-switch-mode.el --- select opam switches within emacs  -*- lexical-binding: t; -*-
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
;; Provide command `opam-switch-set-switch' to change the opam switch
;; of the running emacs session and minor mode `opam-switch-mode' to
;; select the opam switch via a menu bar menu.
;;
;; `opam-switch-set-switch' reads the name of the switch in the
;; minibuffer, providing completion with all available switches. With
;; no input (i.e., leaving the minibuffer empty) the environment is
;; reset to the state before the first call of
;; `opam-switch-set-switch'.
;;
;; The menu is generated each time the minor mode is enabled and
;; contains the switches that are known at that time. If you create a
;; new switch, re-enable the minor mode to get it added to the menu.
;; The menu contains an additional entry "reset" to reset the
;; environment to the state when emacs was started.
;;
;; For obvious reasons, `opam-switch-set-switch' does not change the
;; switch of any other shell.
;;
;; 

(require 'seq)

;;; User options and variables

(defgroup opam-switch-mode ()
  "Customization for opam switch support in Emacs"
  :group 'external)

  
(defcustom opsw--program-name "opam"
  "Name or path of the opam binary."
  :group 'opam-switch-mode
  :type 'string)

(defcustom opsw--common-options ()
  "Options to be supplied to every opam invocation.
This must be a list of strings, each member string an option
accepted by opam."
  :group 'opam-switch-mode
  :type '(repeat string))

(defcustom opsw--common-environment
  '("OPAMUTF8=never"
    "OPAMCOLOR=never"
    "LC_ALL=C")
  "Process environment to be set for every opam invocation.
List of strings compatible with `process-environment', i.e., each
element should have the form of ENVVARNAME=VALUE.

The process environment must ensure that output is plain ascii
without color, non-ascii arrow symbols and that it is in English.
Otherwise parsing the output of opam commands won't work."
  :group 'opam-switch-mode
  :type '(repeat string))

(defcustom opam-switch-change-opam-switch-hook nil
  "Hook run when the opam switch changes.
This is used, for instance, to let Proof General kill the coq
background process when the opam switch changes."
  :group 'opam-switch-mode
  :type '(repeat function))
  

;;; Code

(defun opsw--run-command-without-stderr (sub-cmd
                                        &optional switch sexp
                                        &rest args)
  "Run opam SUB-CMD, without capturing error output.
Run opam SUB-CMD with additional arguments and insert the output
in the current buffer at point. Error output (stderr) is
discarded. If SWITCH is not nil, an option \"--swith=SWITCH\" is
added. If SEXP is t, option --sexep is added. All remaining
arguments ARGS are added as arguments.

Return exit status of the opam invocation.

Internally this function uses `process-file' internally and will
therfore respect file-name handlers specified via
`default-directory'."
  (let ((process-environment
         (append opsw--common-environment process-environment))
        (options (append args opsw--common-options)))
    (when switch
      (push (format "--switch=%s" switch) options))
    (when sexp
      (push "--sexp" options))
    ;; (message "run %s %s %s" opsw--program-name sub-cmd options)
    (apply 'process-file opsw--program-name
               nil '(t nil) nil sub-cmd options)))

(defun opsw--command-as-string (sub-cmd &optional switch sexp &rest args)
  "Return output of opam SUB-CMD as string or nil.
Same as `opsw--run-command-without-stderr' but return all output
as string. Return nil if opam command fails."
  (with-temp-buffer
    (let ((status
           (apply 'opsw--run-command-without-stderr sub-cmd switch sexp args)))
      (if (eq status 0)
          (buffer-string)
        nil))))

(defun opsw--get-root ()
  "Get the opam root directory.
This is the opam variable 'root'."
  (let ((root (opsw--command-as-string "var" nil nil "root")))
    (unless root
      (error "opam var root failed"))
    (when (eq (aref root (1- (length root))) ?\n)
      (setq root (substring root 0 -1)))
    root))

(defconst opsw--root (opsw--get-root)
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

(defun opsw--get-switches ()
  "Return all opam switches as list of strings."
  (let (opam-switches)
    (with-temp-buffer
      (unless (eq (opsw--run-command-without-stderr "switch") 0)
        ;; opam exit status different from 0 -- some error occured
        (error "opam switch failed"))
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "^.. *\\([^ ]*\\).*$" nil t)
        (push (match-string 1) opam-switches))
      opam-switches)))

(defvar opsw--switch-history nil
  "Minibuffer history list for `opsw--set-switch'.")

(defvar opsw--saved-env nil
  "Saved environment variables, overwritten by an opam switch.
This is a list of saved environment variables. Each saved
variable is a list of two strings, the variable and the value.
Set when the first chosen opam switch overwrites the
environment.")

(defvar opsw--saved-exec-path nil
  "Saved value of `exec-path'.
Set when the first chosen opam switch overwrites `exec-path'.")


(defun opsw--save-current-env (opam-env)
  "Save the current environment values relevant to opam.
Argument OPAM-ENV, coming from calling `opam env', is only used
to find the environment variables to save. `exec-path' is saved
in addition to environment variables."
  (setq opsw--saved-env
	(mapcar (lambda (x) (list (car x) (getenv (car x)))) opam-env))
  (setq opsw--saved-exec-path exec-path))
  
(defun opsw--set-env (opam-env)
  "Sets a new opam environment.
Environment variables in OPAM-ENV are put into the environment of
the current Emacs session. `exec-path' is changed to match the
environment PATH.

It is unclear which value in `exec-path' corresponds to a
previously set opam switch and also which entry in the PATH
environment variable in OPAM-ENV corresponds to the new switch.
Therefore this function uses the following heuristic. First all
entries in `exec-path' that match `opsw--root' are deleted. Then,
the first entry for PATH that maches `opsw--root' is added at the
front of `exec-path'."
  (let ((new-bin-dir
         (seq-find
          (lambda (dir) (string-prefix-p opsw--root dir))
          (parse-colon-path (cadr (assoc "PATH" opam-env))))))
    (unless new-bin-dir
      (error "No opam-root directory in PATH"))
    (mapc (lambda (x) (setenv (car x) (cadr x))) opam-env)
    (setq exec-path
          (seq-remove (lambda (dir) (string-prefix-p opsw--root dir)) exec-path))
    (push new-bin-dir exec-path)))
  
(defun opsw--reset-env ()
  "Reset process environment to the state before setting the first opam switch.
Reset all environment variables and `exec-path' to the values
they had in this emacs session before the first chosen opam
switch overwrote them."
  (mapc (lambda (x) (setenv (car x) (cadr x))) opsw--saved-env)
  (setq exec-path opsw--saved-exec-path)
  (setq opsw--saved-env nil)
  (setq opsw--saved-exec-path nil))


(defun opsw--get-current-switch ()
  "Return name of current switch or \"<none>\"."
  (let ((current-switch (getenv "OPAM_SWITCH_PREFIX")))
    (if current-switch
         (file-name-nondirectory current-switch)
      "<none>")))

(defun opsw--set-switch (switch-name)
  "Chose and set an opam switch.
Set opam switch SWITCH-NAME, which must be a valid opam switch
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

When the switch is changed, `opam-switch-change-opam-switch-hook'
runs. This a can be used to inform other modes that may run
background processes that depend on the currently active opam
switch.

For obvious resons, `opsw--set-switch' will only affect emacs and
not any other shells outside emacs."
  (interactive
   (let* ((switches (opsw--get-switches))
          (current-switch (opsw--get-current-switch)))
     (list
      (completing-read
       (format "current switch %s; switch to (empty to reset): " current-switch)
       switches nil t "" 'opsw--switch-history nil))))
  (when (and (equal switch-name "") (not opsw--saved-env))
    (error "No saved opam environment, cannot reset."))
  (if (equal switch-name "")
      (opsw--reset-env)
    (let ((output-string (opsw--command-as-string "env" switch-name t))
          opam-env)
      (unless output-string
        (error
         "opam env %s failed - probably because of invalid opam switch \"%s\""
         switch-name switch-name))
      (setq opam-env (car (read-from-string output-string)))
      (unless opsw--saved-env
        (opsw--save-current-env opam-env))
      (opsw--set-env opam-env)))
  (run-hooks 'opam-switch-change-opam-switch-hook))

(defalias 'opam-switch-set-switch #'opsw--set-switch)

;;; minor mode, keymap and menu

(defvar opsw--mode-keymap (make-sparse-keymap)
  "Keymap for `opam-switch-mode'")

(defun opsw--menu-items ()
  "Create list of opam switches as menu items for `easy-menu'."
  (append
   ;; first the current switch as info with a separator
   '(["current switch: " nil
      :active t
      :suffix (opsw--get-current-switch)
      :help "Shows the currently selected opam switch"]
     "-------")
   ;; then the list with all the real opam switches
   (mapcar
    (lambda (switch)
      (vconcat
       `(,switch
         (opsw--set-switch ,switch)
         :active t
         :help ,(concat "select opam switch \"" switch "\""))))
    (opsw--get-switches))
   ;; now reset as last element
   '(
     ["reset" (opsw--set-switch "")
      :active opsw--saved-env
      :help "reset to state when emacs was started"]
     )))

(defun opsw--setup-opam-switch-mode ()
  "Re-define menu for `opam-switch-mode'.
This function runs when `opam-switch-mode' is enabled to setup
`opam-switch-mode'. Currently it only redefines the menu.

Note that the code for setting up the keymap and running the hook
is automatically created by `define-minor-mode'."
  (easy-menu-define
    opsw--mode-menu
    opsw--mode-keymap
    "opam mode menu"
    (cons "opam-switch"
          (opsw--menu-items))))

(define-minor-mode opam-switch-mode
  "Toggle opam mode"
  ;; init value - should be nil
  nil
  ;; lighter
  " OPSW"
  ;; keymap
  opsw--mode-keymap
  :group 'opam-switch-mode
  ;; body
  (when opam-switch-mode
    (opsw--setup-opam-switch-mode)))

(provide 'opam-switch-mode)
