
(require 'seq)

(defcustom opam-program-name "opam"
  "XXX")

(defcustom opam-common-options ()
  "XXX")

(defcustom opam-common-environment
  '("OPAMUTF8=never"
    "OPAMCOLOR=never"
    "LC_ALL=C")
  "XXX")

(defun opam-run-command-without-stderr (sub-cmd
                                        &optional switch sexp
                                        &rest args)
  "XXX"
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
  "XXX"
  (with-temp-buffer
    (apply 'opam-run-command-without-stderr sub-cmd switch sexp args)
    (buffer-string)))

(defun opam-run-command-with-stderr (err-buf sub-cmd &optional switch sexp &rest args)
  "XXX"
  (let ((process-environment
         (append opam-common-environment process-environment))
        (options (append args opam-common-options))
        (temp-file (make-nearby-temp-file "emacs-opam-")))
    (when switch
      (push (format "--switch=%s" switch) options))
    (when sexp
      (push "--sexp" options))
    (apply 'process-file opam-program-name
                    nil (list t temp-file) nil sub-cmd options)
    (with-current-buffer err-buf
      (insert-file-contents temp-file))
    (ignore-errors (delete-file temp-file))))

;; (defun test (sub-cmd switch sexp)
;;   (let (err-buf stdout-string)
;;     (with-temp-buffer
;;       (setq err-buf (current-buffer))
;;       (with-temp-buffer
;;         (opam-run-command-with-stderr err-buf sub-cmd switch sexp)
;;         (setq stdout-string (buffer-string)))
;;       (cons stdout-string (buffer-string)))))



(defun opam-get-root ()
  (let ((root (opam-command-as-string "var" nil nil "root")))
    (when (eq (aref root (1- (length root))) ?\n)
      (setq root (substring root 0 -1)))
    root))

(defconst opam-root (opam-get-root)
  "XXX")

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
  ""
  (let (opam-switches)
    (with-temp-buffer
      (opam-run-command-without-stderr "switch")
      (goto-char (point-min))
      (forward-line)
      (while (re-search-forward "^\\(..\\) *\\([^ ]*\\).*$" nil t)
        (push (match-string 2) opam-switches)
        (when (equal (match-string 1) "->")
          (setq default-switch (match-string 2))))
      opam-switches)))

(defvar opam-switch-history nil
  "XXX")

(defvar opam-saved-env nil
  "XXX")

(defvar opam-saved-exec-path nil
  "XXX")


(defun opam-save-current-env (opam-env)
  "XXX"
  (setq opam-saved-env
	(mapcar (lambda (x) (list (car x) (getenv (car x)))) opam-env))
  (setq opam-saved-exec-path exec-path))
  

(defun opam-set-env (opam-env)
  "XXX"
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
  

(defun opam-set-switch (switch-name)
  "XXX"
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

(provide 'opam-mode)
