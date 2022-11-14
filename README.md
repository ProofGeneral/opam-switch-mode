# opam-switch-mode

[![MELPA](https://melpa.org/packages/opam-switch-mode-badge.svg)](https://melpa.org/#/opam-switch-mode)

Provide command `opam-switch-set-switch` to change the opam switch of the
running emacs session and minor mode `opam-switch-mode` to select the opam
switch via a menu bar menu.

The menu is generated each time the minor mode is enabled and contains the
switches that are known at that time. If you create a new switch, re-enable
the minor mode to get it added to the menu. The menu contains an additional
entry "reset" to reset the environment to the state when emacs was started.

## Installing `opam-switch-mode`

The recommended way to install this mode relies on the
[MELPA](https://melpa.org/) repository of Emacs packages, along with the
[`use-package`](https://github.com/jwiegley/use-package) macro.
Assuming you have already set up those in your `.emacs`, just write:

```elisp
(use-package opam-switch-mode
  :ensure t
  :hook
  (coq-mode . opam-switch-mode))
```

so that the minor mode is automatically enabled when `coq-mode` is on,
see also [`opam-switch-mode` aware modes](#opam-switch-mode-aware-modes).

## Command `opam-switch-set-switch`

Invoke with `M-x opam-switch-set-switch RET`.

Choose and set an opam switch.

Set opam switch SWITCH-NAME, which must be a valid opam switch name. When
called interactively, the switch name must be entered in the minibuffer,
which forces completion to a valid switch name or the empty string.

Setting the opam switch for the first time inside emacs will save the
current environment. Using the empty string for SWITCH-NAME will reset the
environment to the saved values.

The switch is set such that all process invocations from emacs respect the
newly set opam switch. In addition to setting environment variables such as
PATH and CAML_LD_LIBRARY_PATH, this also sets `exec-path`, which controls
emacs' subprocesses (`call-process`, `make-process` and similar functions).

When the switch is changed, `opam-switch-change-opam-switch-hook` runs.
This can be used to inform other modes that may run background processes
that depend on the currently active opam switch.

For obvious reasons, `opam-switch-set-switch` will only affect emacs and not
any other shells outside emacs.

## `opam-switch-mode` aware modes

- `coq-mode` from [`proof-general`](https://proofgeneral.github.io/)
  can kill the coq background process, when the opam switch changes,
  see [`coq-kill-coq-on-opam-switch`](https://proofgeneral.github.io/doc/master/userman/Coq-Proof-General/#index-coq_002dkill_002dcoq_002don_002dopam_002dswitch).
