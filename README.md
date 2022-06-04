# opam mode

Provide command `opam-set-switch` to change the opam switch of the
running emacs session and minor mode `opam-mode` to select the opam
switch via a menu bar menu.

The menu is generated each time the minor mode is enabled and
contains the switches that are known at that time. If you create a
new switch, re-enable the minor mode to get it added to the menu.
The menu contains an additional entry "reset" to reset the
environment to the state when emacs was started.


## Command `opam-set-switch`

Invoke with `M-x opam-set-switch`.

Chose and set an opam switch.

Set opam swith SWITCH-NAME, which must be a valid opam switch
name. When called interactively, the switch name must be entered
in the minibuffer, which forces completion to a valid switch name
or the empty string.

Setting the opam switch for the first time inside emacs will save
the current environment. Using the empty string for SWITCH-NAME
will reset the environment to the saved values.

The switch is set such that all process invocations from emacs
respect the newly set opam switch. In addition to setting
environment variables such as PATH and CAML_LD_LIBRARY_PATH, this
also sets `exec-path`, which controls emacs' subprocesses
(`call-process`, `make-process` and similar functions).

For obvious resons, `opam-set-switch` will only affect emacs and
not any other shells outside emacs.
