# opam mode

Provide command `opam-set-switch` to change the opam switch of the
running emacs session. The command reads the name of the switch in
the minibuffer, providing completion with all available switches,
see the documentation of `opam-set-switch` below.

Contrary to the file name, this file does not (yet) provide a minor
mode.

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
