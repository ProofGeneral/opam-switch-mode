# opam mode

Provide command `opam-set-switch` to change the opam switch of the
running emacs session. The command reads the name of the switch in
the minibuffer, providing completion with all available switches.
With no input (i.e., leaving the minibuffer empty) the environment
is reset to the state before the first call of `opam-set-switch`.

For obvious reasons, `opam-set-switch` does not change the switch
of any other shell.

Contrary to the file name, this file does not (yet) provide a minor
mode.
