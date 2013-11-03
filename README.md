# free-keys

Show free bindings in current buffer. Based on https://gist.github.com/bjorne/3796607

To use, call the command `free-keys`. This package takes into account the major mode bindigns as well as any bindings occupied by minor modes active in current buffer. If called with prefix argument `C-u`, you can specify a prefix map to be used, such as `C-c` or `C-c C-x` (these are specified as a string).

You can customize the variable `free-keys-modifiers` if you use non-standard modifiers, such as `H` for hyper, `s` for super or `S` for shift. By default this list contains `C`, `M`, `C-M` and no modifier.

You can customize the variable `free-keys-keys` if you use non-english keyboard layout and want to show free bindings for keys such as č, í, ö, è, å etc.

These bindings are available inside the `*Free keys*` buffer:

Keybinding | Description
-----------|---------------
`b`        | Change the "active" buffer
`p`        | Change the prefix
`q`        | Quit


# Installation

The easiest way is to install this via `package.el` from MELPA repository. If you want to install manually, clone the git repo and add it to your `load-path`:

    (add-to-list 'load-path "path-to-this-git-repo")
