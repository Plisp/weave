# WIP lisp structural editor

* `ast.lisp` is an almost complete parser for common lisp.
* `indentation.lisp` contains formatting rules for both frontend and serializer
* `tui.lisp` is a terminal UI using [uncursed](https://github.com/plisp/uncursed)
* `commands.lisp` implements basic editing commands
* `selection.lisp` implements selection
* `entry-point.lisp` invokes the tui, check build.sh

Currently only testing on sbcl due to non-standard macroexpansions.
The point of structural editing is proven ergonomics (e.g. paredit, vim) adapted to a fully tree-structured context and to have incremental semantic analysis and tracking of image state.

* An interesting technique is used to lexically analyse macro calls, I'm working on a writeup for ELS next year.
* Currently implemented is a generalization of zipper selection as described in [pantograph](https://arxiv.org/pdf/2411.16571).
* Recursive stack debugging is supported without threads, similar to emacs (value restart support and commuting resumed commands is WIP)

Ask me on the lisp discord if you have any questions :D

## Running
In a terminal load
```lisp
(ql:quickload :slynk) ; for sly users, this is slynk
(slynk:create-server :dont-close t :port 4005)
(loop (sleep 1))
```

In emacs, sly-connect to 4005 and
```lisp
(ql:quickload :weave)
(weave-tui:main)
```

### Keybindings:
* `Space` inserts or moves to the next hole
* `Tab` triggers completion, use ctrl-n/p to select
* `Enter` expands the symbol/completion at point into a call, useful for locals
* `Ctrl-n/p/b/f` (and arrow keys) move by atom and goal column
* `Alt-n/p` move by parent stack
* `Alt-P` move by parent until reaching a non-evaluated position
* `Alt-op` for op in `+,-,*,/` wraps the current node in arithmetic

* `Shift-left/right` starts a selection
* `Shift-up/down` adjusts a zipper selection
* `Ctrl-x/c/v` cut, copy and paste

* `Ctrl-w` close buffer
* `Alt-e` evaluation
* `Ctrl-Alt-s` save file
