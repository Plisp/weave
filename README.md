# WIP lisp structural editor

`ast.lisp` is an almost complete parser for common lisp.
`tui.lisp` is a terminal UI using [uncursed](https://github.com/plisp/uncursed).

Currently mainly testing on sbcl due to non-standard macroexpansions.
The point of structural editing is proven ergonomics (e.g. paredit, vim) adapted to a fully tree-structured context and to have incremental accurate type analysis and tracking of image state. I currently have implemented a generalization of zipper selection as described in [pantograph](https://arxiv.org/pdf/2411.16571).

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
