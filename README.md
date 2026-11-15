# Weave WIP - a fluid structural editor

The goal of this project is to raise the bar for interactive lisp development using automated refactoring and deterministic analyses built on an informative semantic model.

* `ast.lisp` is an almost complete parser for CL.
* `indentation.lisp` contains formatting rules for both frontend and serializer, check indent.sh
* `tui.lisp` is a terminal UI using [uncursed](https://github.com/plisp/uncursed)
* `commands.lisp` implements most editing commands
* `entry-point.lisp` invokes the tui, check build.sh

The point of structural editing is proven ergonomics (e.g. paredit, vim) adapted to a fully tree-structured context and moreover the prospect of provenance-aware tooling - utilizing a richer command calculus to avoid the ambiguities of intent inherent in text-based editing.

* An interesting technique is used to lexically analyse macro calls without manual specification, I'm working on a writeup for ELS next year.
* Currently implemented is a generalization of zipper selection as described in [pantograph](https://arxiv.org/pdf/2411.16571), see `Selection.md`.
* Recursive stack debugging is supported without threads, similar to emacs recursive editing (value-accepting restart support and commuting successful resumed commands is WIP).

Ask me on the lisp discord if you have any questions :D

## Running
Currently only testing on sbcl due to non-standard macroexpansions and performance.
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
* `Tab/Backtab` scroll completions
* `Enter` expands the symbol/completion at point into a call
* `Ctrl-n/p/b/f` (and arrow keys) move by atom and goal column
* `Alt-n/p` move by parent stack
* `Alt-P` move by parent until reaching a non-evaluated position. Tip: follow with ctrl+x to wrap
* `Alt-op` for op in `+,-,*,/` wraps the current node in arithmetic

* `Shift-left/right` starts a selection
* `Shift-up/down` adjusts a zipper selection
* `Ctrl-x/c/v` cut, copy and paste

* `Ctrl-w` close buffer
* `Alt-e` evaluation
* `Ctrl-Alt-s` save file

## LLM disclosure
LLM assistance has been used to investigate edge cases and generate corresponding regression tests. These checks were helpful as I'd never attempted a project of this scale. It produced a useful shim surveying correctness of the early parser and usage patterns of certain ill-behaved (reader) macros across locally downloaded repositories and weave's own source, and the `indent.sh` script. I also experimented with generation including mechanical but error-prone translation tasks, particularly standard parsing of reader syntax using eclector, porting SLIME's indentation rules and prototyping some drawing code (almost every line being read or rewritten as I altered underspecified behavior).

However most of weave's source (in `src`) is quite semantically dense and encapsulates many cross-cutting design decisions requiring creative thought. Going forward, I've opted to handwrite changes to `src/` to ensure code quality and better understand the tooling deficiencies that induce a need for LLMs.
