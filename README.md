# ASG editor in Common Lisp

An editor that utilises a
[full semantic graph](https://plisp.github.io/posts/Editing-abstraction-not-text-updated.html)
of the program as the sole internal
representation, eschewing text, which read/written when dealing with persistent
source files. In theory this allows support for:

* calls to existing C libraries
* live redefinition and inspection of a running program
* C code generation with lisp functions
* semantically-aware extensions, loaded dynamically into lisp (no recompile, like emacs!)
  * exploration of dynamic program analysis

At present only C support is implemented and the editor is mostly intended for writing
new programs, as the full C preprocessor, portable serialization (+ formatting)
and comments are not supported. The main goals are to support the development of
[a roguelike game](https://github.com/plisp/lantern) and to explore alternative
projectional editing environments.
