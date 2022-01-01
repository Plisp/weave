# ASG editor in Common Lisp

An editor that utilises a full semantic graph of the program as the sole internal
representation, eschewing text, which read/written when dealing with persistent
source files (more details to come in a blog post). In theory this allows support for:

* calls to existing libraries (requires C wrappers for calling from lisp)
* live redefinition and inspection of a running program
* semantically-aware extensions, loaded dynamically into lisp (no recompile like emacs!)
  * exploration of dynamic program analysis

At present only C support is implemented and the editor is mostly intended for writing
new programs, as the full C preprocessor and comments are not yet supported.
The main goals are to support the development of
[a roguelike game](https://github.com/plisp/lantern) and to explore alternative
projectional editing environments.
