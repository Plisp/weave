# An integrated editor and lisp? in Common Lisp

An editor and language runtime that utilises a
[full semantic graph](https://plisp.github.io/posts/Editing-abstraction-not-text-updated.html)
of the program as the sole internal representation, 'projecting' to text for persistence
when necessary. In theory this allows support for:

* live redefinition and inspection of running programs
* typed macros with access to full compiler analysis
* calls to existing C libraries
