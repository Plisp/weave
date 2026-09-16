#!/bin/sh
exec sbcl --noinform --non-interactive \
     --eval '(load (merge-pathnames ".quicklisp/setup.lisp" (user-homedir-pathname)))' \
     --eval '(ql:quickload :weave :silent t)' \
     --eval '(sb-ext:save-lisp-and-die "weave" :executable t
               :toplevel (lambda () (weave-tui::main) (sb-ext:exit)))'
