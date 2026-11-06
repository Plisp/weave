#!/bin/sh
WEAVE_FORMAT_ROOT=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
export WEAVE_FORMAT_ROOT
exec sbcl --noinform --disable-debugger --eval '(load *standard-input*)' --quit \
          --end-toplevel-options "$@" <<'LISP'
(require :asdf)
(asdf:load-asd (merge-pathnames "weave.asd"
                                (uiop:ensure-directory-pathname
                                  (uiop:getenv "WEAVE_FORMAT_ROOT"))))
(if (find-package :ql)
    (uiop:symbol-call :ql :quickload :weave :silent t)
    (asdf:load-system :weave))
(let* ((arguments (uiop:command-line-arguments))
       (check-only (equal arguments '("--check")))
       (files (directory (make-pathname :name :wild :type "lisp"
                          :defaults (asdf:system-relative-pathname :weave "src/")))))
  (unless (or (null arguments) check-only)
    (error "Usage: indent.sh [--check]"))
  (dolist (file files)
    (let* ((*readtable* (copy-readtable nil))
           (*package* (find-package :cl-user))
           (original (uiop:read-file-string file)))
      (weave-serialization:save-file-checked
       (weave-tui::parse-source original :define-packages t) file #'weave-tui::parse-source
       :original original :check-only check-only)))
  (format t "~a ~d files.~%" (if check-only "Checked" "Formatted") (length files)))
LISP
