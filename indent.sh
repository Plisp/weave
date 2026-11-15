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
       (check-only (member "--check" arguments :test #'string=))
       (directories (remove "--check" arguments :test #'string=)))
  (unless (and (<= (length directories) 1)
               (<= (count "--check" arguments :test #'string=) 1)
               (notany (lambda (argument) (uiop:string-prefix-p "--" argument))
                       directories))
    (error "Usage: indent.sh [--check] [relative-directory]"))
  (let* ((requested (first directories))
         (directory (if requested
                        (progn
                          (when (uiop:absolute-pathname-p requested)
                            (error "Expected a relative directory: ~a" requested))
                          (merge-pathnames (uiop:ensure-directory-pathname requested)
                                           (uiop:getcwd)))
                        (asdf:system-relative-pathname :weave "src/")))
         (files (progn
                  (unless (uiop:directory-exists-p directory)
                    (error "Directory does not exist: ~a" directory))
                  (directory (make-pathname :name :wild :type "lisp"
                                            :defaults directory)))))
    (dolist (file files)
      (let* ((*readtable* (copy-readtable nil))
             (*package* (find-package :cl-user))
             (original (uiop:read-file-string file)))
        (weave-serialization:save-file-checked
         (weave-tui::parse-source original :define-packages t) file #'weave-tui::parse-source
         :original original :check-only check-only)))
    (format t "~a ~d files.~%" (if check-only "Checked" "Formatted") (length files))))
LISP
