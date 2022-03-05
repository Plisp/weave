(defpackage #:weave.runtime
  (:use :cl :alexandria))
(in-package #:weave.runtime)

(defgeneric value (node)
  (:method (node)
    (values nil nil)))

(defmacro defs (name parents slots)
  `(progn
     (defclass ,name (,@parents)
       ,(mapcar (lambda (name)
                  `(,name :initarg ,(make-keyword name)
                          :accessor ,name))
         slots))
     ,@(mapcar (lambda (name) `(export ',name))
               slots)
     (export ',name)))

(defs node ()
  (value))

(defs expr (node)
  ())

(defs number-lit (node) ())

(defs var (node) ())
(defs val (node) ())

;; control flow

(defs if-expr (node)
  ())

(defs assign-expr (node)
  ())

(defs fn-expr (node)
  ())

(defun make-children (&rest objects)
  (let ((v (make-array 1 :fill-pointer t :adjustable t)))
    (loop for object in objects
          do (vector-push-extend object v)
          finally (return v))))
