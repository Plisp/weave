(defpackage #:weave
  (:use :cl :alexandria)
  (:import-from #:raw-bindings-sdl2 #:x #:y #:h #:w)
  (:local-nicknames (#:fonts #:org.shirakumo.font-discovery)
                    (#:sdl #:raw-bindings-sdl2)
                    (#:sdl-ttf #:raw-bindings-sdl2-ttf))
  (:export #:main))
(in-package #:weave)

(defclass node ()
  ((value :initarg :value
          :accessor value))
  (:documentation "represents a node in the AST"))

(defclass string-literal (node)
  ((value :initarg value
          :accessor value
          :type string)))

(defclass number-literal (node)
  ((value :initarg value
          :accessor value
          :type number)))

(defclass typename (node)
  ((value :initarg value
          :accessor value
          :type symbol)))

(defgeneric value (node)
  (:method (node)
    (values nil nil)))

(defun make-children (&rest objects)
  (let ((v (make-array 1 :fill-pointer t :adjustable t)))
    (loop for object in objects
          do (vector-push-extend object v)
          finally (return v))))
