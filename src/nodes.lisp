(defpackage #:kira
  (:use :cl :alexandria)
  (:import-from #:raw-bindings-sdl2 #:x #:y #:h #:w)
  (:local-nicknames (#:cltl2 #:cl-environments)
                    (#:fonts #:org.shirakumo.font-discovery)
                    (#:sdl #:raw-bindings-sdl2)
                    (#:sdl-ttf #:raw-bindings-sdl2-ttf))
  (:export #:main))
(in-package #:kira)

(defparameter *cursor* nil)

(defclass node ()
  ((name :initarg :name
         :accessor name
         :type string) ; TODO SHOULD BE A ROPE
   (fg :initform '(147 161 161)
       ;;:type fixnum
       :initarg :fg
       :accessor fg
       )
   (bg :initform '(108 113 196)
       ;;:type fixnum
       :initarg :bg
       :accessor bg
       )))
