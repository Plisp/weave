;;;;
;;;; terminal frontend
;;;;

(defpackage #:weave-tui
  (:use :cl #:alexandria-2 #:weave-utils)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed))
  (:export))
(in-package #:weave-tui)

;; XXX dynamic vars for debugging, should not be used
(defvar *log*)
(defvar *tui*)
(defun slog (o)
  #+(and sbcl slynk) (sb-concurrency:send-message *log* o)
  o)

;;
;;; ui classes
;;
(defclass ui (tui:tui)
  ())

(defun tui-handle-event (tui ev)
  #+sbcl (slog ev)
  (cond ((equal ev '(#\q :control))
         (tui:stop tui))))

(defclass ast-view (tui:standard-window)
  ((ast :initarg :ast
        :accessor ast)))

(defgeneric display-node (node pos-x pos-y))

;; TODO macro
(defmethod display-node ((node parse:when-let*-form) pos-x pos-y)
  (let ((prefix "when-let ")
        (*print-case* :downcase))
    (if (parse:vars node)
        (with-slots ((vars parse:vars)) node
          (tui:puts prefix pos-x pos-y)
          (loop for (var init) in vars
                do (tui:puts (format nil "~a ~a" var init) pos-y (1+ (length prefix)))
                   (incf pos-y)))
        (tui:puts (format nil "~a~a ~a" prefix (parse:name node) (parse:init node))
                  pos-x pos-y))
    (incf pos-x 2)
    (loop for form in (parse:body node)
          do (tui:puts (format nil "~a" form) pos-y pos-x)
             (incf pos-y))))

(defmethod tui:handle-key-event ((window ast-view) tui event)
  nil)

(defmethod tui:present ((w ast-view))
  (display-node (ast w) 1 1))

;;
;;; main loop
;;
(defun tui-main ()
  (let* ((dimensions (tui:terminal-dimensions))
         (ast (parse:parse '(when-let* ((a (parse::*literal-magic* 3))
                                        (b (1+ a)))
                             (1- b)
                             a)
                           (parse:make-env)))
         (ast-view
           (make-instance 'ast-view
                          :dimensions (tui:make-rect :x 0 :y 0
                                                     :rows (car dimensions)
                                                     :cols (cdr dimensions))
                          :ast ast))
         (tui
           (make-instance 'ui :focused-window ast-view
                              :windows (list ast-view)
                              :event-handler #'tui-handle-event)))
    (setf *tui* tui)
    (tui:run tui :redisplay-on-input t)
    #+sbcl (sb-concurrency:send-message *log* :stop)))

(defun main ()
  (if (member :slynk *features*)
      (progn
        (bt:make-thread (lambda () (tui-main)))
        #+sbcl
        (loop :initially (setf *log* (sb-concurrency:make-mailbox :name "log"))
              :for m = (sb-concurrency:receive-message *log*)
              :until (eq m :stop)
              :do (print m) (force-output)))
      (tui-main)))
