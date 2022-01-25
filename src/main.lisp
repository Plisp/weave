(in-package #:weave)

;;;
;;; globals
;;;

;; editor state
(defvar *running* nil)
(defparameter *tree*
  (make-instance 'node :value "test"))

(defparameter *cursor* (list)
  "A stack of nodes and editing context for the current node, for functional updates.")

;; config
(defparameter *font* '(:family "InputSans" :weight 90))

;; SDL state
(defvar *sdl-window* nil)
(defvar *sdl-renderer* nil)
(defvar *sdl-ttf-font* nil)

;;;
;;; main loop
;;;

(defun %handle-event (event)
  "TODO handle pending events"
  (restart-case
      (case (cffi:foreign-slot-value event 'sdl:sdl-event 'sdl:type)
        (#.sdl:+sdl-quit+
         (setf *running* nil))

        (#.sdl:+sdl-mousemotion+
         (format t "ouse event ~d ~d ~%"
                 (cffi:foreign-slot-value event 'sdl:sdl-mouse-motion-event 'x)
                 (cffi:foreign-slot-value event 'sdl:sdl-mouse-motion-event 'y)))

        (#.sdl:+sdl-keydown+
         (print (cffi:foreign-slot-value
                 (cffi:foreign-slot-value event 'sdl:sdl-keyboard-event 'sdl:keysym)
                 'sdl:sdl-keysym
                 'sdl:sym)))
        )
    (never-gonna-give-you-up ()
      (return-from %handle-event))))

(defun sdlify-color (color)
  (list 'sdl:r (first color)
        'sdl:g (second color)
        'sdl:b (third color)))

(defun sdl-draw (text x y &key color)
  "TODO handle wrapping/clipping"
  (trivia:match (theme-lookup nil)
    ((vector fg bg)
     (apply #'sdl:sdl-set-render-draw-color *sdl-renderer*
            `(,@(or (bg color) bg) 0))
     (sdl:sdl-render-clear *sdl-renderer*)
     (let* ((surface
              (sdl-ttf:ttf-render-utf8-blended *sdl-ttf-font* text
                                               (sdlify-color (or (fg color) fg))))
            (texture
              (sdl:sdl-create-texture-from-surface *sdl-renderer* surface)))
       ;;
       (flet ((surface-w (surface)
                (cffi:foreign-slot-value surface 'sdl:sdl-surface 'sdl:w))
              (surface-h (surface)
                (cffi:foreign-slot-value surface 'sdl:sdl-surface 'sdl:h)))
         (cffi:with-foreign-object (rect 'sdl:sdl-rect)
           (let ((pos-x x)
                 (pos-y y))
             (cffi:with-foreign-slots ((x y w h) rect sdl:sdl-rect)
               (setf x pos-x
                     y pos-y
                     w (surface-w surface)
                     h (surface-h surface))
               (sdl:sdl-render-copy *sdl-renderer* texture
                                    (cffi:null-pointer)
                                    rect)))))
       (sdl:sdl-free-surface surface)
       (sdl:sdl-destroy-texture texture))))
  (sdl:sdl-render-present *sdl-renderer*))

(defun main ()
  (let ((width 800)
        (height 600)
        *sdl-window* *sdl-renderer* *sdl-ttf-font*)
    (unwind-protect
         (progn
           (or (zerop (sdl:sdl-init sdl:+sdl-init-video+))
               (format t "SDL failed to initialize: ~a~%" (sdl:sdl-get-error)))

           (sdl-ttf:ttf-init)
           (setf *sdl-ttf-font* (sdl-ttf:ttf-open-font
                                 (namestring (fonts:file (apply #'fonts:find-font *font*)))
                                 16))
           (when (cffi:null-pointer-p *sdl-ttf-font*)
             (format t "SDL *sdl-ttf-font* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))

           (setf *sdl-window* (sdl:sdl-create-window "main *sdl-window*"
                                                     sdl:+sdl-windowpos-undefined+
                                                     sdl:+sdl-windowpos-undefined+
                                                     width height 0))
           (when (cffi:null-pointer-p *sdl-window*)
             (format t "SDL *sdl-window* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (format t "initialized SDL window~%")

           (setf *sdl-renderer* (sdl:sdl-create-renderer *sdl-window* -1
                                                         sdl:+sdl-renderer-accelerated+))
           (when (cffi:null-pointer-p *sdl-renderer*)
             (format t "SDL *sdl-renderer* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (format t "initialized renderer~%")

           (cffi:with-foreign-object (event 'sdl:sdl-event)
             (loop :initially (setf *running* t)
                   :while *running*
                   :do (sdl:sdl-wait-event event) ; 10ms poll
                       (%handle-event event)
                       (draw-node #'sdl-draw *tree* 0 0))))
      ;; unwind
      (format t "stopped~%")
      (setf *running* nil)
      (when *sdl-window*
        (sdl:sdl-destroy-window *sdl-window*)
        (setf *sdl-window* nil))
      (when *sdl-renderer*
        (sdl:sdl-destroy-renderer *sdl-renderer*)
        (setf *sdl-renderer* nil))
      (when *sdl-ttf-font*
        (sdl-ttf:ttf-close-font *sdl-ttf-font*)
        (setf *sdl-ttf-font* nil))
      (sdl-ttf:ttf-quit)
      (sdl:sdl-quit))))
