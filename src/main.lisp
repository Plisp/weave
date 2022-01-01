(in-package #:weave)

;; editor state
(defvar *running* nil)
(defparameter *text*
  "#include \"test.h\"
//  second very very very loing bit of text

int main()
{
    return 0;
}")

;; config
(defparameter *font* '(:family "InputSans" :weight 90))
(defparameter *empty-line-spacing* 8)
(defparameter *offset* 0)

;; SDL state
(defvar *window* nil)
(defvar *renderer* nil)
(defvar *ttf-font* nil)

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

        (#.sdl:+sdl-mousewheel+
         (case (print (cffi:foreign-slot-value event 'sdl:sdl-mouse-wheel-event 'y))
           (1 (incf *offset* 3))
           (-1 (setf *offset* (max 0 (- *offset* 3))))))

        (#.sdl:+sdl-keydown+
         ())
        )
    (never-gonna-give-you-up ()
      (return-from %handle-event))))

(defun %render ()
  (sdl:sdl-set-render-draw-color *renderer* 0 43 54 0)
  (sdl:sdl-render-clear *renderer*)
  (with-input-from-string (s *text*)
    (loop :with last-y = 0
          :for line = (read-line s nil nil)
          :while line
          :for first-line = t :then nil
          :do (if (= (length line) 0)
                  (incf last-y *empty-line-spacing*)
                  (let* ((surface (sdl-ttf:ttf-render-utf8-blended
                                   *ttf-font* line '(sdl:r 147 sdl:g 161 sdl:b 161)))
                         (texture (sdl:sdl-create-texture-from-surface *renderer* surface)))

                    (flet ((surface-w (surface)
                             (cffi:foreign-slot-value surface 'sdl:sdl-surface 'sdl:w))
                           (surface-h (surface)
                             (cffi:foreign-slot-value surface 'sdl:sdl-surface 'sdl:h)))

                      (cffi:with-foreign-object (rect 'sdl:sdl-rect)
                        (cffi:with-foreign-slots ((x y w h) rect sdl:sdl-rect)
                          (setf x 0
                                y last-y
                                w (surface-w surface)
                                h (surface-h surface))
                          ;; first line may be clipped
                          (if (not first-line)
                              (sdl:sdl-render-copy *renderer* texture
                                                   (cffi:null-pointer)
                                                   rect)
                              (cffi:with-foreign-object (clip 'sdl:sdl-rect)
                                (cffi:with-foreign-slots ((x y w h) clip sdl:sdl-rect)
                                  (setf x 0
                                        y *offset*
                                        w (surface-w surface)
                                        h (- (surface-h surface) *offset*)))
                                (decf h *offset*)
                                (sdl:sdl-render-copy *renderer* texture clip rect)))
                          (incf last-y h))))
                    (sdl:sdl-free-surface surface)
                    (sdl:sdl-destroy-texture texture)))))
  (sdl:sdl-render-present *renderer*))

(defun main ()
  (let ((width 800)
        (height 600)
        *window* *renderer* *ttf-font*)
    (unwind-protect
         (progn
           (or (zerop (sdl:sdl-init sdl:+sdl-init-video+))
               (format t "SDL failed to initialize: ~a~%" (sdl:sdl-get-error)))

           (sdl-ttf:ttf-init)
           (setf *ttf-font* (sdl-ttf:ttf-open-font
                             (namestring (fonts:file (apply #'fonts:find-font *font*)))
                             16))
           (when (cffi:null-pointer-p *ttf-font*)
             (format t "SDL *ttf-font* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))

           (setf *window* (sdl:sdl-create-window "main *window*"
                                                 sdl:+sdl-windowpos-undefined+
                                                 sdl:+sdl-windowpos-undefined+
                                                 width height 0))
           (when (cffi:null-pointer-p *window*)
             (format t "SDL *window* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (format t "initialized SDL window~%")

           (setf *renderer* (sdl:sdl-create-renderer *window* -1
                                                     sdl:+sdl-renderer-accelerated+))
           (when (cffi:null-pointer-p *renderer*)
             (format t "SDL *renderer* failed to initialize: ~a~%" (sdl:sdl-get-error))
             (return-from main))
           (format t "initialized renderer~%")

           (cffi:with-foreign-object (event 'sdl:sdl-event)
             (loop :initially (setf *running* t)
                   :while *running*
                   :do (sdl:sdl-wait-event event) ; 10ms poll
                       (%handle-event event)
                       (%render))))
      ;; unwind
      (format t "stopped~%")
      (setf *running* nil)
      (when *window*
        (sdl:sdl-destroy-window *window*)
        (setf *window* nil))
      (when *renderer*
        (sdl:sdl-destroy-renderer *renderer*)
        (setf *renderer* nil))
      (when *ttf-font*
        (sdl-ttf:ttf-close-font *ttf-font*)
        (setf *ttf-font* nil))
      (sdl-ttf:ttf-quit)
      (sdl:sdl-quit))))
