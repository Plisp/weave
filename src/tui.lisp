;;;;
;;;; terminal frontend
;;;;
;;
;; TODO
;; highlight lexical occurrences of the symbol under cursor using envmaps
;; need a generic method for inserting and deleting nodes
;; add a window for list serialization
;; more efficient navigation to obviate the need for a goal column
;; comments, and deal with more reader macros
;; need to allow curried ast nodes
;; need to analyze and present convert method applications

(defpackage #:weave-tui
  (:use :cl #:alexandria-2 #:weave-utils)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed))
  (:export))
(in-package #:weave-tui)

;; dynamic vars for debugging, should not be used
(defvar *log*)
(defvar *tui*)
(defun slog (o)
  #+(and sbcl slynk) (sb-concurrency:send-message *log* o)
  o)

(defclass ui (tui:elemental)
  ((ast :initarg :ast
        :accessor ast)
   (stack :initform (list)
          :accessor stack
          :type list)))

(defclass context ()
  ((ui :initarg :ui
       :initform (error "no ui in context")
       :accessor ui
       :type ui)
   (depth :initarg :depth
          :initform 0
          :accessor depth
          :type non-negative-fixnum)))

(defclass ast-view (tui:view)
  ((node :initarg :node
         :initform (error "no node in ast view?")
         :accessor node)
   (hoverable :initarg :hoverable
              :initform nil
              :accessor hoverable)))

(defclass node-with-context ()
  ((node :initarg :node
         :reader node)
   (context :initarg :context
            :reader context)))

;;
;;; ast classes
;;
(defgeneric render-node (ast context rect))

(defmethod tui:render ((o node-with-context) rect)
  (render-node (node o) (context o) rect))

(defmethod render-node :before (node context rect)
  (slog `(rendering ,node to ,rect)))

;; contract: do NOT overwrite child render
(defmethod render-node :before (node context rect)
  (let ((i (* 10 (depth context))))
    (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                   (tui:copy-rect rect :x 0 :y 0)
                   rect)))

(defgeneric handle-key-for (ast context event)
  (:documentation "event is a uncursed key event. If this node is active then
adjust cursor state in the context, specialized for the `ast' node.
Typically we will use the default implementation below."))

;; ASSUME leaf nodes are non-overlapping
;; we index them into an array of blocks sorted by line then column
(defun build-atom-array (tree rows)
  (let ((rows (make-array rows :initial-element (list))))
    (tui::view-traverse tree (lambda (view)
                               (when (and (typep view 'ast-view)
                                          (hoverable view))
                                 (let ((rect (tui:rect view)))
                                   (push view (aref rows (tui:rect-y rect)))))
                               t))
    (map-into rows (lambda (row) (sort row #'< :key (lambda (v) (tui:rect-x (tui:rect v)))))
              rows)))

(defun node-below (atom-array y col)
  "Takes a y-offset from 0 to rows, this way it's possible to search the first row.
COL should essentially indicate some preferred column. Returns NIL if not found."
  ;; note: must bounds-check y first
  (let ((new-y (position-if-not #'null atom-array :start y)))
    (or new-y (return-from node-below nil))
    ;; if offset y has no views after it then we will find our current view
    (slog atom-array)
    (slog (format nil "down to y: ~d, col: ~d" new-y col))
    (loop for view in (aref atom-array new-y)
          for x1 = (tui:rect-x (tui:rect view))
          for x2 = (+ (tui:rect-x (tui:rect view)) (tui:rect-cols (tui:rect view)))
          do (when (<= (1+ x1) col x2)
               (loop-finish))
          finally (return (slog (node view))))))

(defun node-above (atom-array y col)
  "Same as `node-below'"
  (let ((new-y (position-if-not #'null atom-array :end y :from-end t)))
    (or new-y (return-from node-above nil))
    ;; if offset y has no views after it then we will find our current view
    (slog atom-array)
    (slog (format nil "up to y: ~d, col: ~d" new-y col))
    (loop for view in (aref atom-array new-y)
          for x1 = (tui:rect-x (tui:rect view))
          for x2 = (+ (tui:rect-x (tui:rect view)) (tui:rect-cols (tui:rect view)))
          do (when (<= (1+ x1) col x2)
               (loop-finish))
          finally (return (slog (node view))))))

(defun node-left (atom-array y x)
  (slog (list atom-array y x))
  (if-let (res (find-if (lambda (view)
                          (let* ((rect (tui:rect view))
                                 (view-x2 (+ (tui:rect-x rect) (tui:rect-cols rect))))
                            (< view-x2 x)))
                        (aref atom-array y)
                        :from-end t))
    (node res)
    (node-above atom-array y 1)))

(defun node-right (atom-array y x)
  (slog (list atom-array y x))
  (if-let (res (find-if (lambda (view)
                          (<= x (tui:rect-x (tui:rect view))))
                        (aref atom-array y)))
    (node res)
    (node-below atom-array (1+ y) 1)))

(defun reconstruct-stack (node root-view)
  (let ((found-stack (list)))
    (block nil
      (labels ((rec (view stack)
                 (if (and (typep view 'ast-view) (eq (node view) node))
                     (progn
                       (setf found-stack stack)
                       (return))
                     (map nil (lambda (c) (if (typep c 'ast-view) ; oops, was view
                                         (rec c (cons c stack))
                                         (rec c stack)))
                          (tui:children view)))))
        (rec root-view (list))))
    (slog (mapcar 'node found-stack))
    (assert found-stack)
    found-stack))

(defun find-view-for-thing (atom-array node)
  (loop for row across atom-array
        do (loop for v in row
                 do (when (eq node (node v)) ; NOTE: must be a node
                      (return-from find-view-for-thing v)))))

(defun node-active? (node ui)
  (and (stack ui) (eq node (node (first (stack ui))))))

(defvar *atom-key-handlers* (make-hash-table :test 'equalp))
(defmethod handle-key-for ((node parse::atom-form) ui event)
  (slog (list 'moving-in-atom node event))
  (funcall (gethash event *atom-key-handlers*) node ui))

(defun atom-move-downwards (node ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect (find-view-for-thing atom-array node))))
    (setf (stack ui)
          (reconstruct-stack (or (node-below atom-array
                                             (1+ (tui:rect-y this-rect))
                                             (+ (tui:rect-x this-rect)
                                                (truncate (tui:rect-x this-rect) 2)))
                                 node)
                             (tui:root-view ui)))))

(setf (gethash (uncursed-sys::make-event :kind :down-arrow) *atom-key-handlers*)
      'atom-move-downwards)

(defun atom-move-upwards (node ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect (find-view-for-thing atom-array node))))
    (setf (stack ui)
          (reconstruct-stack (or (node-above atom-array
                                             (tui:rect-y this-rect)
                                             (+ (tui:rect-x this-rect)
                                                (truncate (tui:rect-x this-rect) 2)))
                                 node)
                             (tui:root-view ui)))))

(setf (gethash (uncursed-sys::make-event :kind :up-arrow) *atom-key-handlers*)
      'atom-move-upwards)

(defun atom-move-left (node ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect (find-view-for-thing atom-array node))))
    (setf (stack ui)
          (reconstruct-stack (or (node-left atom-array (tui:rect-y this-rect)
                                            (tui:rect-x this-rect))
                                 node)
                             (tui:root-view ui)))))

(setf (gethash (uncursed-sys::make-event :kind :left-arrow) *atom-key-handlers*)
      'atom-move-left)

(defun atom-move-right (node ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect (find-view-for-thing atom-array node))))
    (setf (stack ui)
          (reconstruct-stack (or (node-right atom-array (tui:rect-y this-rect)
                                             (+ (tui:rect-x this-rect)
                                                (tui:rect-cols this-rect)))
                                 node)
                             (tui:root-view ui)))))

(setf (gethash (uncursed-sys::make-event :kind :right-arrow) *atom-key-handlers*)
      'atom-move-right)

;;; literals - no cursor state needed
(defmethod render-node ((node parse::literal-form) context rect)
  (let* ((str (format nil "~a" (parse:form node)))
         (outrect (tui:copy-rect rect :rows 1 :cols (tui:display-width str)))
         (focused (node-active? node (ui context))))
    (tui:puts str 1 1 rect (if focused
                               (tui:make-style :fg #x0
                                               :bg (when focused #xb58900))
                               (tui:make-style :fg #x2aa198)))
    (values (make-instance 'ast-view :rect outrect :node node
                                     :hoverable t
                                     :focused focused
                                     :key-handler (lambda (v e)
                                                    (declare (ignore v))
                                                    (handle-key-for node (ui context) e)))
            0 #x993300)))

;;; symbol-references
(defmethod render-node ((node parse::symbol-ref) context rect)
  (let* ((str (format nil "~a" (parse:name node)))
         (outrect (tui:copy-rect rect :rows 1 :cols (tui:display-width str)))
         (focused (node-active? node (ui context))))
    (tui:puts str 1 1 rect (tui:make-style :bg (when focused #xb58900)))
    (values (make-instance 'ast-view :rect outrect :node node
                                     :hoverable t
                                     :focused focused
                                     :key-handler (lambda (v e)
                                                    (declare (ignore v))
                                                    (handle-key-for node (ui context) e)))
            0 #x993300)))

;;; function call
(defmethod render-node ((node parse::function-call) context rect)
  ;; name(args
  ;;      ...)
  (let* ((name-view (render-node (parse:name node) context rect))
         (name-rect (tui:rect name-view))
         (name-x2 (+ (tui:rect-x name-rect) (tui:rect-cols name-rect))))
    (incf (depth context))
    ;; TODO test empty vertical container
    (let* ((args-view (tui:vertical-container
                       (tui::clamp-rect (tui:copy-rect rect :x (+ 1 name-x2)) rect)
                       (mapcar (lambda (n)
                                 (make-instance 'node-with-context :node n
                                                                   :context context))
                               (parse:args node))))
           (args-rect (tui:rect args-view)))
      (let* ((outrect (tui:copy-rect rect :rows (max 1 (tui:rect-rows args-rect))
                                          :cols (+ 1
                                                   (tui:rect-cols name-rect)
                                                   (tui:rect-cols args-rect))))
             (view (make-instance 'ast-view
                                   :node node
                                   :rect outrect
                                   :children (list name-view args-view))))
        (values view 0 #x339900)))))

;; TODO be generic
;; (defmethod handle-key-for ((node parse::function-call) context event)
;;   (when (node-active? node context)
;;     ;; navigate to the next argument
;;     ))

;;; let form
;; (defclass let-cursor ()
;;   ((form :initarg :form
;;          :initform (error "cursor form not provided")
;;          :reader form)
;;    (bodyform :initform nil
;;              :accessor bodyform)
;;    (bind :initform nil
;;          :accessor bind)
;;    (init :initform nil
;;          :accessor init)))

;; ;; XXX child reationshpi
;; (defmethod render-node ((node parse:let*-form) context rect)
;;   (let ((prefix "let* ")
;;         (*print-case* :downcase)
;;         (pos-y (1+ (tui:rect-y rect)))
;;         (pos-x (1+ (tui:rect-x rect))))
;;     (tui:puts prefix pos-x pos-y rect)
;;     ;; bindings
;;     (with-slots ((vars parse:vars)) node
;;       (loop for (var init) in vars
;;             do (tui:puts (format nil "~a ~a" var init) pos-y (1+ (length prefix)) rect)
;;                (incf pos-y)))
;;     ;; thing
;;     (let* ((bview (tui:vertical-container
;;                    (tui:copy-rect rect :x (+ 2 (tui:rect-x rect))
;;                                        :y (slog (1- pos-y)))
;;                    (parse:body node)))
;;            (brect (tui:rect bview)))
;;       (values (make-instance 'tui:view
;;                              :rect (tui:copy-rect rect
;;                                                   :rows (- (+ (tui:rect-y brect)
;;                                                               (tui:rect-rows brect))
;;                                                            (tui:rect-y rect))
;;                                                   :cols (max (tui:rect-cols brect))))
;;               0 #x993300))))

;;
;;; main loop
;;

(defmethod tui:render-state ((ui ui))
  (make-instance 'context :ui ui))
(defmethod tui:render ((ctx context) rect)
  (render-node (ast (ui ctx)) ctx rect))

(defmethod tui:redisplay :after ((ui ui))
  (slog (make-string 100 :initial-element #\-))
  (when (null (stack ui))
    (let ((views (build-atom-array (tui:root-view ui) (tui:rows ui))))
      (setf (stack ui)
            (reconstruct-stack (node-below views 0 1) (tui:root-view ui))))))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\c)
             (tui:event-controlp event))
        (tui:stop ui)
        (call-next-method))))

(defun tui-main ()
  (let* ((ast ;; (parse:parse '(let* ((a (parse::*literal-magic* 3))
              ;;                      (b (1+ a)))
              ;;                (1- b))
              ;;              (parse:make-env))
           (parse:parse '(foo (bar (bazaar (parse::*literal-magic* 5)
                                    (parse::*literal-magic* #\9)))
                          (parse::*literal-magic* 3)
                          b)
                         (parse:make-env))
              )
         (tui (make-instance 'ui :ast ast)))
    (setf *tui* tui)
    (tui:run tui :redisplay-on-input t)
    #+sbcl
    (sb-concurrency:send-message *log* :stop)))

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
