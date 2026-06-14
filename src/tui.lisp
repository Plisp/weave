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

(defclass completion-state ()
  ((anchor :initarg :anchor
           :reader anchor
           :documentation "the hole associated with this completion")
   (candidates :initarg :candidates
               :accessor candidates)
   (selected :initform nil
             :accessor selected)
   (top-line :initform 0
             :accessor top-line)))

(defclass ui (tui:elemental)
  ((ast :initarg :ast
        :accessor ast)
   (stack :initform (list)
          :accessor stack
          :type list)
   (goal-col :initform 1
             :accessor goal-col
             :type positive-fixnum)
   (completion-state :initform nil
                     :accessor completion-state
                     :type (or null completion-state))))

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

(defclass hole ()
  ((text :initarg :text
         :initform ""
         :accessor text
         :type simple-string)))

(defmethod render-node ((node hole) context rect)
  (with-accessors ((text text)) node
    (let* ((text (if (string= text "") "hole" text))
           (outrect (tui:copy-rect rect :rows 1 :cols (tui:display-width text)))
           (focused (node-active? node (ui context))))
      (tui:puts text 1 1 rect (if focused
                                  (tui:make-style :fg #x0
                                                  :bg (when focused #xb58900)
                                                  :underlinep t)
                                  (tui:make-style :underlinep t)))
      (values (make-instance 'ast-view :rect outrect :node node
                                       :hoverable t
                                       :focused focused
                                       :key-handler (lambda (v e)
                                                      (declare (ignore v))
                                                      (handle-key-for node (ui context) e)))
              0 #x993300))))

(defmethod handle-key-for ((node hole) context event)
  (let ((c (tui:event-kind event)))
    (cond ((and (alpha-char-p c)
                (not (or (tui:event-controlp event)
                         (tui:event-altp event) (tui:event-metap event))))
           ;; TODO edit - swap with symbol node, begin completion
           )
          ((and (digit-char-p c)
                (not (or (tui:event-controlp event)
                         (tui:event-altp event) (tui:event-metap event))))
           ;; TODO edit - swap with literal number node
           )
          ((char= c #\Tab)
           ;; TODO edit - swap with function-call node, begin function completion
           (slog 'tab)))))

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

(defun view-below (atom-array y col)
  "Takes a y-offset from 0 to rows, this way it's possible to search the first row.
COL should essentially indicate some preferred column. Returns NIL if not found."
  ;; note: must bounds-check y first
  (let ((new-y (position-if-not #'null atom-array :start y)))
    (or new-y (return-from view-below nil))
    ;; if offset y has no views after it then we will find our current view
    (loop for view in (aref atom-array new-y)
          for x1 = (tui:rect-x (tui:rect view))
          for x2 = (tui:rect-x2 (tui:rect view))
          do (when (<= col x2)
               (loop-finish))
          finally (return view))))

(defun view-above (atom-array y col)
  "Same as `view-below'"
  (let ((new-y (position-if-not #'null atom-array :end y :from-end t)))
    (or new-y (return-from view-above nil))
    ;; if offset y has no views after it then we will find our current view
    (loop for view in (aref atom-array new-y)
          for x1 = (tui:rect-x (tui:rect view))
          for x2 = (tui:rect-x2 (tui:rect view))
          do (when (<= col x2)
               (loop-finish))
          finally (return view))))

(defun view-left (atom-array y x)
  (if-let (res (find-if (lambda (view) (< (tui:rect-x2 (tui:rect view)) x))
                        (aref atom-array y)
                        :from-end t))
    res ; most-positive-fixnum = very last
    (view-above atom-array y most-positive-fixnum)))

(defun view-right (atom-array y x)
  (if-let (res (find-if (lambda (view) (<= x (tui:rect-x (tui:rect view))))
                        (aref atom-array y)))
    res
    (view-below atom-array (1+ y) 1)))

(defun reconstruct-stack (node root-view)
  "ASSUMES: view tree contains node tree"
  (let ((found-stack (list)))
    (block nil
      (labels ((rec (view stack)
                 (if (and (typep view 'ast-view) (eq (node view) node))
                     (progn
                       (setf found-stack stack)
                       (return))
                     (map nil (lambda (c) (if (typep c 'ast-view) ; oops, was view
                                         (rec c (cons (node c) stack))
                                         (rec c stack)))
                          (tui:children view)))))
        (rec root-view (list))))
    (assert (slog found-stack))
    found-stack))

(defun find-view-for-thing (atom-array node)
  (loop for row across atom-array
        do (loop for v in row
                 do (when (eq node (node v)) ; NOTE: must be a node
                      (return-from find-view-for-thing v)))))

(defun node-active? (node ui)
  (and (stack ui) (eq node (first (stack ui)))))

(defvar *atom-key-handlers* (make-hash-table :test 'equalp))
(defmethod handle-key-for ((node parse::atom-form) ui event)
  (slog (list 'event (tui:event-kind event)))
  (funcall (gethash event *atom-key-handlers*) node ui))

(defun atom-move (view-finder node ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect (find-view-for-thing atom-array node))))
    (multiple-value-bind (new-view new-goal)
        (funcall view-finder atom-array this-rect)
      (when new-view
        (setf (stack ui) (reconstruct-stack (node new-view) (tui:root-view ui)))
        (when new-goal
          (slog (format nil "goal is ~d" new-goal))
          (setf (goal-col ui) new-goal))))))

(setf (gethash (uncursed-sys::make-event :kind :down-arrow) *atom-key-handlers*)
      (lambda (node ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-below atom-array
                                 (1+ (tui:rect-y this-rect)) (goal-col ui)))
                   node ui)))

(setf (gethash (uncursed-sys::make-event :kind :up-arrow) *atom-key-handlers*)
      (lambda (node ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-above atom-array (tui:rect-y this-rect) (goal-col ui)))
         node ui)))

(setf (gethash (uncursed-sys::make-event :kind :left-arrow) *atom-key-handlers*)
      (curry #'atom-move
             (lambda (atom-array this-rect)
               (let ((view (view-left atom-array
                                      (tui:rect-y this-rect) (tui:rect-x this-rect))))
                 (values view (when view (tui:rect-x2 (tui:rect view))))))))

(setf (gethash (uncursed-sys::make-event :kind :right-arrow) *atom-key-handlers*)
      (curry #'atom-move
             (lambda (atom-array this-rect)
               (let ((view (view-right atom-array
                                       (tui:rect-y this-rect) (tui:rect-x2 this-rect))))
                 (values view (when view (tui:rect-x2 (tui:rect view))))))))

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
         (name-x2 (tui:rect-x2 name-rect)))
    (incf (depth context))
    ;; TODO test empty vertical container
    (let* ((args-view (tui:vertical-container
                       (tui:clamp-rect (tui:copy-rect rect :x (+ 1 name-x2)) rect)
                       (mapcar (lambda (n)
                                 (make-instance 'node-with-context :node n
                                                                   :context context))
                               (parse:args node))))
           (args-rect (tui:rect args-view)))
      (decf (depth context))
      (let* ((outrect (tui:copy-rect rect :rows (max 1 (tui:rect-rows args-rect))
                                          :cols (+ 1
                                                   (tui:rect-cols name-rect)
                                                   (tui:rect-cols args-rect))))
             (view (make-instance 'ast-view
                                   :node node
                                   :rect outrect
                                   :children (list name-view args-view))))
        (values view 0 #x339900)))))

;;; let form
(defclass let-cursor ()
  ((bodyform :initform nil
             :accessor bodyform)
   (bind :initform nil
         :accessor bind)
   (init :initform nil
         :accessor init)))

(defmethod render-node ((node parse:let*-form) context rect)
  (let ((prefix "let* ")
        (*print-case* :downcase)
        (pos-y (1+ (tui:rect-y rect)))
        (pos-x (1+ (tui:rect-x rect))))
    (tui:puts prefix pos-x pos-y rect)
    ;; bindings
    (with-slots ((vars parse:vars)) node
      (loop for (var init) in vars
            do (tui:puts (format nil "~a ~a" var init) pos-y (1+ (length prefix)) rect)
               (incf pos-y)))
    ;; thing
    (let* ((bview (tui:vertical-container
                   (tui:copy-rect rect :x (+ 2 (tui:rect-x rect))
                                       :y (1- pos-y))
                   (parse:body node)))
           (brect (tui:rect bview)))
      (values (make-instance 'tui:view
                             :rect (tui:copy-rect rect
                                                  :rows (- (+ (tui:rect-y brect)
                                                              (tui:rect-rows brect))
                                                           (tui:rect-y rect))
                                                  :cols (max (tui:rect-cols brect))))
              0 #x993300))))

;;
;;; main loop
;;

(defmethod tui:render-state ((ui ui))
  (make-instance 'context :ui ui))
(defmethod tui:render ((ctx context) rect)
  (render-node (ast (ui ctx)) ctx rect))
(defmethod tui:render ((o node-with-context) rect)
  (render-node (node o) (context o) rect))

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
                                    (parse::*literal-magic* #\9))
                               (parse::*literal-magic* #\9))
                          (parse::*literal-magic* 3)
                          b)
                        (parse:make-env)))
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
