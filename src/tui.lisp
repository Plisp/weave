;;;;
;;;; terminal frontend
;;;;
;;
;; highlight lexical occurrences of the symbol under cursor using envmaps
;; need a generic method for inserting and deleting nodes
;; add a window for list serialization
;; more efficient navigation to obviate the need for a goal column
;; comments, and deal with more reader macros
;; need to allow curried ast nodes
;; need to analyze and present convert method applications

(defpackage #:weave-tui
  (:use :cl #:alexandria-2 #:weave-utils)
  (:import-from #:weave-parser
                #:location
                #:make-location #:location-node #:location-id #:location=
                #:getloc #:update)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed))
  (:export))
(in-package #:weave-tui)

;; dynamic vars for debugging, should not be used
(defvar *log* (sb-concurrency:make-mailbox :name "log"))
(defvar *tui*)
(defmacro slog (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons ',form ,res))
       ,res)))

(defstruct context-wrapper thing stack (context (error "no context")))
(defun wrap-context (thing stack context)
  (make-context-wrapper :thing thing :stack stack :context context))

(defclass ast-view (tui:view)
  ((location :initarg :loc
             :initform (error "ast view must correspond to a location")
             :accessor location
             :type location)
   (hoverable :initarg :hoverable
              :initform nil
              :accessor hoverable)))

(defgeneric handle-key (node view location context event)
  (:documentation "event is a uncursed key event. If this node is active then
adjust cursor state in the context, specialized for the `cursor'.
If this returns NIL, propagate up the cursor stack.")
  (:method (node view location context event) nil))

(defclass completion-state ()
  ((anchor :initarg :anchor
           :reader anchor
           :documentation "the node associated with this completion")
   (candidates :initarg :candidates
               :accessor candidates)
   (selection :initform 0
              :accessor selection)))

(defclass ui (tui:elemental)
  ((ast :initarg :ast
        :accessor ast)
   (focus :initarg :focus
          :accessor focus
          :type location)
   (goal-col :initform 1
             :accessor goal-col
             :type positive-fixnum)
   (completion-state :initform nil
                     :accessor completion-state
                     :type (or null completion-state))
   ;; these are caches computed on every redisplay
   (stack :initarg :stack
          :accessor stack ; stack is always non-empty
          :type list)
   (node-views :initform (make-hash-table)
               :accessor node-views)))

(defun ui-rect (ui)
  (tui:make-rect :x 0 :y 0 :rows (tui:rows ui) :cols (tui:cols ui)))

;;
;;; ast classes
;;

(defgeneric render-node (node stack context rect))

;; contract: do NOT overwrite child render
(defmethod render-node :before (node stack context rect)
  (let ((i (* 25 (1- (length stack))))) ; stack is always nonempty
    (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                   (tui:copy-rect rect :x 0 :y 0)
                   rect)))

(defvar *global-key-handlers* (make-hash-table :test 'equalp))
(defun global-key-handler (node location context)
  " handler may return t to stop propagation up the stack"
  (lambda (view event)
    (slog event)
    (assert (loc-active location context))
    (if-let (handler (gethash event *global-key-handlers*))
      (funcall handler view context)
      (loop for thisnode = (slog node) then (slog (location-node location))
            for location in (slog (stack context))
            thereis (handle-key thisnode view location context event)))
    ;; state updates here
    (when-let (completion (completion-state context))
      (with-accessors ((candidates candidates)
                       (anchor anchor))
          completion
        (if (not (eq (getloc (focus context)) anchor))
            (setf (completion-state context) nil)
            (setf candidates
                  (remove-if-not (lambda (s)
                                   (alexandria:starts-with-subseq (parse::name anchor) s))
                                 candidates)))))
    ))

(setf (gethash (uncursed-sys::make-event :kind #\i :controlp t) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (state (completion-state ui))
          (setf (selection state) (mod (1+ (selection state))
                                       (length (candidates state)))))))

(setf (gethash (uncursed-sys::make-event :kind #\tab :shiftp t) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (state (completion-state ui))
          (setf (selection state) (mod (1- (selection state))
                                       (length (candidates state)))))))

(setf (gethash (uncursed-sys::make-event :kind #\newline) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (state (completion-state ui))
          ;; only 'full' edits should be updated persistently
          ;; XXX this still requires making a record due to shared state
          ;;     consider (edit 1)-move-(record 1)-edit
          ;;     also consider interning symbols
          (setf (slot-value (anchor state) 'parse::name)
                (nth (selection state) (candidates state)))
          (setf (completion-state ui) nil))))

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
    (map-into rows (lambda (row) (sort row #'< :key (compose #'tui:rect-x #'tui:rect)))
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

(defun atom-move (view-finder view ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect view)))
    (multiple-value-bind (new-view new-goal)
        (funcall view-finder atom-array this-rect)
      (when new-view ; assumes atoms are all ast-views
        (setf (focus ui) (location new-view))
        (when new-goal
          (slog (format nil "goal col is ~d" new-goal))
          (setf (goal-col ui) new-goal))))))

(setf (gethash (uncursed-sys::make-event :kind :down-arrow) *global-key-handlers*)
      (lambda (view ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-below atom-array (1+ (tui:rect-y this-rect)) (goal-col ui)))
                   view ui)))

(setf (gethash (uncursed-sys::make-event :kind :up-arrow) *global-key-handlers*)
      (lambda (view ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-above atom-array (tui:rect-y this-rect) (goal-col ui)))
                   view ui)))

(setf (gethash (uncursed-sys::make-event :kind :left-arrow) *global-key-handlers*)
      (lambda (view ui)
        (atom-move
         (lambda (atom-array this-rect)
           (let ((view (view-left atom-array
                                  (tui:rect-y this-rect) (tui:rect-x this-rect))))
             (values view (when view (tui:rect-x2 (tui:rect view))))))
         view ui)))

(setf (gethash (uncursed-sys::make-event :kind :right-arrow) *global-key-handlers*)
      (lambda (view ui)
        (atom-move
         (lambda (atom-array this-rect)
           (let ((view (view-right atom-array
                                   (tui:rect-y this-rect) (tui:rect-x2 this-rect))))
             (values view (when view (tui:rect-x2 (tui:rect view))))))
         view ui)))

;;; hole
(defclass hole ()
  ((text :initarg :text
         :initform ""
         :accessor text
         :type simple-string)))

(defun loc-active (location context)
  (when (stack context)
    (location= location (focus context))))

(defmethod render-node :around (node stack context rect)
  (let ((vals (multiple-value-list (call-next-method))))
    ;; save window
    (setf (gethash node (node-views context)) (first vals))
    ;; cache stack
    (when (location= (car stack) (focus context))
      (setf (stack context) stack))
    (values-list vals)))

(defmethod parse::is-atom ((node hole)) t)
(defmethod render-node ((node hole) stack context rect)
  (with-accessors ((text text)) node
    (let* ((location (car stack))
           (text (if (string= text "") "hole" text))
           (focused (location= location (focus context))))
      (tui:puts text 1 1 rect (if focused
                                  (tui:make-style :fg #x0
                                                  :bg (when focused #xb58900)
                                                  :underlinep t)
                                  (tui:make-style :fg (tui:color 30 200 0)
                                                  :underlinep t)))
      (make-instance 'ast-view
                      :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                      :loc location
                      :hoverable t
                      ;; leaf handler never called unless focused
                      :key-handler (when focused
                                     (global-key-handler node location context))
                      :focused focused))))

(defun is-regular-char-event (event)
  (and (characterp (tui:event-kind event))
       (not (or (tui:event-controlp event)
                (tui:event-altp event) (tui:event-metap event)))))

(defun completion-candidates (context)
  context
  (loop for s being the symbols of (find-package "CL")
        collect (string-downcase s)))

(defmethod handle-key ((node hole) view location context event)
  (let ((c (tui:event-kind event)))
    (cond
      ((and (is-regular-char-event event) (alpha-char-p c))
       ;; begin completion
       (let ((newnode (make-instance 'parse::symbol-ref :name (string c))))
         (multiple-value-bind (ast newloc)
             (ast-replace location (constantly newnode)
                          (findcdr-if (lambda (l) (location= location l)) (stack context)))
           (setf (ast context) ast
                 (focus context) newloc))
         (setf (completion-state context)
               (make-instance 'completion-state
                              :anchor newnode
                              :candidates (completion-candidates context))))
       t)
      ((and (is-regular-char-event event) (digit-char-p c))
       ;; number node TODO consider other bases
       (let ((newnode (make-instance 'parse::literal :str (string c))))
         (multiple-value-bind (ast newloc)
             (ast-replace location (constantly newnode)
                          (findcdr-if (lambda (l) (location= location l)) (stack context)))
           (setf (ast context) ast
                 (focus context) newloc)))
       t)
      ;; unhandled
      (t nil))))

(defun ensure-string (s)
  (if (symbolp s) (copy-array (string s)) s))

;;; literals - no cursor state needed
(defmethod handle-key ((node parse::literal) view location ui event)
  (when (is-regular-char-event event)
    (with-slots ((s parse::str)) node
      (let ((c (tui:event-kind event)))
        (cond ((digit-char-p c)
               (setf s (ensure-string s))
               (let ((oldlen (length s)))
                 (setf s (adjust-array s (1+ oldlen)))
                 (setf (schar s oldlen) c))
               t)
              ((char= c #\Rubout)
               (setf s (ensure-string s))
               (let ((oldlen (length s)))
                 (if (< 1 oldlen)
                     (progn (setf s (adjust-array s (1- oldlen)))
                            t)
                     (let ((newnode (make-instance 'hole :text "")))
                       (multiple-value-bind (ast newloc)
                           (ast-replace location (constantly newnode)
                                        (findcdr-if (lambda (l) (location= location l))
                                                    (stack ui)))
                         (setf (ast ui) ast
                               (focus ui) newloc))
                       t)))))))))

(defmethod render-node ((node parse::literal) stack context rect)
  (let* ((location (car stack))
         (str (format nil "~a" (parse::str node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (if focused
                               (tui:make-style :fg #x0 :bg (when focused #xb58900))
                               (tui:make-style :fg #x2aa198)))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :loc location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; symbol-references
(defmethod handle-key ((node parse::symbol-ref) view location ui event)
  (when (is-regular-char-event event)
    (with-slots ((s parse::name)) node
      (let ((c (tui:event-kind event)))
        (cond ((alpha-char-p c)
               (setf s (ensure-string s))
               (let ((oldlen (length s)))
                 (setf s (adjust-array s (1+ oldlen)))
                 (setf (schar s oldlen) c))
               (setf (completion-state ui)
                     (make-instance 'completion-state
                                    :anchor node
                                    :candidates (completion-candidates ui)))
               t)
              ((char= c #\Rubout)
               (setf s (ensure-string s))
               (let ((oldlen (length s)))
                 (if (< 1 oldlen)
                     (progn (setf s (adjust-array s (1- oldlen)))
                            t)
                     (let ((newnode (make-instance 'hole :text "")))
                       (multiple-value-bind (ast newloc)
                           (ast-replace location (constantly newnode)
                                        (findcdr-if (lambda (l) (location= location l))
                                                    (stack ui)))
                         (setf (ast ui) ast
                               (focus ui) newloc))
                       t)))))))))

(defmethod render-node ((node parse::symbol-ref) stack context rect)
  (let* ((location (car stack))
         (str (format nil "~a" (parse::name node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (tui:make-style :bg (when focused #xb58900)))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :loc location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; binders
(defmethod render-node ((node parse::binder) stack context rect)
  (let* ((location (car stack))
         (str (format nil "~a" (parse::name node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (tui:make-style :bg (when focused #xb58900) :italicp t))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :loc location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; function call
(defvar *fun-key-handlers* (make-hash-table :test 'equalp))
(defmethod handle-key ((node parse::function-call) view location ui event)
  (when-let ((handler (gethash event *fun-key-handlers*)))
    (funcall handler node location ui)))

;; XXX nonstandard loop
(defun ast-replace (loc updater stack)
  "Requires that (location-node `loc') = (location-node (car `stack'))
for loop invariant node ~ (location-node old-loc).
Returns the new ast and location relative to the updated node."
  (loop with old = (getloc loc)
        with id = (location-id loc)
        with newnode = (update (location-node loc) id (funcall updater old))
        for old-loc in stack
        until (eq t (location-node old-loc))
        for node = newnode then (update (location-node old-loc) (location-id old-loc) node)
        finally (return (values node (make-location :node newnode :id id)))))

(defun insert-hole (node call-loc ui)
  "Assumes body is an identifier (unique).
Inserts after current node if currently in the body"
  (slog `("insert hole" ,node))
  ;; inserts a hole after the current item and focuses it
  (let ((i (position-if (lambda (l) (location= call-loc l)) (stack ui))))
    (when (zerop i)
      ;; may handle further up
      (return-from insert-hole nil))
    ;; non-toplevel, child (1- i) >= 0
    (let* ((stack (nthcdr (1- i) (stack ui)))
           (id (location-id (car stack)))
           (id (if (integerp id) (1+ id) 0)))
      (multiple-value-bind (ast child-loc)
          (ast-replace (make-location :node node :id 'parse::body)
                       (lambda (body) (list-insert body (make-instance 'hole) id))
                       stack)
        (slog ast)
        (slog child-loc)
        (setf (focus ui) (make-location :node (location-node child-loc) :id id)
              (ast ui) ast))
      t)))
(setf (gethash (uncursed-sys::make-event :kind #\newline :altp t) *fun-key-handlers*)
      #'insert-hole)

(defmethod render-node ((node parse::function-call) stack context rect)
  (let* ((location (car stack))
         (name-loc (make-location :node node :id 'name))
         (name-view (render-node (parse::name node)
                                 (cons name-loc stack) context rect))
         (name-rect (tui:rect name-view))
         (args-view (tui:vertical-container
                     (tui:clamp-rect (tui:copy-rect rect :x (+ 1 (tui:rect-x2 name-rect)))
                                     rect)
                     (enumerate
                      (lambda (argnode i)
                        (wrap-context argnode
                                      (cons (make-location :node node :id i) stack)
                                      context))
                      (parse::body node))))
         (args-rect (tui:rect args-view)))
    (make-instance 'ast-view
                    :loc location
                    :rect (tui:copy-rect rect :rows (max 1 (tui:rect-rows args-rect))
                                              :cols (+ 1
                                                       (tui:rect-cols name-rect)
                                                       (tui:rect-cols args-rect)))
                    :children (list name-view args-view)
                    :key-handler (global-key-handler node location context)
                    :focused (location= location (focus context)))))

;;; let form

;; (defmethod render-node ((node parse:let*-form) location stack context rect)
;;   (let ((prefix "let* "))
;;     (tui:puts prefix 1 1 rect (tui:make-style :boldp t))
;;     (let* ((bindings
;;              (tui:vertical-container
;;               (tui:clamp-rect (tui:copy-rect rect :x (+ (tui:rect-x rect)
;;                                                         (tui:display-width prefix)))
;;                               rect)
;;               (loop for (var initform) in (parse:vars node)
;;                     for i from 0
;;                     collect (wrap-context (make-bind-pair :var var :init initform :index i)
;;                                          context))))
;;            (bindrect (tui:rect bindings)))
;;       (incf (stack context))
;;       (let* ((bview (tui:vertical-container
;;                      (tui:clamp-rect (tui:copy-rect rect :x (+ 2 (tui:rect-x rect))
;;                                                          :y (tui:rect-y2 bindrect))
;;                                      rect)
;;                      (mapcar (lambda (node) (wrap-context node context))
;;                              (parse:body node))))
;;              (brect (tui:rect bview)))
;;         (decf (stack context))
;;         (make-instance 'ast-view
;;                         :loc (make-location :node node)
;;                         :children (list bindings bview)
;;                         :rect (tui:copy-rect rect
;;                                              :rows (+ (tui:rect-y bindrect)
;;                                                       (tui:rect-y brect))
;;                                              :cols (max (+ (length prefix)
;;                                                            (tui:rect-cols bindrect))
;;                                                         (+ 2 (tui:rect-cols brect)))))))))

;;; completions
(defclass completion ()
  ((name :initarg :name
         :reader name
         :initform (error "name not provided")
         :type string)
   (index :initarg :index
          :reader index
          :initform (error "index not provided")
          :type integer)
   (state :initarg :state
          :reader state
          :initform (error "state not provided")
          :type completion-state)))

(defmethod tui:render ((c completion) rect)
  (if (plusp (tui:rect-rows rect))
      (let* ((state (state c))
             (prefix (parse::name (anchor state))))
        (flet ((style (bold)
                 (tui:make-style :fg #xeeeeee
                                 :bg (when (eql (index c) (selection state)) #x2aa198)
                                 :boldp bold)))
          (tui:puts prefix 1 1 rect (style t))
          (tui:puts (nth-value 1 (alexandria:starts-with-subseq prefix (name c)
                                                                :return-suffix t))
                    1 (+ 1 (length prefix)) rect (style nil)))
        (make-instance 'tui:view
                       :rect (tui:copy-rect rect :rows 1
                                                 :cols (tui:display-width (name c)))))
      (make-instance 'tui:view :rect (tui:copy-rect rect :rows 0))))

(defun render-completion (ui)
  (when-let (state (completion-state ui))
    (let* ((anchor (anchor state))
           (anchor-rect (tui:rect (gethash anchor (node-views ui))))
           (maxlen (loop for c in (candidates state) maximize (tui:display-width c))))
      (tui:vertical-container
       (tui:clamp-rect (tui:make-rect :x (tui:rect-x anchor-rect)
                                      :y (tui:rect-y2 anchor-rect)
                                      :rows (tui:rows ui)
                                      :cols (max maxlen (tui:cols ui)))
                       (ui-rect ui))
       (enumerate (lambda (s i)
                    (make-instance 'completion :name s
                                               :index i
                                               :state state))
                  (candidates state))))))

;;
;;; main loop
;;

(defmethod tui:render ((ui ui) rect)
  ;; do not allow use of old caches
  (clrhash (node-views ui))
  (setf (stack ui) nil)
  ;; note: order of this list matters
  (let ((toplevel-views
          (list (render-node (ast ui) (list (make-location :node t)) ui rect)
                (render-completion ui))))
    (make-instance
     'tui:view
     :rect (ui-rect ui)
     :children (delete nil toplevel-views))))

(defmethod tui:render ((o context-wrapper) rect)
  (render-node (context-wrapper-thing o) (context-wrapper-stack o)
               (context-wrapper-context o)
               rect))

(defmethod tui:redisplay :around ((ui ui))
  (restart-case
      (progn
        (call-next-method)
        (sb-concurrency:send-message *log* (cons nil 'redisplay-done)))
    (stop ()
      :report "exit"
      (tui:stop ui))))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\c)
             (tui:event-controlp event))
        (tui:stop ui)
        (call-next-method))
    (sb-concurrency:send-message *log* (cons nil 'event-handled))))

(defvar *log-stop* (gensym))
(defun tui-main ()
  (let* ((ast (parse:parse ';; (let* ((aaa (parse::*literal-magic* "3"))
                            ;;        (bbb (1+ aaa)))
                            ;;  (1- b))
               (- (+ a (parse::*literal-magic* "3")) b)
               (parse:make-env)))
         (root-loc (make-location :node t))
         (tui (make-instance 'ui :ast ast
                                 :focus root-loc
                                 :stack (list root-loc)))
         (*tui* tui))
    (unwind-protect (tui:run tui :redisplay-on-input t)
      (slog *log-stop*))))

(defun main ()
  (if (member :slynk *features*)
      (progn
        (bt:make-thread (lambda () (tui-main)))
        (loop :for (form . value) = (sb-concurrency:receive-message *log*)
              :until (eq value *log-stop*)
              :do (if form
                      (format t "~a~%|> ~s~%" form value)
                      (format t "~a~a~%" (make-string 70 :initial-element #\-) value))
                  (force-output)))
      (tui-main)))
