;;;;
;;;; terminal frontend
;;;;
;;
;; anchored selection of subtrees
;; highlight lexical occurrences of the symbol under cursor using envmaps
;; need to convert method applications

(defpackage #:weave-tui
  (:use :cl #:alexandria-2 #:weave-utils)
  (:import-from #:weave-parser
                #:location #:make-location #:location-node #:location-id #:location=
                #:lockind #:getloc #:update)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed)
                    (#:tui-sys #:uncursed-sys))
  (:export))
(in-package #:weave-tui)

;; dynamic vars for debugging, should not be used
(defvar *log* (sb-concurrency:make-mailbox :name "log"))
(defmacro slog (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons ',form ,res))
       ,res)))

(defclass ast-view (tui:view)
  ((location :initarg :location
             :initform (error "ast view must correspond to a location")
             :accessor location
             :type location)
   (hoverable :initarg :hoverable
              :initform nil
              :accessor hoverable)))

(defgeneric handle-key (node view location ui event)
  (:documentation "event is a uncursed key event. If this node is active then
adjust cursor state in the context, specialized for the `cursor'.
If this returns NIL, propagate up the cursor stack.")
  (:method (node view location context event) nil))

(defclass completion-state ()
  ((anchor :initarg :anchor
           :accessor anchor
           :documentation "the node associated with this completion")
   (candidates :initarg :candidates
               :accessor candidates)
   (selection :initform 0
              :accessor selection)))

(defclass state ()
  ((ast :initarg :ast
        :initform (error "must provide ast")
        :reader ast)
   (focus :initarg :focus
          :initform (error "must provide insertion focus")
          :reader focus)))

(defclass ui (tui:elemental)
  ((ast :initarg :ast
        :initform (error "no ast")
        :accessor ast)
   (focus :initarg :focus
          :initform (error "no focus")
          :accessor focus)
   (edit-loc :initarg :edit-loc
             :initform nil
             :accessor edit-loc)
   (goal-col :initform 1
             :accessor goal-col
             :type positive-fixnum)
   (completion-state :initform nil
                     :accessor completion-state
                     :type (or null completion-state))
   (history :initform (list)
            :accessor history
            :type list)
   (future :initform (list)
           :accessor future
           :type list)
   ;; these are caches computed on every redisplay
   (stack :initarg :stack
          :accessor stack ; stack is always non-empty
          :type list)
   (focus-rect :initform nil
               :accessor focus-rect
               :type (or null tui:rect))
   (node-views :initform (make-hash-table)
               :accessor node-views)))

(defun ui-rect (ui)
  (tui:make-rect :x 0 :y 0 :rows (tui:rows ui) :cols (tui:cols ui)))

(defmethod parse:get-location ((node ui) id)
  (declare (ignore id))
  (ast node))

(defmethod parse:update ((ui ui) id new-value)
  (declare (ignore id))
  (setf (ast ui) new-value)
  ui)

(defun append-id (id i)
  (cond ((eq id 'parse:body) i)
        ((symbolp id) (list id i))
        (t `(,@id ,i))))

(defun parent-id (id)
  (cond ((integerp id) 'parse:body)
        ((listp id)
         (let ((l (butlast id)))
           (if (= (length l) 1)
               (car l)
               l)))
        (t (cerror "continue" "what parent id ~a" id))))

(defun id-index (id)
  (if (listp id) (lastcar id) id))

(defun ast-replace (loc updater stack)
  "Requires that (location-node `loc') = (location-node (car `stack'))
for loop invariant node ~ (location-node old-loc).
Returns the new ast and location relative to the updated node."
  (loop with old = (getloc loc)
        with id = (location-id loc)
        with newnode = (update (location-node loc) id (funcall updater old))
        for old-loc in stack
        for node = newnode then (update (location-node old-loc) (location-id old-loc) node)
        finally (return (make-location :node newnode :id id))))

(defun ast-insert (item loc index stack)
  (let ((child-loc (ast-replace loc
                                (lambda (body) (list-insert body item index))
                                stack)))
    (make-location :node (location-node child-loc)
                   :id (append-id (location-id loc) index))))

(defun ast-delete (loc index stack)
  "assumes that list has length > 1"
  (let ((child-loc (ast-replace loc (lambda (body) (list-remove body index)) stack)))
    (make-location :node (location-node child-loc)
                   :id (append-id (location-id loc) (max 0 (1- index))))))

(defun parent-stack (stack)
  "This relies on ids being either 'symbol or ('symbol integer*) for list addressing."
  (let* ((loc (car stack))
         (id (location-id loc)))
    (cond ((listp id)
           (cons (make-location :node (location-node loc) :id (parent-id id))
                 (cdr stack)))
          (t (cdr stack)))))

(defun save-history (ui)
  (push (make-instance 'state :focus (focus ui) :ast (ast ui))
        (history ui)))

(defun swap-node (location newnode ui)
  "saves undo history"
  (when (or (null (edit-loc ui)) (not (location= location (edit-loc ui))))
    (save-history ui))
  ;; perform the insertion
  (let ((newloc (ast-replace location (constantly newnode)
                             (findcdr-if (lambda (l) (eq (location-node location)
                                                    (location-node l)))
                                         (stack ui)))))
    (setf (future ui) nil
          (focus ui) newloc
          (edit-loc ui) newloc)))

;;
;;; ast classes
;;

(defgeneric unwrap (node)
  (:method (node) node))
(defgeneric render-node (node stack context rect))

(defmethod render-node :around (node stack context rect)
  (let* ((vals (multiple-value-list (call-next-method)))
         (view (first vals))
         (rect (tui:rect view)))
    ;; inverse linear scaling, drawing after children isn't ideal
    (let ((i (truncate (- 255 (/ 255 (1+ (/ (expt (length stack) 2) 8)))))))
      (unless (parse:is-atom node)
        (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                       (tui:copy-rect rect :x 0 :y 0) rect
                       :blend 0.1)))
    ;; save window
    (setf (gethash (unwrap node) (node-views context)) view)
    (when (eq (unwrap node) (getloc (focus context)))
      (setf (focus-rect context) rect))
    ;; cache stack
    (when (location= (car stack) (focus context))
      (setf (stack context) stack))
    (values-list vals)))

(defvar *default-key-handlers* (make-hash-table :test 'equal))
(defvar *global-key-handlers* (make-hash-table :test 'equal))
(defun global-key-handler (node location ui)
  " handler may return t to stop propagation up the stack"
  (lambda (view event)
    (slog event)
    (assert (location= location (focus ui)))
    (or (when-let (handler (gethash event *global-key-handlers*))
          (funcall handler view ui))
        ;; propagate
        (loop for thisnode = (slog node) then (slog (location-node location))
              for location in (slog (stack ui))
              thereis (handle-key thisnode view location ui event))
        ;; contextual handlers
        (when-let (handler (gethash event *default-key-handlers*))
          (slog "default handler called")
          (funcall handler view ui)))
    ;; state updates here
    (when-let (completion (completion-state ui))
      (with-accessors ((candidates candidates)
                       (anchor anchor))
          completion
        (if (not (eq (getloc (focus ui)) anchor))
            (setf (completion-state ui) nil)
            (progn ;; note: if we're typing then we've already converted symbol->string
              (let* ((name (parse:name anchor))
                     (valid (delete-if-not (lambda (s) (starts-with-subseq name s))
                                           candidates))
                     (exact-match (find name valid :test #'string=)))
                (if exact-match
                    (setf candidates
                          (sort valid #'<
                                :key (lambda (s)
                                       (mk-string-metrics:damerau-levenshtein s name))))
                    (setf candidates valid)))))))
    ))

(setf (gethash (tui-sys:make-event :kind #\i :controlp t) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (state (completion-state ui))
          (setf (selection state) (mod (1+ (selection state))
                                       (length (candidates state)))))))

(setf (gethash (tui-sys:make-event :kind #\tab :shiftp t) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (state (completion-state ui))
          (setf (selection state) (mod (1- (selection state))
                                       (length (candidates state)))))))


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
  (if-let (res (find-if (lambda (view) (<= (tui:rect-x2 (tui:rect view)) x))
                        (slog (aref atom-array y))
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
        (setf (focus ui) (slog (location new-view)))
        (when new-goal
          (slog (format nil "goal col is ~d" new-goal))
          (setf (goal-col ui) new-goal))))))

(setf (gethash (tui-sys:make-event :kind #\n :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-below atom-array (1+ (tui:rect-y this-rect)) (goal-col ui)))
                   view ui)))

(setf (gethash (tui-sys:make-event :kind #\p :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (atom-move (lambda (atom-array this-rect)
                     (view-above atom-array (tui:rect-y this-rect) (goal-col ui)))
                   view ui)))

(setf (gethash (tui-sys:make-event :kind #\b :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (atom-move
         (lambda (atom-array this-rect)
           (let ((view (view-left atom-array
                                  (tui:rect-y this-rect) (tui:rect-x this-rect))))
             (values view (when view (tui:rect-x2 (tui:rect view))))))
         view ui)))

(setf (gethash (tui-sys:make-event :kind #\f :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (atom-move
         (lambda (atom-array this-rect)
           (let ((view (view-right atom-array
                                   (tui:rect-y this-rect) (tui:rect-x2 this-rect))))
             (values view (when view (tui:rect-x2 (tui:rect view))))))
         view ui)))

(defun move-parent (ui)
  (when-let (new-stack (parent-stack (stack ui)))
    (slog `(moving to ,new-stack))
    (setf (focus ui) (car new-stack)
          (stack ui) new-stack)))

(setf (gethash (tui-sys:make-event :kind #\p :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (move-parent ui)))

(setf (gethash (tui-sys:make-event :kind #\P :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        ;; this is really a do-while idiom
        (prog1 (move-parent ui)
          (loop for last-focus = nil then loc
                for loc = (slog (car (stack ui)))
                for node = (getloc loc)
                while (typep node 'parse:eval-form)
                until (eq (location-node loc) ui)
                do (move-parent ui)
                finally (when (slog last-focus)
                          (setf (focus ui) last-focus))))))

;;; undo

;; can't pop the one remaining thing
(defun undo (ui)
  (unless (null (history ui))
    (let ((prev (pop (history ui))))
      (slog 'undo)
      (push (make-instance 'state :ast (ast ui) :focus (focus ui))
            (future ui))
      (setf (ast ui) (ast prev)
            (focus ui) (focus prev)))))

(defun redo (ui)
  (unless (null (future ui))
    (let ((next (pop (future ui))))
      (slog 'redo)
      (push (make-instance 'state :ast (ast ui) :focus (focus ui))
            (history ui))
      (setf (ast ui) (ast next)
            (focus ui) (focus next)))))

(setf (gethash (tui-sys:make-event :kind #\u :controlp t) *global-key-handlers*)
      (lambda (view ui) view (undo ui)))
(setf (gethash (tui-sys:make-event :kind #\r :controlp t) *global-key-handlers*)
      (lambda (view ui) view (redo ui)))

;;; hole
;; holes substitute editable structure: atoms and eval-forms

(defclass hole ()
  ((text :initarg :text
         :initform (error "hole text not provided")
         :accessor text
         :type simple-string)))
(defun hole (&optional (text "")) (make-instance 'hole :text text))

(defmethod parse:is-atom ((node hole)) t)
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
                     :location location
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
  (declare (ignore context))
  (loop for s being the symbols of (find-package "CL") collect (string s)))

(defmethod handle-key ((node hole) view location ui event)
  (let ((c (tui:event-kind event)))
    (when (is-regular-char-event event)
      (trivia:match (parse:lockind location)
        ((eql 'parse:binder)
         (when (and (graphic-char-p c) (not (digit-char-p c)))
           (swap-node location (make-instance 'parse:binder :name (string-upcase c)) ui)))
        ((eql 'parse:eval-form)
         (cond
           ((and (graphic-char-p c) (not (digit-char-p c)))
            (let ((newnode (make-instance 'parse:symbol-ref :name (string-upcase c))))
              (swap-node location newnode ui)
              ;; begin completion
              (setf (completion-state ui)
                    (make-instance 'completion-state
                                   :anchor newnode
                                   :candidates (completion-candidates ui)))))
           ((digit-char-p c)
            (let ((newnode (make-instance 'parse:literal :str (string c))))
              (swap-node location newnode ui)))))
        ((eql 'parse:symbol-ref)
         (when (and (graphic-char-p c) (not (digit-char-p c)))
           (let ((newnode (make-instance 'parse:symbol-ref :name (string-upcase c))))
             (swap-node location newnode ui)
             ;; begin completion
             (setf (completion-state ui)
                   (make-instance 'completion-state
                                  :anchor newnode
                                  :candidates (completion-candidates ui))))))))))

;;; literals - no cursor state needed
(defmethod handle-key ((node parse:literal) view location ui event)
  (when (is-regular-char-event event)
    (let ((s (parse:str node))
          (c (tui:event-kind event)))
      (cond ((digit-char-p c)
             (swap-node location
                        (make-instance 'parse:literal :str (format nil "~a~a" s c))
                        ui)
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (swap-node location
                              (make-instance 'parse:literal
                                             :str (string-drop s 1))
                              ui)
                   (swap-node location (hole) ui)))
             t)))))

(defmethod render-node ((node parse:literal) stack context rect)
  (let* ((location (car stack))
         (str (format nil "~a" (parse:str node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (if focused
                               (tui:make-style :fg #x0 :bg (when focused #xb58900))
                               (tui:make-style :fg #x2aa198)))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; symbol-references
(defmethod handle-key ((node parse:symbol-ref) view location ui event)
  (when (is-regular-char-event event)
    (let ((s (parse:name node))
          (c (tui:event-kind event)))
      (cond ((graphic-char-p c)
             (let ((newnode
                     (make-instance 'parse:symbol-ref
                                    :name (format nil "~a~a" s (string-upcase c)))))
               (swap-node location newnode ui)
               (if-let (state (completion-state ui))
                 (setf (anchor state) newnode)
                 (setf (completion-state ui)
                       (make-instance 'completion-state
                                      :anchor newnode
                                      :candidates (completion-candidates ui)))))
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (let ((newnode (make-instance 'parse:symbol-ref
                                                 :name (string-drop s 1))))
                     (swap-node location newnode ui)
                     (when (completion-state ui)
                       (setf (completion-state ui)
                             (make-instance 'completion-state
                                            :anchor newnode
                                            :candidates (completion-candidates ui)))))
                   (swap-node location (hole) ui)))
             t)))))

(defparameter *symbol-mappings* '((<= . #\≤) (>= . #\≥) (* . #\⋅) (/= . #\≠)))

(defmethod render-node ((node parse:symbol-ref) stack context rect)
  (let* ((location (car stack))
         (name (parse:name node))
         (str (if-let (exp (assoc-value *symbol-mappings* name :test #'string-equal))
                (string exp)
                (string-downcase name)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (tui:make-style :bg (when focused #xb58900)))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; binders
(defmethod handle-key ((node parse:binder) view location ui event)
  (when (is-regular-char-event event)
    (let ((s (parse:name node))
          (c (tui:event-kind event)))
      (cond ((graphic-char-p c)
             (swap-node location
                        (make-instance 'parse:binder :name (format nil "~a~a" s c))
                        ui)
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (swap-node location
                              (make-instance 'parse:binder
                                             :name (string-drop s 1))
                              ui)
                   (swap-node location (hole) ui)))
             t)))))

(defmethod render-node ((node parse:binder) stack context rect)
  (let* ((location (car stack))
         (str (string-downcase (parse:name node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (tui:make-style :bg (when focused #xb58900) :italicp t))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

;;; list
(defun flat-list-renderer (list loc-mapper stack context)
  (let ((index 0)
        (pad nil))
    (lambda (rect)
      (when list
        (if pad
            (progn
              (setf pad nil)
              (make-instance 'tui:view :rect (tui:copy-rect rect :rows 1 :cols 1)))
            (multiple-value-prog1
                (render-node (car list)
                             (cons (funcall loc-mapper index) stack)
                             context rect)
              (setf list (cdr list)
                    index (+ 1 index)
                    pad t)))))))

;; simple list
(defmethod render-node ((l list) stack context rect)
  (let* ((location (car stack))
         (view
           (tui:horizontal-container
            rect
            (flat-list-renderer l
                                (lambda (index)
                                  (make-location :node (location-node location)
                                                 :id `(,@(location-id location) ,index)))
                                (cdr stack) context))))
    ;;
    (setf (tui:key-handler view) (global-key-handler l location context)
          (tui:focused view) (location= location (focus context)))
    view))

;;; function call
(defvar *fun-key-handlers* (make-hash-table :test 'equal))

(defmethod handle-key ((node parse:function-call) view location ui event)
  (when-let ((handler (gethash event *fun-key-handlers*)))
    (funcall handler location ui)))

(setf (gethash (tui-sys:make-event :kind #\newline) *fun-key-handlers*)
      (lambda (location ui)
        (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
          ;; whole node selected, handle further up
          (unless (zerop i)
            (let* ((stack (nthcdr (1- i) (slog (stack ui))))
                   (id (location-id (car stack))))
              (save-history ui)
              (setf (focus ui)
                    (ast-insert (hole)
                                (make-location :node (location-node (car stack))
                                               :id 'parse:body)
                                (if (integerp id) (1+ id) 0)
                                (stack ui))))))))

(defun list-renderer (list loc-mapper stack context)
  (let ((index 0))
    (lambda (rect)
      (when list
        (multiple-value-prog1
            (render-node (car list)
                         (cons (funcall loc-mapper index) stack)
                         context rect)
          (setf list (cdr list)
                index (+ 1 index)))))))

;; note: assumes length 1 symbol mapping for <= and >=
(defparameter *arb-arity-binops* #("*" "+" "-" "<" ">" "<=" ">=" "=" "/="))
(define-constant +top-left+ (name-char "U1CE16") :test #'equal)
(define-constant +bot-left+ (name-char "U1CE17") :test #'equal)
(define-constant +top-right+ (name-char "U1CE18") :test #'equal)
(define-constant +bot-right+ (name-char "U1CE19") :test #'equal)
;; - unary operators have space removed
;; - for 2 arguments, draw *arb-arity-binops* (idk about higher arity args duping ops)
;; - for 2 arg division and TODO (f)floor/ceiling/truncate, draw horizontally
(defmethod render-node ((node parse:function-call) stack context rect)
  (cond
    ((and (typep (parse:name node) 'parse:symbol-ref)
          (string= (parse:name (parse:name node)) "/")
          (= 2 (length (parse:body node))))
     (let* ((location (car stack))
            (arg1-view (render-node (first (parse:body node))
                                    (cons (make-location :node node :id 0) stack)
                                    context
                                    rect))
            (arg1-rect (tui:rect arg1-view))
            (arg2-view (render-node
                        (second (parse:body node))
                        (cons (make-location :node node :id 1) stack)
                        context
                        (tui:clamp-rect (tui:copy-rect rect :y (1+ (tui:rect-y2 arg1-rect)))
                                        rect)))
            (arg2-rect (tui:rect arg2-view))
            (width (1+ (max (tui:rect-cols arg1-rect) (tui:rect-cols arg2-rect))))
            (div-loc (make-location :node node :id 'parse:name))
            (line-view
              (make-instance 'ast-view
                             :location div-loc
                             :hoverable t
                             :rect (tui:copy-rect rect :rows 1 :cols width
                                                       :y (tui:rect-y2 arg1-rect))
                             :key-handler (global-key-handler (parse:name node)
                                                              div-loc context)
                             :focused (location= div-loc (focus context)))))
       (setf (gethash (parse:name node) (node-views context)) line-view)
       (when (location= div-loc (focus context))
         (setf (stack context) (cons div-loc stack))
         (setf (focus-rect context) (tui:rect line-view)))
       (tui:puts (make-string width :initial-element #\─)
                 (1+ (tui:rect-rows arg1-rect)) 1 rect)
       ;;
       (make-instance 'ast-view
                      :location location
                      :rect (tui:copy-rect rect :cols width
                                                :rows (+ (tui:rect-rows arg1-rect)
                                                         1 (tui:rect-rows arg2-rect)))
                      :children (list arg1-view arg2-view line-view)
                      :key-handler (global-key-handler node location context)
                      :focused (location= location (focus context)))))
    ;; need wrapper for args
    ((and (typep (parse:name node) 'parse:symbol-ref)
          (find (parse:name (parse:name node)) *arb-arity-binops* :test #'string=)
          (= 2 (length (parse:body node))))
     (labels ((is-call-to (node f)
                (and (typep node 'parse:function-call)
                     (typep (parse:name node) 'parse:symbol-ref)
                     (string= f (parse:name (parse:name node)))))
              (is-bracketed (arg)
                (and (is-call-to node "*")
                     (or (is-call-to arg "+") (is-call-to arg "-")))))
       (let* ((location (car stack))
              (arg1 (first (parse:body node)))
              (arg1-bracketed (is-bracketed arg1))
              (arg1-view
                (if arg1-bracketed
                    (render-node arg1 (cons (make-location :node node :id 0) stack)
                                 context (tui:clamp-rect
                                          (tui:copy-rect rect :x (1+ (tui:rect-x rect)))
                                          rect))
                    (render-node arg1 (cons (make-location :node node :id 0) stack)
                                 context rect)))
              (arg1-rect (tui:rect arg1-view))
              (op-view
                (render-node (parse:name node)
                             (cons (make-location :node node :id 'parse:name) stack)
                             context (tui:clamp-rect
                                      (tui:copy-rect rect :x (1+ (tui:rect-x2 arg1-rect)))
                                      rect)))
              (op-rect (tui:rect op-view))
              (arg2 (second (parse:body node)))
              (arg2-bracketed (is-bracketed arg2))
              (arg2-view
                (render-node arg2 (cons (make-location :node node :id 1) stack)
                             context (tui:clamp-rect
                                      (tui:copy-rect rect :x (1+ (tui:rect-x2 op-rect)))
                                      rect)))
              (arg2-rect (tui:rect arg2-view)))
         ;; draw parens
         (when arg1-bracketed
           (let ((rcol (+ 2 (tui:rect-cols arg1-rect))))
             (if (= 1 (tui:rect-rows arg1-rect))
                 (progn
                   (tui:put #\( 1 1 rect)
                   (tui:put #\) 1 rcol rect))
                 (progn
                   (tui:put #\⎛ 1 1 rect)
                   (tui:put #\⎝ (tui:rect-rows arg1-rect) 1 rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put #\⎜ y 1 rect))
                   (tui:put #\⎞ 1 rcol rect)
                   (tui:put #\⎠ (tui:rect-rows arg1-rect) rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put #\⎟ y rcol rect))))))
         (when arg2-bracketed
           (let ((lcol (- (tui:rect-x arg2-rect) (tui:rect-x rect)))
                 (rcol (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                          (1+ (tui:rect-cols arg2-rect)))))
             (if (= 1 (tui:rect-rows arg2-rect))
                 (progn
                   (tui:put #\( 1 lcol rect)
                   (tui:put #\) 1 (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                     (1+ (tui:rect-cols arg2-rect)))
                            rect))
                 (progn
                   (tui:put #\⎛ 1 lcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put #\⎜ y (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                     rect))
                   (tui:put #\⎝ (tui:rect-rows arg2-rect) lcol rect)
                   (tui:put #\⎞ 1 rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put #\⎟ y rcol rect))
                   (tui:put #\⎠ (tui:rect-rows arg2-rect) rcol rect)))))
         (make-instance 'ast-view
                        :location location
                        :rect (tui:copy-rect rect
                                             :rows (max (tui:rect-rows arg1-rect)
                                                        (tui:rect-rows arg2-rect))
                                             :cols (+ (tui:rect-cols arg1-rect)
                                                      (if arg1-bracketed 1 0)
                                                      2 (tui:rect-cols op-rect)
                                                      (tui:rect-cols arg2-rect)
                                                      (if arg2-bracketed 1 0)))
                        :children (list arg1-view arg2-view op-view)
                        :key-handler (global-key-handler node location context)
                        :focused (location= location (focus context))))))
    (t
     (let* ((location (car stack))
            (name-view (render-node (parse:name node)
                                    (cons (make-location :node node :id 'parse:name) stack)
                                    context rect))
            (name-rect (tui:rect name-view))
            (args-view (tui:vertical-container
                        (tui:clamp-rect (tui:copy-rect rect :x (+ 1 (tui:rect-x2 name-rect)))
                                        rect)
                        (list-renderer (parse:body node)
                                       (lambda (i) (make-location :node node :id i))
                                       stack context)))
            (args-rect (tui:rect args-view))
            (fun-rect (tui:clamp-rect
                       (tui:copy-rect rect :rows (max 1 (tui:rect-rows args-rect))
                                           :cols (+ (tui:rect-cols name-rect)
                                                    1 (tui:rect-cols args-rect)))
                       rect)))
       (make-instance 'ast-view
                      :location location
                      :rect fun-rect
                      :children (list name-view args-view)
                      :key-handler (global-key-handler node location context)
                      :focused (location= location (focus context)))))))

;;; let form
(defvar *let-key-handlers* (make-hash-table :test 'equal))
(defmethod handle-key ((node parse:let*-form) view location ui event)
  (when-let ((handler (gethash event *let-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (slog (stack ui))) ui)))))

(setf
 (gethash (tui-sys:make-event :kind #\rubout) *let-key-handlers*)
 (lambda (stack ui)
   (let* ((letloc (car stack))
          (letnode (location-node letloc))
          (vars-loc (make-location :node letnode :id 'parse:vars)))
     (trivia:match (slog (location-id letloc))
       ((list (eql 'parse:vars) (and (type integer) bi))
        (if (= 1 (length (getloc vars-loc)))
            (swap-node vars-loc (list `(,(hole) ,(hole))) ui)
            (progn
              (save-history ui)
              (setf (focus ui) (ast-delete vars-loc bi stack)))))
       ((eql 'parse:vars)
        (save-history ui)
        (swap-node vars-loc (list `(,(hole) ,(hole))) ui))))))

(setf
 (gethash (tui-sys:make-event :kind #\newline) *let-key-handlers*)
 (lambda (stack ui)
   (let* ((letloc (car stack))
          (letnode (location-node letloc)))
     (save-history ui)
     (setf (focus ui)
           (trivia:cmatch (slog (location-id letloc))
             ((and (type integer) body-i)
              (ast-insert (hole)
                          (make-location :node letnode :id 'parse:body)
                          (1+ body-i) (stack ui)))
             ((eql 'parse:vars)
              (ast-insert (hole)
                          (make-location :node letnode :id 'parse:body)
                          0 (stack ui)))
             ((eql 'parse:op)
              (let ((bindloc
                      (ast-insert `(,(hole) ,(hole))
                                  (make-location :node letnode :id 'parse:vars)
                                  0 (stack ui))))
                (make-location :node (location-node bindloc)
                               :id `(,@(location-id bindloc) 0))))
             ((list (eql 'parse:vars) (and (type integer) bi))
              (let ((bindloc
                      (ast-insert `(,(hole) ,(hole))
                                  (make-location :node letnode :id 'parse:vars)
                                  (1+ bi) (stack ui))))
                (make-location :node (location-node bindloc)
                               :id `(,@(location-id bindloc) 0))))
             ;; structural editing within binding
             ((list (eql 'parse:vars) (and (type integer) bi) (and (type integer) i))
              (ast-insert (hole)
                          (make-location :node letnode :id `(parse:vars ,bi))
                          (1+ i) (stack ui))))))))

(defstruct bindings (list))
(defmethod unwrap ((node bindings)) (bindings-list node))
(defmethod render-node ((l bindings) stack context rect)
  (let* ((location (car stack))
         (view
           (tui:vertical-container
            rect
            (list-renderer (bindings-list l)
                           (lambda (index)
                             (make-location :node (location-node location)
                                            :id `(,(location-id location) ,index)))
                           (cdr stack) context))))
    ;;
    (setf (tui:key-handler view) (global-key-handler l location context)
          (tui:focused view) (location= location (focus context)))
    view))

(defparameter *let-indent* 2)
(defmethod render-node ((letnode parse:let*-form) stack context rect)
  (let* ((op "let*")
         (location (car stack))
         (bindings
           (render-node
            (make-bindings :list (parse:vars letnode))
            (cons (make-location :node letnode :id 'parse:vars) stack)
            context
            (tui:clamp-rect (tui:copy-rect rect :x (+ (tui:rect-x rect) (length op) 1))
                            rect)))
         (bind-rect (tui:rect bindings))
         (body-view
           (tui:vertical-container
            (tui:clamp-rect (tui:copy-rect rect :x (+ *let-indent* (tui:rect-x rect))
                                                :y (tui:rect-y2 bind-rect))
                            rect)
            (list-renderer (parse:body letnode)
                           (lambda (id) (make-location :node letnode :id id))
                           stack context)))
         (body-rect (tui:rect body-view))
         (let-op-loc (make-location :node letnode :id 'parse:op))
         (let-op-focused (location= let-op-loc (focus context)))
         (let-view
           (make-instance 'ast-view
                          :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width op))
                          :location let-op-loc
                          :hoverable t
                          :key-handler (when let-op-focused
                                         (global-key-handler 'let* let-op-loc context))
                          :focused let-op-focused)))
    (when let-op-focused
      (setf (stack context) (cons (make-location :node letnode :id 'parse:op) stack))
      (setf (focus-rect context) (tui:rect let-view)))
    (tui:puts op 1 1 rect)
    (make-instance 'ast-view
                   :location location
                   :children (list let-view bindings body-view)
                   :rect (tui:copy-rect rect
                                        :rows (+ (tui:rect-rows bind-rect)
                                                 (tui:rect-rows body-rect))
                                        :cols (max (+ (length op)
                                                      1 (tui:rect-cols bind-rect))
                                                   (+ 2 (tui:rect-cols body-rect))))
                   :key-handler (global-key-handler letnode location context)
                   :focused (location= location (focus context)))))

;;; completions

(defun render-completion (name index state rect)
  (if (plusp (tui:rect-rows rect))
      (let ((prefix (parse:name (anchor state))))
        (flet ((style (bold)
                 (tui:make-style :fg #xeeeeee
                                 :bg (when (eql index (selection state)) #x2aa198)
                                 :boldp bold)))
          (tui:puts (string-downcase prefix) 1 1 rect (style t))
          (tui:puts (string-downcase
                     (nth-value 1 (starts-with-subseq prefix name :return-suffix t)))
                    1 (+ 1 (length prefix)) rect (style nil)))
        (make-instance 'tui:view
                       :rect (tui:copy-rect rect :rows 1
                                                 :cols (tui:display-width name))))
      (make-instance 'tui:view :rect (tui:copy-rect rect :rows 0))))

(defun render-completion-window (ui)
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
       (let ((remaining (candidates state))
             (index 0))
         (lambda (rect)
           (when remaining
             (multiple-value-prog1 (render-completion (car remaining) index state rect)
               (setf remaining (cdr remaining)
                     index (+ 1 index))))))))))

;;
;;; main loop
;;

(defmethod tui:render ((ui ui))
  ;; do not allow use of old caches
  (clrhash (node-views ui))
  (setf (stack ui) nil)
  ;; note: order of rendering here matters
  (tui:fill-rect (tui:make-style :bg #x0) (ui-rect ui) (ui-rect ui))
  (let ((toplevel-views
          (list (render-node (ast ui) (list (make-location :node ui)) ui (ui-rect ui))
                (render-completion-window ui))))
    ;; draw focused node
    (let ((focus-rect (focus-rect ui)))
      (slog (focus ui))
      (tui:fill-rect (tui:make-style :bg #xb58900)
                     (tui:copy-rect focus-rect :x 0 :y 0) focus-rect
                     :blend t))
    (make-instance
     'tui:view
     :rect (ui-rect ui)
     :children (delete nil toplevel-views))))

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
(defvar *state*)
(defun tui-main ()
  (let* ((ast (parse:parse
               '(let* ((aaa (parse::*literal-magic* "3"))
                       (bbb (/ aaa aaa)))
                 (1- b))
               (parse:make-env)))
         (root-loc (make-location :node 'undefined))
         (tui (make-instance 'ui :ast ast :focus root-loc)))
    (setf *state* tui)
    (setf (location-node root-loc) tui)
    ;; set default background to black (xterm extension)
    (format *terminal-io* "~c]11;#000000~c" #\esc (code-char 7))
    (unwind-protect
         (tui:run tui :redisplay-on-input t)
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

(setf (gethash (tui-sys:make-event :kind #\newline) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let* ((state (completion-state ui))
                    (selection (nth (selection state) (candidates state))))
          (let ((s (find-symbol selection)))
            (cond ((eq :function (cl-environments:function-information s))
                   (let* ((args (loop for a in (slynk-backend:arglist s)
                                      while (not (find a lambda-list-keywords))
                                      count a))
                          (name-node (make-instance 'parse:symbol-ref :name s)))
                     ;; only do this in the body
                     (cond ((integerp (location-id (focus ui)))
                            (let ((function-node
                                    (make-instance 'parse:function-call
                                                   :name name-node
                                                   :body (or (loop repeat args
                                                                   collect (hole))
                                                             (list (hole))))))
                              (swap-node (focus ui) function-node ui)
                              (setf (focus ui)
                                    (make-location :node function-node
                                                   :id (if (plusp args) 0 'parse:name)))))
                           ((eq (parse:lockind (focus ui)) 'parse:symbol-ref)
                            (swap-node (focus ui) name-node ui)))))
                  ((eq s 'cl:let*)
                   (swap-node (focus ui)
                              (make-instance 'parse:let*-form
                                             :body (list (hole))
                                             :decls ()
                                             :vars (list `(,(hole) ,(hole))))
                              ui))
                  (t
                   (swap-node (focus ui)
                              (make-instance 'parse:symbol-ref :name s)
                              ui))))
          (setf (completion-state ui) nil)
          t)))

(defmethod move-back ((node parse:function-call) id)
  (trivia:cmatch id
    ((and (type integer) i) (if (< 0 i)
                                (1- i)
                                'parse:name))
    ((eql 'parse:name) 'parse:name)))

(defmethod move-back ((node parse:let*-form) id)
  (trivia:cmatch id
    ((eql 'parse:op) 'parse:op)
    ((eql 'parse:vars) 'parse:op)
    ((and (type integer) i) (if (< 0 i)
                                (1- i)
                                'parse:vars))
    ((list (eql 'parse:vars) (and (type integer) bi))
     (if (< 0 bi)
         (list 'parse:vars (1- bi))
         'parse:op))
    ((list (eql 'parse:vars) (and (type integer) bi) (and (type integer) n))
     (list 'parse:vars bi (max 0 (1- n))))))

;; convert back to hole
(setf (gethash (tui-sys:make-event :kind #\rubout) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let ((focused (getloc (focus ui))))
          (cond ((typep focused 'parse:eval-form)
                 (swap-node (focus ui) (hole) ui))
                ;; note: this is not precise
                ((and (eq 'parse:eval-form (lockind (focus ui)))
                      (typep focused 'hole))
                 (let* ((location (focus ui))
                        (id (location-id location))
                        (parent (location-node location)))
                   ;; by default only delete children in some body of a form
                   (when (or (integerp id) (and (listp id) (integerp (lastcar id))))
                     (let ((body-loc (make-location :node parent :id (parent-id id))))
                       (if (< 1 (length (getloc body-loc)))
                           (setf (focus ui)
                                 (ast-delete body-loc (id-index id) (stack ui)))
                           (let ((child-loc
                                   (ast-replace body-loc
                                                (lambda (body) (list-remove body (id-index id)))
                                                (stack ui))))
                             (setf (focus ui)
                                   (make-location :node (location-node child-loc)
                                                  :id (move-back (location-node child-loc)
                                                                 id)))))
                       t))))))))

;; slurp
(setf (gethash (tui-sys:make-event :kind #\) :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (parent-stack (parent-stack (stack ui)))
          (let* ((loc (focus ui))
                 (node (getloc loc))
                 (id (location-id loc))
                 (i (id-index id))
                 (parent (getloc (car parent-stack)))
                 (pbody (with-lookup (body (parse:get-body parent) parent)
                          body)))
            ;; i *think* these are sufficient
            (when (and (nth-value 1 (parse:get-body node))
                       (listp pbody) (< (1+ i) (length pbody))
                       (typep (nth (1+ i) pbody) 'parse:eval-form))
              (swap-node
               (make-location :node (location-node loc) :id (parent-id id))
               (list-update (list-remove pbody (1+ i))
                            (update node 'parse:body
                                    `(,@(getloc (make-location :node node :id 'parse:body))
                                      ,(nth (1+ i) pbody)))
                            i)
               ui)
              (setf (focus ui)
                    (make-location :node (location-node (focus ui))
                                   :id (append-id (location-id (focus ui)) i))))))))

;; raise
(setf (gethash (tui-sys:make-event :kind #\} :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let ((focus-node (getloc (focus ui))))
          (when (and (< 1 (length (stack ui)))
                     (typep focus-node 'parse:eval-form))
            (loop for stack = (cdr (stack ui)) then (cdr stack)
                  while stack
                  for loc = (car stack)
                  for node = (getloc loc)
                  until (typep node 'parse:eval-form)
                  finally (swap-node loc focus-node ui))))))

;; ;; barf
;; (setf (gethash (tui-sys:make-event :kind #\} :altp t) *default-key-handlers*)
;;       (lambda (view ui)
;;         (declare (ignore view))
;;         (when (typep (getloc (focus ui)) 'parse:eval-form)
;;           (swap-node (focus ui) (hole) ui))))
