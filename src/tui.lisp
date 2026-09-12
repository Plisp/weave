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
                #:locsort #:getloc #:update
                #:append-id #:parent-id #:bodylike-id #:id-index
                #:ref-list #:elements #:hole #:text)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed)
                    (#:tui-sys #:uncursed-sys))
  (:export))
(in-package #:weave-tui)

;; dynamic vars for debugging, should not be used
(defvar *state*)
(defvar *log* (sb-concurrency:make-mailbox :name "log"))
(defvar *log-stop* (gensym))
(defmacro slog (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons ',form ,res))
       ,res)))
(defmacro slog* (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons nil ,res))
       ,res)))

(defclass ast-view (tui:view)
  ((location :initarg :location
             :initform (error "ast view must correspond to a location")
             :accessor location
             :type location)
   (view-stack :initform nil
               :accessor view-stack
               :type list)
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
   ;; the focus is (car stack)
   (stack :initarg :stack
          :initform (error "must provide insertion stack")
          :reader stack)))

(defclass selection ()
  (;; tracks the stack during selection-mode, for growing and shrinking zippers
   (context :initarg :context
            :initform (error "no context")
            :accessor selection-context)
   ;; the location of the child where the selection was started
   (location :initarg :location
             :initform (error "no location")
             :reader selection-location)
   (point :initarg :point
          :initform (error "no point")
          :accessor selection-point
          :type integer)
   (activep :initform t
            :accessor selection-activep)))

(defclass zipper (selection)
  ;; from the selection start location out to the top of the zipper
  ((stack :initarg :stack
          :initform (error "no stack")
          :reader zipper-stack
          :type list)))

(defun end-selection-mode (ui)
  (when-let (selection (selection ui))
    (setf (selection-activep selection) nil)))

(defun selection-anchor (selection)
  (id-index (location-id (selection-location selection))))

(defun selection-range (selection)
  (values (min (selection-anchor selection) (selection-point selection))
          (max (selection-anchor selection) (selection-point selection))))

(defun selection-parent-id (selection)
  (parent-id (location-id (selection-location selection))))

(defun selection-forms (selection)
  (let ((loc (selection-location selection)))
    (assert (bodylike-id (location-id loc)))
    (multiple-value-bind (low high)
        (selection-range selection)
      (subseq (elements (parse:get-location (location-node loc)
                                            (selection-parent-id selection)))
              low (1+ high)))))

(defun zipper-top-p (ui stack)
  (when-let (selection (selection ui))
    (and (typep selection 'zipper)
         (eq (getloc (car stack)) ; top of zipper
             (location-node (lastcar (zipper-stack selection)))))))

(defun zipper-depth (zipper)
  (length (zipper-stack zipper)))

(defun location-selected-p (ui stack)
  (when-let* ((selection (selection ui))
              (id (location-id (car stack))))
    (and (eq (location-node (selection-location selection))
             (location-node (car stack)))
         (bodylike-id id)
         (equal (selection-parent-id selection) (parent-id id))
         (multiple-value-bind (low high)
             (selection-range selection)
           (<= low (id-index id) high)))))

(defun selection-at-focus (ui)
  (when (location-selected-p ui (stack ui))
    (selection ui)))

(defclass cutbuffer ()
  ((content :initarg :content
            :initform (error "no content")
            :reader content)
   (location :initarg :location
             :initform (error "no location")
             :reader location)))

(defclass ui (tui:elemental)
  ((ast :initarg :ast ; this slot exists to cache the back of the stack (root)
        :initform (error "no ast")
        :accessor ast)
   (edit-loc :initarg :edit-loc
             :initform nil
             :accessor edit-loc)
   (cutbuffer :initform nil
              :accessor cutbuffer)
   (goal-col :initform 1
             :accessor goal-col
             :type positive-fixnum)
   (completion-state :initform nil
                     :accessor completion-state
                     :type (or null completion-state))
   (selection :initform nil
              :accessor selection
              :type (or null selection))
   (zipper :initform nil
           :accessor zipper
           :type (or null zipper))
   (history :initform (list)
            :accessor history
            :type list)
   (future :initform (list)
           :accessor future
           :type list)
   ;; maintained by every edit and cursor move
   (stack :initarg :stack
          :accessor stack ; stack is always non-empty
          :type list)
   ;; recomputed on every redisplay
   (binder-cache :initform nil
                 :accessor binder-cache
                 :type list)
   (focus-rect :initform nil
               :accessor focus-rect
               :type (or null tui:rect))
   (node-views :initform (make-hash-table)
               :accessor node-views)))

(defun ui-rect (ui)
  (tui:make-rect :x 0 :y 0 :rows (tui:rows ui) :cols (tui:cols ui)))

(defun focus (ui)
  (car (stack ui)))

;; no update
(defmethod parse:get-location ((node ui) id)
  (declare (ignore id))
  (ast node))

(defmethod parse:location-sort ((ui ui) id)
  (declare (ignore id))
  'parse:eval-form)

;;
;;; location/stack
;;
;; - a stack tracks locations from the focus outward to the root, ending in `ui'
;; - ast-delete/insert/replace are stack-respecting edit operations
;;
(defun rebuild-spine (loc updater stack)
  "Requires that (location-node `loc') = (location-node (car `stack'))
Functionally rebuilds the path from `loc' out to the ui or root, applying `updater' to
the value at `loc', but retaining the current focus. Returns the new stack and root."
  (assert (eq (location-node loc) (location-node (car stack))))
  (labels ((rebuild (stack newnode newstack)
             (if (null stack)
                 (values (reverse newstack) newnode)
                 (let ((this (car stack)))
                   (if (typep (location-node this) 'ui) ; no need to update ui
                       (values (reverse (cons this newstack)) newnode)
                       (let* ((id (location-id this))
                              (new-parent (update (location-node this) id newnode)))
                         (rebuild (cdr stack)
                                  new-parent
                                  (cons (make-location :node new-parent :id id)
                                        newstack))))))))
    (rebuild (cons loc (cdr stack))
             (funcall updater (getloc loc))
             (list))))

(defun commit-edit (ui stack root)
  "Installs a `rebuild-spine' result."
  (end-selection-mode ui)
  (setf (ast ui) root
        (stack ui) stack)
  (focus ui))

(defun refocus (ui id)
  "Moves the focus to another location in the node it is already within."
  (end-selection-mode ui)
  (let ((loc (make-location :node (location-node (focus ui)) :id id)))
    (setf (stack ui) (cons loc (cdr (stack ui))))
    loc))

(defun descend (ui id)
  "Moves the focus to a location-id inside the currently focused node."
  (end-selection-mode ui)
  (let ((loc (make-location :node (getloc (focus ui)) :id id)))
    (push loc (stack ui))
    loc))

(defun ast-replace (loc updater ui &optional (stack (stack ui)))
  (multiple-value-call #'commit-edit ui (rebuild-spine loc updater stack)))

(defun ast-insert (item loc index ui &optional (stack (stack ui)))
  (multiple-value-bind (new-stack root)
      (rebuild-spine loc
                     (lambda (body) (list-insert (elements body) item index))
                     stack)
    (commit-edit ui new-stack root)
    (refocus ui (append-id (location-id loc) index))))

(defun ast-delete (loc index ui &optional (stack (stack ui)))
  "assumes that list has length > 1"
  (multiple-value-bind (new-stack root)
      (rebuild-spine loc (lambda (body) (list-remove (elements body) index)) stack)
    (commit-edit ui new-stack root)
    (refocus ui (append-id (location-id loc) (max 0 (1- index))))))

(defun parent-stack (stack)
  "relies on ids being either 'symbol or ('symbol integer*) for list addressing."
  (let* ((loc (car stack))
         (id (location-id loc)))
    (cond ((and (listp id) (eq (first id) 'parse:body))
           (cdr stack))
          ((listp id)
           (cons (make-location :node (location-node loc) :id (parent-id id))
                 (cdr stack)))
          (t (cdr stack)))))

(defun save-history (ui)
  (push (make-instance 'state :stack (stack ui) :ast (ast ui))
        (history ui)))

(defun swap-node (location newnode ui)
  "saves undo history"
  (when (or (null (edit-loc ui)) (not (location= location (edit-loc ui))))
    (save-history ui))
  ;; perform the insertion
  (let ((newloc (ast-replace location (constantly newnode) ui
                             (member-if (lambda (l) (eq (location-node location)
                                                   (location-node l)))
                                        (stack ui)))))
    (setf (future ui) nil
          (edit-loc ui) newloc)))

;;
;;; ast classes
;;

(defgeneric render-node (node stack context rect &key &allow-other-keys))

(defun bindings-at (loc)
  "What the location-node of `loc' binds at the location, nothing if unevaluated."
  (when (member (locsort loc) '(parse:eval-form parse:function-code))
    (parse:location-bindings (location-node loc) (location-id loc))))

(defun function-position-p (loc)
  "Whether a symbol-ref at `loc' names a function rather than a variable."
  (let ((node (location-node loc))
        (id (location-id loc)))
    (or (and (typep node 'parse:function-call) (eq id 'parse:name))
        (and (typep node 'parse:function-form) (eq id 'parse:fun-designator)))))

(defun compute-focus-binder (ui)
  (let ((node (getloc (focus ui))))
    (when (and (typep node 'parse:symbol-ref) (not (typep node 'parse:binder)))
      (let ((kind (if (function-position-p (focus ui)) :function :variable))
            (name (parse:name node)))
        (loop for loc in (stack ui)
              do (loop for (k . binder) in (bindings-at loc)
                       do (when (and (eq k kind)
                                     (typep binder 'parse:binder)
                                     (string= name (parse:name binder)))
                            (return-from compute-focus-binder binder))))))))

(defun focus-binder (ui)
  "The binder the symbol-ref under the cursor refers to, innermost binding first."
  (let ((cache (binder-cache ui)))
    (if (eq (car cache) (stack ui))
        (cdr cache)
        (cdr (setf (binder-cache ui)
                   (cons (stack ui) (compute-focus-binder ui)))))))

(defmethod render-node :around (node stack context rect &key)
  (let* ((vals (multiple-value-list (call-next-method)))
         (view (first vals))
         (rect (tui:rect view)))
    ;; inverse linear scaling, drawing after children isn't ideal
    (let ((i (truncate (- 255 (/ 255 (1+ (/ (expt (length stack) 2) 8)))))))
      (unless (parse:is-atom node)
        (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                       (tui:copy-rect rect :x 0 :y 0) rect
                       :blend 0.1)))
    (when (eq node (focus-binder context))
      (tui:fill-rect (tui:make-style :bg (tui:color #x85 #x99 #x00))
                     (tui:copy-rect rect :x 0 :y 0) rect
                     :blend 0.4))
    (when (location-selected-p context stack)
      (tui:fill-rect (tui:make-style :bg (tui:color #x6c #x71 #xc4))
                     (tui:copy-rect rect :x 0 :y 0) rect
                     :blend 0.4))
    (when (zipper-top-p context stack)
      (dolist (form (selection-forms (selection context)))
        (when-let (view (gethash form (node-views context)))
          (let ((form-rect (tui:rect view)))
            (tui:fill-rect (tui:make-style :bg (tui:color #x6c #x71 #xc4))
                           (tui:copy-rect form-rect :x 0 :y 0) form-rect
                           :blend 0.4))))
      (tui:fill-rect (tui:make-style :bg (tui:color #xb5 #x89 #x00))
                     (tui:copy-rect rect :x 0 :y 0) rect
                     :blend 0.2))
    ;; save window
    (setf (gethash node (node-views context)) view)
    (when (typep view 'ast-view)
      (setf (view-stack view) stack))
    (when (location= (car stack) (focus context))
      (setf (focus-rect context) rect))
    (values-list vals)))

(defparameter *default-key-handlers* (make-hash-table :test 'equal))
(defparameter *global-key-handlers* (make-hash-table :test 'equal))
(defun global-key-handler (node location ui)
  "handler may return t to stop propagation up the stack"
  (lambda (view event)
    (slog event)
    (assert (location= location (focus ui)))
    (or (when-let (handler (gethash event *global-key-handlers*))
          (funcall handler view ui))
        ;; propagate
        (loop for thisnode = node then (location-node location)
              for location in (slog (stack ui))
              thereis (when (handle-key thisnode view location ui event)
                        (slog* `(handled-at ,node))
                        t))
        ;; contextual handlers
        (when-let (handler (gethash event *default-key-handlers*))
          (slog* "default handler called")
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
                    (setf candidates valid)))))))))

(defun fname (node)
  (parse:name (parse:name node)))

;; note: assumes length 1 symbol mapping for <= and >=
(defparameter *arb-arity-binops* #("/" "*" "+" "-" "<" ">" "<=" ">=" "=" "/="))
(defun binop-precedence (op)
  (trivia:ematch (string op)
    ((or "*") 3)
    ((or "/" "+" "-") 2)
    ((or "<" ">" "<=" ">=" "=" "/=") 1)))

(defun is-binop-call (node)
  (and (typep node 'parse:function-call)
       (find (fname node) *arb-arity-binops* :test #'string=)))

(defun wrap-arith (op ui)
  "take first arithmetic operator with greater precedence than `op'
if none, surround current atom"
  (when (eq (locsort (car (stack ui))) 'parse:eval-form)
    (loop for stack = (stack ui) then (cdr stack)
          for loc in (stack ui)
          for node = (location-node loc)
          while (and (is-binop-call node)
                     (> (binop-precedence (fname node)) (binop-precedence op)))
          finally (let ((sym (make-instance 'parse:symbol-ref
                                            :name (symbol-name op)
                                            :home-package (symbol-package op)))
                        (loc (car stack)))
                    (swap-node loc
                               (make-instance 'parse:function-call
                                              :name sym
                                              :body (list (getloc loc) (hole)))
                               ui)
                    (descend ui '(parse:body 1)))
                  (return t))))

(loop for s across *arb-arity-binops*
      do (when (= 1 (length s))
           (setf (gethash (tui-sys:make-event :kind (schar s 0) :altp t)
                          *global-key-handlers*)
                 ;; note: bug if we don't compute from s prior to its mutation by loop
                 (let ((sym (intern s)))
                   (lambda (view ui)
                     (declare (ignore view))
                     (wrap-arith sym ui))))))

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
`col' should essentially indicate some preferred column. Returns NIL if not found."
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
  (end-selection-mode ui)
  (let* ((atom-array (build-atom-array (tui:root-view ui) (tui:rows ui)))
         (this-rect (tui:rect view)))
    (multiple-value-bind (new-view new-goal)
        (funcall view-finder atom-array this-rect)
      (when new-view ; assumes atoms are all ast-views
        (setf (stack ui) (slog (view-stack new-view)))
        (when new-goal
          (slog* (format nil "goal col is ~d" new-goal))
          (setf (goal-col ui) new-goal))))))

(defun move-down (view ui)
  (atom-move (lambda (atom-array this-rect)
               (view-below atom-array (1+ (tui:rect-y this-rect)) (goal-col ui)))
             view ui))

(setf (gethash (tui-sys:make-event :kind #\n :controlp t) *default-key-handlers*)
      #'move-down)
(setf (gethash (tui-sys:make-event :kind :down-arrow) *default-key-handlers*)
      #'move-down)

(defun move-up (view ui)
  (atom-move (lambda (atom-array this-rect)
               (view-above atom-array (tui:rect-y this-rect) (goal-col ui)))
             view ui))

(setf (gethash (tui-sys:make-event :kind #\p :controlp t) *default-key-handlers*)
      #'move-up)
(setf (gethash (tui-sys:make-event :kind :up-arrow) *default-key-handlers*)
      #'move-up)

(defun move-left (view ui)
  (atom-move
   (lambda (atom-array this-rect)
     (let ((view (view-left atom-array
                            (tui:rect-y this-rect) (tui:rect-x this-rect))))
       (values view (when view (tui:rect-x2 (tui:rect view))))))
   view ui))

(setf (gethash (tui-sys:make-event :kind #\b :controlp t) *default-key-handlers*)
      #'move-left)
(setf (gethash (tui-sys:make-event :kind :left-arrow) *default-key-handlers*)
      #'move-left)

(defun move-right (view ui)
  (atom-move
   (lambda (atom-array this-rect)
     (let ((view (view-right atom-array
                             (tui:rect-y this-rect) (tui:rect-x2 this-rect))))
       (values view (when view (tui:rect-x2 (tui:rect view))))))
   view ui))

(setf (gethash (tui-sys:make-event :kind #\f :controlp t) *default-key-handlers*)
      #'move-right)
(setf (gethash (tui-sys:make-event :kind :right-arrow) *default-key-handlers*)
      #'move-right)

(defun selection-list-location (selection)
  (make-location :node (location-node (selection-location selection))
                 :id (selection-parent-id selection)))

(defun current-selection (ui)
  (when-let (selection (selection ui))
    (and (selection-activep selection) selection)))

(defun begin-selection (ui)
  (let* ((loc (focus ui))
         (id (location-id loc)))
    (when (and (bodylike-id id) (eq 'parse:eval-form (locsort loc)))
      (setf (selection ui)
            (make-instance 'selection
                           :context (stack ui)
                           :location loc
                           :point (id-index id))))))

(defun extend-selection (ui delta)
  (when-let (selection (or (current-selection ui) (begin-selection ui)))
    (let ((point (+ (selection-point selection) delta)))
      (when (<= 0 point (1- (length (elements (getloc (selection-list-location selection))))))
        ;; when the underlying range is reselected, forget the zipper
        (when (typep selection 'zipper)
          (change-class selection 'selection))
        (setf (selection-point selection) point)
        ;; keep selection active after move
        (refocus ui (append-id (selection-parent-id selection) point))
        (setf (selection-activep selection) t)))
    t))

(defun eval-list-position-p (location)
  "Whether LOCATION is an element of a list of evaluated forms."
  (let ((id (location-id location)))
    (and (bodylike-id id)
         (eq 'parse:eval-form
             (parse:location-sort (location-node location)
                                  (append-id (parent-id id) 0))))))

(defun zipper-target-p (location)
  "Whether a zipper can top out at `location'."
  (or (eval-list-position-p location)
      (eq 'parse:eval-form (locsort location))))

(defun expand-selection (ui)
  (when-let (selection (or (current-selection ui) (begin-selection ui)))
    (let ((depth (if (typep selection 'zipper) (zipper-depth selection) 0)))
      ;; skip positions where we cannot copy a usefully pastable zipper
      ;; e.g. nonevaluated let bindings need more sophisticated sort tracking
      (loop for location in (nthcdr (1+ depth) (selection-context selection))
            for level from (1+ depth)
            do (when (zipper-target-p location)
                 (let ((stack (subseq (selection-context selection) 0 level)))
                   (if (plusp depth)
                       (reinitialize-instance selection :stack stack)
                       (change-class selection 'zipper :stack stack)))
                 (return t))))))

(defun shrink-selection (ui)
  (when-let (selection (current-selection ui))
    (when (typep selection 'zipper)
      (loop for level from (1- (zipper-depth selection)) downto 1
            for location = (nth level (selection-context selection))
            do (when (zipper-target-p location)
                 (reinitialize-instance
                  selection :stack (subseq (selection-context selection) 0 level))
                 (return t))
            finally ;; normal selection
                    (change-class selection 'selection)
                    (return t)))))

(setf (gethash (tui-sys:make-event :kind :up-arrow :shiftp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (expand-selection ui)))

(setf (gethash (tui-sys:make-event :kind :down-arrow :shiftp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (shrink-selection ui)))

(setf (gethash (tui-sys:make-event :kind :left-arrow :shiftp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (extend-selection ui -1)))

(setf (gethash (tui-sys:make-event :kind :right-arrow :shiftp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (extend-selection ui 1)))

(defun move-parent (ui)
  (end-selection-mode ui)
  (when-let (new-stack (parent-stack (stack ui)))
    (slog* `(moving to ,new-stack))
    (setf (stack ui) new-stack)))

(setf (gethash (tui-sys:make-event :kind #\p :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (move-parent ui)))

(setf (gethash (tui-sys:make-event :kind #\P :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        ;; this is really a do-while idiom
        (prog1 (move-parent ui)
          (loop for last-stack = nil then stack
                for stack = (slog (stack ui))
                for loc = (car stack)
                for node = (getloc loc)
                while (typep node 'parse:eval-form)
                until (eq (location-node loc) ui)
                do (move-parent ui)
                finally (when (slog last-stack)
                          (setf (stack ui) last-stack))))))

;;; undo

;; can't pop the one remaining thing
(defun undo (ui)
  (unless (null (history ui))
    (let ((prev (pop (history ui))))
      (slog 'undo)
      (push (make-instance 'state :ast (ast ui) :stack (stack ui))
            (future ui))
      (setf (ast ui) (ast prev)
            (stack ui) (stack prev)))))

(defun redo (ui)
  (unless (null (future ui))
    (let ((next (pop (future ui))))
      (slog 'redo)
      (push (make-instance 'state :ast (ast ui) :stack (stack ui))
            (history ui))
      (setf (ast ui) (ast next)
            (stack ui) (stack next)))))

(setf (gethash (tui-sys:make-event :kind #\u :controlp t) *global-key-handlers*)
      (lambda (view ui) view (undo ui)))
(setf (gethash (tui-sys:make-event :kind #\r :controlp t) *global-key-handlers*)
      (lambda (view ui) view (redo ui)))

;;; hole
;; note: holes only replace symbols or evaluation contexts

(defmethod render-node ((node hole) stack context rect &key)
  (with-accessors ((text text)) node
    (let* ((location (car stack))
           (text (if (string= text "") "hole" text))
           (focused (location= location (focus context))))
      (tui:puts text 1 1 rect (if focused
                                  (tui:make-style :fg #x0 :underlinep t)
                                  (tui:make-style :fg (tui:color 30 200 0) :underlinep t)))
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

(defun symbol-char-p (c)
  (or (alphanumericp c) (find c "+-*/=<>!?&%$_~^.:@[]{}")))

(defmethod handle-key ((node hole) view location ui event)
  (let ((c (tui:event-kind event)))
    (when (is-regular-char-event event)
      (trivia:match (slog (locsort location))
        ((eql 'parse:binder)
         (when (and (symbol-char-p c) (not (digit-char-p c)))
           (swap-node location (make-instance 'parse:binder :name (string-upcase c)) ui)))
        ((eql 'parse:eval-form)
         (cond
           ((and (symbol-char-p c) (not (digit-char-p c)))
            (let ((newnode (make-instance 'parse:symbol-ref :name (string-upcase c))))
              (swap-node location newnode ui)))
           ((digit-char-p c)
            (let ((newnode (make-instance 'parse:literal :str (string c))))
              (swap-node location newnode ui)))))
        ((eql 'parse:symbol-ref)
         (when (and (symbol-char-p c) (not (digit-char-p c)))
           (let ((newnode (make-instance 'parse:symbol-ref :name (string-upcase c))))
             (swap-node location newnode ui))))
        ((eql 'parse:unevaluated)
         (cond
           ((char= c #\()
            (swap-node location (ref-list (hole)) ui)
            (refocus ui (append-id (location-id (focus ui)) 0)))
           ((and (symbol-char-p c) (not (digit-char-p c)))
            (swap-node location
                       (make-instance 'parse:symbol-ref :name (string-upcase c))
                       ui))
           ((digit-char-p c)
            (swap-node location (make-instance 'parse:literal :str (string c)) ui))))))))

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

(defmethod render-node ((node parse:literal) stack context rect &key)
  (let* ((location (car stack))
         (str (format nil "~a" (parse:str node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (if focused
                               (tui:make-style :fg #x0)
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
      (cond ((symbol-char-p c)
             (let ((newnode
                     (make-instance 'parse:symbol-ref
                                    :name (format nil "~a~a" s (string-upcase c))
                                    :home-package (parse:home-package node))))
               (swap-node location newnode ui)
               (if-let (state (completion-state ui))
                 (setf (anchor state) newnode)
                 (when (< 1 (length s))
                   (setf (completion-state ui)
                         (make-instance 'completion-state
                                        :anchor newnode
                                        :candidates (completion-candidates ui))))))
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (let ((newnode (make-instance 'parse:symbol-ref
                                                 :name (string-drop s 1)
                                                 :home-package (parse:home-package node))))
                     (swap-node location newnode ui)
                     (when (completion-state ui)
                       (setf (completion-state ui)
                             (make-instance 'completion-state
                                            :anchor newnode
                                            :candidates (completion-candidates ui)))))
                   (swap-node location (hole) ui)))
             t)))))

(defparameter *symbol-mappings* '((<= . #\≤) (>= . #\≥) (* . #\⋅) (/= . #\≠)))

(defmethod render-node ((node parse:symbol-ref) stack context rect &key)
  (let* ((location (car stack))
         (name (parse:name node))
         (str (if-let (exp (assoc-value *symbol-mappings* name :test #'string-equal))
                (string exp)
                (string-downcase name)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect)
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
      (cond ((symbol-char-p c)
             (swap-node location
                        (make-instance 'parse:binder
                                       :name (format nil "~a~a" s (string-upcase c))
                                       :home-package (parse:home-package node))
                        ui)
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (swap-node location
                              (make-instance 'parse:binder
                                             :name (string-drop s 1)
                                             :home-package (parse:home-package node))
                              ui)
                   (swap-node location (hole) ui)))
             t)))))

(defmethod render-node ((node parse:binder) stack context rect &key)
  (let* ((location (car stack))
         (str (string-downcase (parse:name node)))
         (focused (location= location (focus context))))
    (tui:puts str 1 1 rect (tui:make-style :italicp t))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location context))
                   :focused focused)))

(defun render-symbol (op loc stack context rect)
  "Renders `loc' as a bare focusable token for `op', a string designator."
  (let* ((text (string-downcase op))
         (focused (location= loc (focus context)))
         (view
           (make-instance 'ast-view
                          :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                          :location loc
                          :hoverable t
                          :key-handler (when focused (global-key-handler op loc context))
                          :focused focused)))
    (tui:puts text 1 1 rect)
    (setf (view-stack view) (cons loc stack))
    (when focused (setf (focus-rect context) (tui:rect view)))
    view))

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

;; a list as written, drawn as the elements it holds
(defmethod render-node ((l parse:ref-list) stack context rect &key (direction :horizontal))
  (let* ((location (car stack))
         (elements
           (funcall (ecase direction
                      (:horizontal #'flat-list-renderer)
                      (:vertical #'list-renderer))
                    (elements l)
                    (lambda (index)
                      (make-location :node (location-node location)
                                     :id (append-id (location-id location) index)))
                    (cdr stack) context))
         (view (ecase direction
                 (:horizontal (tui:horizontal-container rect elements))
                 (:vertical (tui:vertical-container rect elements)))))
    ;;
    (setf (tui:key-handler view) (global-key-handler l location context)
          (tui:focused view) (location= location (focus context)))
    view))

;;; function call
(defparameter *fun-key-handlers* (make-hash-table :test 'equal))

(defmethod handle-key ((node parse:function-call) view location ui event)
  ;; I don't expect function calls to have any special binds percolated up the tree
  (when-let ((handler (gethash event *fun-key-handlers*)))
    (when (and (< 1 (length (stack ui)))
               (location= location (second (stack ui)))) ; we need to be a direct child
      (funcall handler ui))))

(setf (gethash (tui-sys:make-event :kind #\space) *fun-key-handlers*)
      (lambda (ui)
        (let* ((fun-loc (car (stack ui)))
               (fun-node (location-node fun-loc))
               (id (location-id fun-loc))
               (i (if (bodylike-id id) (1+ (id-index id)) 0)))
          (if (= i (length (parse:body fun-node)))
              (progn
                (save-history ui)
                (ast-insert (hole) (make-location :node fun-node :id 'parse:body)
                            i ui))
              (refocus ui (list 'parse:body i))))))

(setf (gethash (tui-sys:make-event :kind #\newline) *fun-key-handlers*)
      (lambda (ui)
        (let* ((fun-loc (car (stack ui)))
               (id (location-id fun-loc)))
          (save-history ui)
          (ast-insert (hole) ; function-call is at top of the stack vvv
                      (make-location :node (location-node fun-loc) :id 'parse:body)
                      (if (bodylike-id id) (1+ (id-index id)) 0)
                      ui))))

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

(define-constant +top-left+ (name-char "U1CE16") :test #'equal)
(define-constant +bot-left+ (name-char "U1CE17") :test #'equal)
(define-constant +top-right+ (name-char "U1CE18") :test #'equal)
(define-constant +bot-right+ (name-char "U1CE19") :test #'equal)
;; - unary operators have space removed
;; - for 2 arguments, draw *arb-arity-binops* infix
;;   - higher arity duplicated op views break invariant: always a single focused view
;; - for 2 arg division, draw horizontally. maybe future: (f)floor/ceiling/truncate
(defmethod render-node ((node parse:function-call) stack context rect &key)
  (cond
    ;; note this case must go first since division is a binop
    ((and (string= (fname node) "/")
          (= 2 (length (parse:body node))))
     (let* ((location (car stack))
            (arg1-view (render-node
                        (first (parse:body node))
                        (cons (make-location :node node :id '(parse:body 0)) stack)
                        context
                        rect))
            (arg1-rect (tui:rect arg1-view))
            (arg2-view (render-node
                        (second (parse:body node))
                        (cons (make-location :node node :id '(parse:body 1)) stack)
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
       (setf (view-stack line-view) (cons div-loc stack))
       (when (location= div-loc (focus context))
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
    ((and (find (fname node) *arb-arity-binops* :test #'string=)
          (= 2 (length (parse:body node))))
     (flet ((is-bracketed (arg)
              (and (is-binop-call node) (is-binop-call arg)
                   (or (> (binop-precedence (fname node)) (binop-precedence (fname arg)))
                       (and (string= (fname node) "-")
                            (or (string= (fname arg) "+")
                                (string= (fname arg) "-")))))))
       (let* ((location (car stack))
              (arg1 (first (parse:body node)))
              (arg1-bracketed (is-bracketed arg1))
              (arg1-view
                (if arg1-bracketed
                    (render-node arg1
                                 (cons (make-location :node node :id '(parse:body 0)) stack)
                                 context (tui:clamp-rect
                                          (tui:copy-rect rect :x (1+ (tui:rect-x rect)))
                                          rect))
                    (render-node arg1
                                 (cons (make-location :node node :id '(parse:body 0)) stack)
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
                (render-node arg2
                             (cons (make-location :node node :id '(parse:body 1)) stack)
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
                        (tui:clamp-rect
                         (tui:copy-rect rect :x (+ 1 (tui:rect-x2 name-rect)))
                         rect)
                        (list-renderer (parse:body node)
                                       (lambda (i) (make-location :node node
                                                             :id (list 'parse:body i)))
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
(defparameter *let-key-handlers* (make-hash-table :test 'equal))
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
        (if (= 1 (length (elements (getloc vars-loc))))
            (swap-node vars-loc (ref-list (ref-list (hole) (hole))) ui)
            (progn
              (save-history ui)
              (ast-delete vars-loc bi ui stack))))
       ((eql 'parse:vars)
        (save-history ui)
        (swap-node vars-loc (ref-list (ref-list (hole) (hole))) ui))))))

(setf
 (gethash (tui-sys:make-event :kind #\newline) *let-key-handlers*)
 (lambda (stack ui)
   (let* ((letloc (car stack))
          (letnode (location-node letloc)))
     (save-history ui)
     (flet ((insert-binding (index)
              ;; focus the binder of the pair just inserted
              (let ((bindloc (ast-insert (ref-list (hole) (hole))
                                         (make-location :node letnode :id 'parse:vars)
                                         index ui stack)))
                (refocus ui `(,@(location-id bindloc) 0)))))
       (trivia:cmatch (slog (location-id letloc))
         ((list (eql 'parse:body) (and (type integer) body-i))
          (ast-insert (hole)
                      (make-location :node letnode :id 'parse:body)
                      (1+ body-i) ui stack))
         ((eql 'parse:vars)
          (ast-insert (hole)
                      (make-location :node letnode :id 'parse:body)
                      0 ui stack))
         ((eql 'parse:op) (insert-binding 0))
         ((list (eql 'parse:vars) (and (type integer) bi)) (insert-binding (1+ bi)))
         ;; structural editing within binding
         ((list (eql 'parse:vars) (and (type integer) bi) (and (type integer) i))
          (ast-insert (hole)
                      (make-location :node letnode :id `(parse:vars ,bi))
                      (1+ i) ui stack)))))))

(defparameter *let-indent* 2)

(defmethod render-node ((letnode parse:let*-form) stack context rect &key)
  (let* ((op 'let*)
         (location (car stack))
         (bindings
           (render-node
            (parse:vars letnode)
            (cons (make-location :node letnode :id 'parse:vars) stack)
            context
            (tui:clamp-rect (tui:copy-rect rect :x (+ (tui:rect-x rect) (length "let*") 1))
                            rect)
            :direction :vertical))
         (bind-rect (tui:rect bindings))
         (body-view
           (tui:vertical-container
            (tui:clamp-rect (tui:copy-rect rect :x (+ *let-indent* (tui:rect-x rect))
                                                :y (tui:rect-y2 bind-rect))
                            rect)
            (list-renderer (parse:body letnode)
                           (lambda (i) (make-location :node letnode :id (list 'parse:body i)))
                           stack context)))
         (body-rect (tui:rect body-view))
         (let-view (render-symbol op (make-location :node letnode :id 'parse:op)
                                 stack context rect)))
    (make-instance 'ast-view
                   :location location
                   :children (list let-view bindings body-view)
                   :rect (tui:copy-rect rect
                                        :rows (+ (tui:rect-rows bind-rect)
                                                 (tui:rect-rows body-rect))
                                        :cols (max (+ (length "let*")
                                                      1 (tui:rect-cols bind-rect))
                                                   (+ 2 (tui:rect-cols body-rect))))
                   :key-handler (global-key-handler letnode location context)
                   :focused (location= location (focus context)))))

;;; lambda list
(defparameter *fun-code-key-handlers* (make-hash-table :test 'equal))

(defmethod handle-key ((node parse:function-code) view location ui event)
  (declare (ignore view))
  (when-let ((handler (gethash event *fun-code-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (stack ui)) ui)))))

(setf
 (gethash (tui-sys:make-event :kind #\newline) *fun-code-key-handlers*)
 (lambda (stack ui)
   (let* ((loc (car stack))
          (node (location-node loc)))
     (flet ((insert-into (id index)
              (save-history ui)
              (ast-insert (hole) (make-location :node node :id id) index ui stack))
            (shorter-than (id limit)
              (< (length (elements (getloc (make-location :node node :id id)))) limit)))
       (trivia:cmatch (location-id loc)
         ((list (eql 'parse:lambda-list) (and (type integer) i))
          (insert-into 'parse:lambda-list (1+ i)))
         ((eql 'parse:lambda-list) (insert-into 'parse:lambda-list 0))
         ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j))
          (when (shorter-than `(parse:lambda-list ,i) 3)
            (insert-into `(parse:lambda-list ,i) (1+ j))))
         ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j)
                (and (type integer) k))
          (when (shorter-than `(parse:lambda-list ,i ,j) 2)
            (insert-into `(parse:lambda-list ,i ,j) (1+ k))))
         ((list (eql 'parse:body) (and (type integer) i))
          (insert-into 'parse:body (1+ i))))))))

(setf
 (gethash (tui-sys:make-event :kind #\() *fun-code-key-handlers*)
 (lambda (stack ui)
   (let ((loc (car stack)))
     (trivia:match (location-id loc)
       ((or (list (eql 'parse:lambda-list) (type integer))
            (list (eql 'parse:lambda-list) (type integer) (type integer)))
        (save-history ui)
        (ast-replace loc (lambda (param) (ref-list param (hole))) ui stack)
        (refocus ui (append-id (location-id loc) 1))
        t)))))

(setf
 (gethash (tui-sys:make-event :kind #\rubout) *fun-code-key-handlers*)
 (lambda (stack ui)
   (let* ((loc (car stack))
          (node (location-node loc))
          (ll-loc (make-location :node node :id 'parse:lambda-list)))
     (trivia:match (location-id loc)
       ((list (eql 'parse:lambda-list) (and (type integer) i))
        (save-history ui)
        (if (= 1 (length (parse:elements (getloc ll-loc))))
            (swap-node ll-loc (parse:ref-list) ui)
            (ast-delete ll-loc i ui stack)))
       ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j))
        (unless (zerop j)
          (save-history ui)
          (ast-delete (make-location :node node :id `(parse:lambda-list ,i)) j ui stack)))
       ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j)
              (and (type integer) k))
        (unless (zerop k)
          (save-history ui)
          (ast-delete (make-location :node node :id `(parse:lambda-list ,i ,j))
                      k ui stack)))
       ((eql 'parse:lambda-list)
        (save-history ui)
        (swap-node ll-loc (parse:ref-list) ui))))))

;;; function-code (lambda-list and body, shared by defun/defmacro/defmethod/
;;; lambda/flet/labels). TODO docstring/declarations
(defparameter *lambda-body-indent* 2)
(defmethod render-node ((node parse:function-code) stack context rect &key)
  (let* ((location (car stack))
         (lambda-list-view
           (render-node
            (parse:lambda-list node)
            (cons (make-location :node node :id 'parse:lambda-list) stack)
            context rect))
         (ll-rect (tui:rect lambda-list-view))
         (body-view
           (tui:vertical-container
            (tui:clamp-rect (tui:copy-rect rect :x (+ *lambda-body-indent* (tui:rect-x rect))
                                                :y (tui:rect-y2 ll-rect))
                            rect)
            (list-renderer (parse:body node)
                           (lambda (i) (make-location :node node :id (list 'parse:body i)))
                           stack context)))
         (body-rect (tui:rect body-view)))
    (make-instance 'ast-view
                   :location location
                   :children (list lambda-list-view body-view)
                   :rect (tui:copy-rect rect
                                        :rows (+ (tui:rect-rows ll-rect)
                                                 (tui:rect-rows body-rect))
                                        :cols (max (tui:rect-cols ll-rect)
                                                   (+ *lambda-body-indent*
                                                      (tui:rect-cols body-rect))))
                   :key-handler (global-key-handler node location context)
                   :focused (location= location (focus context)))))

(defmethod move-back ((node parse:function-code) id)
  (trivia:cmatch id
    ((list (eql 'parse:body) (and (type integer) i))
     (if (< 0 i) (list 'parse:body (1- i)) 'parse:lambda-list))))

(defmethod render-node ((node parse:lambda-form) stack context rect &key)
  (let* ((op "λ")
         (location (car stack))
         (fc-view
           (render-node
            (parse:fun-code node)
            (cons (make-location :node node :id 'parse:fun-code) stack)
            context
            (tui:clamp-rect (tui:copy-rect rect :x (+ (tui:rect-x rect) 1 1))
                            rect)))
         (fc-rect (tui:rect fc-view))
         (op-view (render-symbol op (make-location :node node :id 'parse:op)
                                stack context rect)))
    (make-instance 'ast-view
                   :location location
                   :children (list op-view fc-view)
                   :rect (tui:copy-rect rect
                                        :rows (tui:rect-rows fc-rect)
                                        :cols (+ 1 1 (tui:rect-cols fc-rect)))
                   :key-handler (global-key-handler node location context)
                   :focused (location= location (focus context)))))

;;; global key handlers
(setf (gethash (tui-sys:make-event :kind #\newline) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let* ((state (completion-state ui))
                    (selection (nth (selection state) (candidates state))))
          (let ((s (find-symbol selection)))
            (cond
              ((eq :function (cl-environments:function-information s))
               (let* ((args (loop for a in (slynk-backend:arglist s)
                                  while (not (find a lambda-list-keywords))
                                  count a))
                      (name-node (make-instance 'parse:symbol-ref
                                                :name (symbol-name s)
                                                :home-package (symbol-package s))))
                 (cond
                   ((and (eq 'parse:eval-form (locsort (focus ui)))
                         (bodylike-id (location-id (focus ui))))
                    (let ((function-node
                            (make-instance 'parse:function-call
                                            :name name-node
                                            :body (or (loop repeat args collect (hole))
                                                      (list (hole))))))
                      (swap-node (focus ui) function-node ui)
                      (descend ui (if (plusp args) '(parse:body 0) 'parse:name))))
                   ((eq (locsort (focus ui)) 'parse:symbol-ref)
                    (let* ((oldbody (parse:body (location-node (focus ui))))
                           (function-node
                             (make-instance
                              'parse:function-call
                              :name name-node
                              :body (append oldbody
                                            (loop for i below (- args (length oldbody))
                                                  collect (hole))))))
                      (swap-node (second (stack ui)) function-node ui))))))
              ((eq s 'cl:let*)
               (swap-node (focus ui)
                          (make-instance 'parse:let*-form
                                          :body (list (hole))
                                          :decls ()
                                          :vars (ref-list (ref-list (hole) (hole))))
                          ui))
              (t
               (swap-node (focus ui)
                          (make-instance 'parse:symbol-ref
                                         :name (symbol-name s)
                                         :home-package (symbol-package s))
                          ui))))
          (setf (completion-state ui) nil)
          t)))

(defmethod move-back ((node parse:function-call) id)
  (trivia:cmatch id
    ((list (eql 'parse:body) (and (type integer) i))
     (if (< 0 i) (list 'parse:body (1- i)) 'parse:name))
    ((eql 'parse:name) 'parse:name)))

(defmethod move-back ((node parse:let*-form) id)
  (trivia:cmatch id
    ((eql 'parse:op) 'parse:op)
    ((eql 'parse:vars) 'parse:op)
    ((list (eql 'parse:body) (and (type integer) i))
     (if (< 0 i) (list 'parse:body (1- i)) 'parse:vars))
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
                ((and (eq 'parse:eval-form (locsort (focus ui)))
                      (typep focused 'hole))
                 (let* ((location (focus ui))
                        (id (location-id location))
                        (parent (location-node location)))
                   ;; by default only delete children in some body of a form, not named locs
                   (when (bodylike-id id)
                     (let ((body-loc (make-location :node parent :id (parent-id id))))
                       (save-history ui)
                       (if (< 1 (length (elements (getloc body-loc))))
                           (ast-delete body-loc (id-index id) ui)
                           (let ((child-loc
                                   (ast-replace body-loc
                                                (lambda (body) (list-remove body (id-index id)))
                                                ui)))
                             (refocus ui (move-back (location-node child-loc) id))))
                       t))))))))

(setf (gethash (tui-sys:make-event :kind :right-arrow :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let* ((stack (stack ui))
               (loc (car stack))
               (up (location-node loc))
               (ploc (cadr stack))
               (upup (location-node ploc)))
          (when (and (is-binop-call up) (is-binop-call upup)
                     (trivia:match (location-id ploc)
                       ((list (eql 'parse:body) (eql 0)) t))
                     (trivia:match (location-id loc)
                       ((list (eql 'parse:body) (eql 1)) t)))
            (swap-node (caddr stack)
                       (make-instance
                        'parse:function-call
                        :name (parse:name up)
                        :body (list (first (parse:body up))
                                    (make-instance
                                     'parse:function-call
                                     :name (parse:name upup)
                                     :body (list (second (parse:body up))
                                                 (second (parse:body upup))))))
                       ui)
            (descend ui '(parse:body 1))
            (descend ui '(parse:body 0))))))

(setf (gethash (tui-sys:make-event :kind :left-arrow :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let* ((stack (stack ui))
               (loc (car stack))
               (up (location-node loc))
               (ploc (cadr (stack ui)))
               (upup (location-node ploc)))
          (when (and (is-binop-call up) (is-binop-call upup)
                     (trivia:match (location-id ploc)
                       ((list (eql 'parse:body) (eql 1)) t))
                     (trivia:match (location-id loc)
                       ((list (eql 'parse:body) (eql 0)) t)))
            (swap-node (caddr stack)
                       (make-instance
                        'parse:function-call
                        :name (parse:name up)
                        :body (list (make-instance
                                     'parse:function-call
                                     :name (parse:name upup)
                                     :body (list (first (parse:body upup))
                                                 (first (parse:body up))))
                                    (second (parse:body up))))
                       ui)
            (descend ui '(parse:body 0))
            (descend ui '(parse:body 1))))))

;; slurp
(setf (gethash (tui-sys:make-event :kind #\) :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let (parent-stack (and (bodylike-id (location-id (focus ui)))
                                     (parent-stack (stack ui))))
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
               ui))))))

;; barf
;; (setf (gethash (tui-sys:make-event :kind #\} :altp t) *default-key-handlers*)
;;       (lambda (view ui)
;;         (declare (ignore view))
;;         (when (typep (getloc (focus ui)) 'parse:eval-form)
;;           (swap-node (focus ui) (hole) ui))))

(defun fill-zipper (zipper forms)
  "Fills the zipper hole with forms and returns a complete node."
  (multiple-value-bind (low high) (selection-range zipper)
    (parse:copy-node
     (nth-value 1 (rebuild-spine (selection-list-location zipper)
                                 (lambda (body)
                                   (let ((elts (elements body)))
                                     `(,@(subseq elts 0 low)
                                       ,@forms
                                       ,@(subseq elts (1+ high)))))
                                 (zipper-stack zipper))))))

(defun take-selection (ui selection)
  "Reifies selection state into the cutbuffer without editing."
  (if (typep selection 'zipper)
      (setf (zipper ui) selection
            (cutbuffer ui) nil)
      (let ((forms (mapcar #'parse:copy-node (selection-forms selection))))
        (setf (cutbuffer ui)
              (make-instance 'cutbuffer
                             :location (selection-location selection)
                             :content (if (rest forms) forms (first forms)))
              (zipper ui) nil)))
  t)

(defun cut-selection (ui selection)
  "Cuts the current selection or zipper, modifying the ast."
  (let ((forms (selection-forms selection)))
    (take-selection ui selection)
    (if (typep selection 'zipper)
        (let* ((stack (nthcdr (zipper-depth selection) (stack ui)))
               (hole (car stack))
               (id (location-id hole)))
          (if (> (length forms) 1)
              ;; a zipper may top out at any eval-form, which is not always a place
              ;; several forms can be left in e.g. dotimes counter
              (when (eval-list-position-p hole)
                (save-history ui)
                (let ((slot (parent-id id))
                      (index (id-index id)))
                  (ast-replace (make-location :node (location-node hole) :id slot)
                               (lambda (body)
                                 (let ((elts (elements body)))
                                   `(,@(subseq elts 0 index)
                                     ,@forms
                                     ,@(subseq elts (1+ index)))))
                               ui stack)
                  (refocus ui (append-id slot index))))
              (progn
                (save-history ui)
                (ast-replace hole (constantly (first forms)) ui stack))))
        (multiple-value-bind (low high) (selection-range selection)
          (save-history ui)
          (let ((slot (selection-parent-id selection)))
            (ast-replace (selection-list-location selection)
                         (lambda (body)
                           (let ((elts (elements body)))
                             (or `(,@(subseq elts 0 low) ,@(subseq elts (1+ high)))
                                 (list (hole)))))
                         ui)
            (refocus ui (append-id slot (max 0 (1- low)))))))
    t))

(setf (gethash (tui-sys:make-event :kind #\c :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (if-let (selection (selection-at-focus ui))
          (take-selection ui selection)
          (let ((loc (focus ui)))
            (when (locsort loc)
              (setf (cutbuffer ui)
                    (make-instance 'cutbuffer :location loc
                                              :content (parse:copy-node (getloc loc)))
                    (zipper ui) nil))))
        t))

(setf (gethash (tui-sys:make-event :kind #\x :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (if-let (selection (selection-at-focus ui))
          (cut-selection ui selection)
          (let* ((loc (focus ui))
                 (node (getloc loc)))
            (when (locsort loc)
              (setf (cutbuffer ui)
                    (make-instance 'cutbuffer :location loc
                                              :content (parse:copy-node node))
                    (zipper ui) nil)
              (swap-node loc (hole) ui))))
        t))

(defun paste-forms (ui forms)
  (let ((focus (focus ui)))
    (when (eval-list-position-p focus)
      (let ((slot (parent-id (location-id focus)))
            (index (id-index (location-id focus))))
        (save-history ui)
        (ast-replace (make-location :node (location-node focus) :id slot)
                     (lambda (body)
                       (let ((elts (elements body)))
                         `(,@(subseq elts 0 index)
                           ,@(mapcar #'parse:copy-node forms)
                           ,@(subseq elts index))))
                     ui)
        (refocus ui (append-id slot index))
        t))))

(defun paste-zipper (ui zipper)
  "Pastes from (zipper ui) around the current selection using fill-zipper,
modifying the ast."
  (let ((focus (focus ui))
        (selection (selection-at-focus ui)))
    (if selection
        (multiple-value-bind (low high) (selection-range selection)
          (let ((forms (selection-forms selection))
                (slot (selection-parent-id selection)))
            (save-history ui)
            (ast-replace (selection-list-location selection)
                         (lambda (body)
                           (let ((elts (elements body)))
                             `(,@(subseq elts 0 low)
                               ,(fill-zipper zipper forms)
                               ,@(subseq elts (1+ high)))))
                         ui)
            (refocus ui (append-id slot low))))
        (when (eq 'parse:eval-form (locsort focus))
          (swap-node focus (fill-zipper zipper (list (getloc focus))) ui)))
    t))

(setf (gethash (tui-sys:make-event :kind #\v :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        ;; these are mutually exclusive as cutting a zipper nulls the cutbuffer
        (cond ((zipper ui) (paste-zipper ui (zipper ui)))
              ((cutbuffer ui)
               (let ((content (content (cutbuffer ui))))
                 (if (listp content)
                     (paste-forms ui content)
                     (when (equal (locsort (focus ui))
                                  (locsort (location (cutbuffer ui))))
                       (swap-node (focus ui) (parse:copy-node content) ui))))))
        t))

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
  ;; note: order of rendering here matters
  (tui:fill-rect (tui:make-style :bg #x0) (ui-rect ui) (ui-rect ui))
  (let ((toplevel-views
          (list (render-node (ast ui) (last (stack ui)) ui (ui-rect ui))
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
        (slog* (format nil "~a~a" (make-string 70 :initial-element #\-) 'redisplay-done)))
    (stop ()
      :report "exit"
      (tui:stop ui))))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\q)
             (tui:event-controlp event))
        (tui:stop ui)
        (call-next-method))
    (slog* (format nil "~a~a" (make-string 70 :initial-element #\-) 'event-handled))))

(defun tui-main ()
  (let* ((ast (parse::parse-from-string
               "(lambda (a &key (b a supplied-p))
                  (1- b) :test)"))
         (root-loc (make-location :node 'undefined))
         (tui (make-instance 'ui :ast ast :stack (list root-loc))))
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
                      (format t "~a~%" value))
                  (force-output)))
      (tui-main)))
