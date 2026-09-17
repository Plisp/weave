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
                #:locsort #:node-at #:update
                #:append-id #:parent-id #:bodylike-id #:id-index
                #:ref-list #:elements #:gen-list-p #:gen-tree-ref
                #:hole
                #:text)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed)
                    (#:tui-sys #:uncursed-sys))
  (:export #:main))
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

(defun reading-order-p (views)
  "Does each of `views' start no earlier than the one before it, by line then column?"
  (loop for (a b) on views
        while b
        always (let ((ra (tui:rect a))
                     (rb (tui:rect b)))
                 (or (< (tui:rect-y ra) (tui:rect-y rb))
                     (and (= (tui:rect-y ra) (tui:rect-y rb))
                          (<= (tui:rect-x ra) (tui:rect-x rb)))))))

(defclass ordered-view (tui:view)
  ()
  (:documentation "A mixin view whose children must be in reading order. Checked at
construction, do not modify."))

(defmethod initialize-instance :after ((view ordered-view) &key)
  (assert (reading-order-p (tui:children view)) ()
          "Children of ~a are not in reading order: ~a" view (tui:children view)))

(defclass ast-view (ordered-view)
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

(defun selection-locations (selection)
  (let ((loc (selection-location selection)))
    (assert (bodylike-id (location-id loc)))
    (multiple-value-bind (low high)
        (selection-range selection)
      (loop for i from low to high
            collect (make-location :node (location-node loc)
                                   :id (append-id (selection-parent-id selection) i))))))

(defun selection-forms (selection)
  (mapcar #'node-at (selection-locations selection)))

(defun zipper-top-p (ui stack)
  (when-let (selection (selection ui))
    (and (typep selection 'zipper)
         (eq (node-at (car stack)) ; top of zipper
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
  ((forms :initarg :forms
          :initform (error "no forms")
          :reader cutbuffer-forms)
   (sorts :initarg :sorts
          :initform (error "no sorts")
          :reader cutbuffer-sorts)))

(defun copy-locations (locations)
  (make-instance 'cutbuffer
                 :forms (mapcar (lambda (loc) (parse:copy-node (node-at loc))) locations)
                 :sorts (mapcar #'locsort locations)))

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
   (redisplay-cache :initform (make-hash-table :test #'equal)
                    :reader redisplay-cache)
   (goal-stacks :initform nil
                :accessor goal-stacks
                :documentation "The stacks at each node alt+p moved up from.")
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

(defun refocus (ui id)
  "Moves the focus to another location in the node it is already within."
  (end-selection-mode ui)
  (let ((loc (make-location :node (location-node (focus ui)) :id id)))
    (setf (stack ui) (cons loc (cdr (stack ui))))
    loc))

(defun descend (ui id)
  "Moves the focus to a location-id inside the currently focused node."
  (end-selection-mode ui)
  (let ((loc (make-location :node (node-at (focus ui)) :id id)))
    (push loc (stack ui))
    loc))

(defun parent-stack (stack)
  "relies on ids being either 'symbol or ('symbol integer*) for list addressing.
Plain list bodies are skipped since they are never focused."
  (let* ((loc (car stack))
         (id (location-id loc)))
    (cond ((and (listp id)
                (listp (node-at (make-location :node (location-node loc) :id (parent-id id)))))
           (cdr stack))
          ((listp id)
           (cons (make-location :node (location-node loc) :id (parent-id id))
                 (cdr stack)))
          (t (cdr stack)))))

;;
;;; editing operations
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
             (funcall updater (node-at loc))
             (list))))

(defun commit-edit (ui stack root)
  "Installs a `rebuild-spine' result."
  (end-selection-mode ui)
  (setf (ast ui) root
        (stack ui) stack)
  (focus ui))

(defun save-history (ui)
  (push (make-instance 'state :stack (stack ui) :ast (ast ui))
        (history ui)))

(defun ast-replace (loc updater ui &optional (stack (stack ui)))
  (if (typep (location-node loc) 'parse:macro-call)
      (replace-in-macro-call loc (funcall updater (node-at loc)) ui)
      (multiple-value-call #'commit-edit ui (rebuild-spine loc updater stack))))

(defun edit-list (loc updater focus-id ui stack)
  "Replaces the list at `loc' using `updater' and focuses `focus-id'."
  (if (typep (location-node loc) 'parse:macro-call)
      (replace-in-macro-call loc (funcall updater (node-at loc)) ui focus-id)
      (progn (multiple-value-call #'commit-edit ui (rebuild-spine loc updater stack))
             (refocus ui focus-id))))

(defun ast-insert (item loc index ui &optional (stack (stack ui)) tail)
  "Focuses the inserted item, optionally descends the path `tail'."
  (edit-list loc (lambda (body) (list-insert (elements body) item index))
             (reduce #'append-id (cons index tail) :initial-value (location-id loc))
             ui stack))

(defun ast-delete (loc index ui &optional (stack (stack ui)))
  "assumes that list has length > 1"
  (edit-list loc (lambda (body) (list-remove (elements body) index))
             (append-id (location-id loc) (max 0 (1- index))) ui stack))

(defun value-at-path (loc path)
  "The thing at `path' from the list at `loc' or NIL if there is none."
  (let ((node (location-node loc))
        (list (node-at loc)))
    (assert (gen-list-p list))
    (if (typep node 'parse:macro-call)
        (reduce (lambda (value id) (node-at (make-location :node value :id id)))
                (parse:path-at node
                               (reduce #'append-id path :initial-value (location-id loc)))
                :initial-value node)
        (gen-tree-ref list path))))

(defun insert-or-jump (loc index ui stack &key (item (hole)) tail)
  "Moves to the hole at `index' of the list at `loc' and down the indices `tail' if passed,
otherwise inserts `item' at `index' and focuses down `tail' within it. `stack' is the
part of the ui stack starting in the node of `loc'."
  (let ((path (cons index tail)))
    (if (typep (value-at-path loc path) 'parse:hole)
        (progn
          (setf (stack ui) stack)
          (refocus ui (reduce #'append-id path :initial-value (location-id loc))))
        (progn
          (save-history ui)
          (ast-insert item loc index ui stack tail)))))

(defun replace-in-macro-call (location newnode ui &optional focus-id)
  "Replaces the syntax at `location' in a macro call, which may reparse part of it into a
form. Pop the cursor to the call's location so the parsed call can be inserted,
then descend to the original cursor position, or to the syntax at `focus-id' if given."
  (let* ((call (location-node location))
         (outer (cdr (member-if (lambda (l) (eq (location-node l) call)) (stack ui)))))
    (multiple-value-bind (new-call path)
        (parse:update call (location-id location) newnode)
      (multiple-value-call #'commit-edit ui
        (rebuild-spine (car outer) (constantly new-call) outer))
      ;; descend into the parsed form, or keep the original path
      (dolist (id (if focus-id
                      (parse:path-at new-call focus-id)
                      (or path (list (location-id location))))
                  (focus ui))
        (push (make-location :node (node-at (focus ui)) :id id) (stack ui))))))

(defun swap-node (location newnode ui)
  "saves undo history"
  (when (eq (location-id location) :open)
    (setf location (second (member location (stack ui) :test #'location=))))
  (when (or (null (edit-loc ui)) (not (location= location (edit-loc ui))))
    (save-history ui))
  ;; perform the insertion
  (let ((newloc (if (typep (location-node location) 'parse:macro-call)
                    (replace-in-macro-call location newnode ui)
                    (ast-replace location (constantly newnode) ui
                                 (member-if (lambda (l) (eq (location-node location)
                                                       (location-node l)))
                                            (stack ui))))))
    (setf (future ui) nil
          (edit-loc ui) newloc)))

;;
;;; ast classes
;;

(defgeneric render-node (node stack context rect &key &allow-other-keys))

(defmacro with-redisplay-cache ((ui key) &body body)
  "The value of `body', computed once per redisplay of `ui' for `key'."
  (once-only (ui key)
    (with-gensyms (value found)
      `(multiple-value-bind (,value ,found) (gethash ,key (redisplay-cache ,ui))
         (if ,found
             ,value
             (setf (gethash ,key (redisplay-cache ,ui)) (progn ,@body)))))))

(defun bindings-at (loc ui)
  (with-redisplay-cache (ui (cons (location-node loc) (location-id loc)))
    (when (member (locsort loc) '(parse:eval-form parse:function-code))
      (parse:location-bindings (location-node loc) (location-id loc)))))

(defun function-position-p (loc)
  (let ((node (location-node loc))
        (id (location-id loc)))
    (or (and (typep node 'parse:function-call) (eq id 'parse:name))
        (and (typep node 'parse:macro-call) (eq id 'parse:op))
        (and (typep node 'parse:function-form) (eq id 'parse:fun-designator)))))

(defun block-position-p (loc)
  (and (typep (location-node loc) 'parse:return-from-form)
       (eq (location-id loc) 'parse:name)))

(defun lexical-symbol-ref (node loc)
  (and (typep node 'parse:symbol-ref)
       (or (eq (locsort loc) 'parse:eval-form)
           (eq (locsort loc) 'parse:fun-designator)
           (function-position-p loc)
           (block-position-p loc))))

(defun compute-focus-binder (node stack ui)
  (let ((loc (car stack)))
    (when (lexical-symbol-ref node loc)
      (let ((kind (cond ((function-position-p loc) :function)
                        ((block-position-p loc) :block)
                        (t :variable)))
            (name (parse:name node)))
        (loop for loc in stack
              do (loop for (k . binder) in (bindings-at loc ui)
                       do (when (and (eq k kind)
                                     (typep binder 'parse:binder) ; ignore holes
                                     (string= name (parse:name binder)))
                            (return-from compute-focus-binder binder))))))))

(defun focus-binder (ui)
  "The binder the symbol-ref under the cursor refers to, innermost binding first."
  (with-redisplay-cache (ui :focus-binder)
    (compute-focus-binder (node-at (focus ui)) (stack ui) ui)))

(defun symbol-ref-boundp (node stack ui)
  (or (compute-focus-binder node stack ui)
      ;; blocks are never global
      (and (not (block-position-p (car stack)))
           (multiple-value-bind (symbol found) (parse:resolve node)
             (and found
                  (if (function-position-p (car stack))
                      (fboundp symbol)
                      (boundp symbol)))))))

(defun unexpanded-macro-op-p (loc)
  "Whether `loc' is the name of a macro call that failed to expand."
  (let ((node (location-node loc)))
    (and (typep node 'parse:macro-call)
         (eq (location-id loc) 'parse:op)
         ;; a hole op has no name to blame
         (typep (parse:op node) 'parse:symbol-ref)
         (not (parse:expanded node)))))

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
    (if (eq node (focus-binder context))
        (tui:fill-rect (tui:make-style :bg (tui:color #x85 #x99 #x00))
                       (tui:copy-rect rect :x 0 :y 0) rect
                       :blend 0.4)
        (when (and (lexical-symbol-ref node (car stack))
                   (not (symbol-ref-boundp node stack context)))
          (tui:fill-rect (tui:make-style :bg (tui:color #xdc #x32 #x2f))
                         (tui:copy-rect rect :x 0 :y 0) rect
                         :blend 0.4)))
    (when (unexpanded-macro-op-p (car stack))
      (tui:fill-rect (tui:make-style :bg (tui:color #xcb #x4b #x16))
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

(defparameter *default-key-handlers* (make-hash-table :test #'equal))
(defparameter *global-key-handlers* (make-hash-table :test #'equal))
(defun plain-body-location-p (location)
  "Whether `location' addresses a whole evaluated body, which is an invariant violation
since these aren't a proper location."
  (let ((id (location-id location)))
    (and (symbolp id)
         (eq '&body (cdr (assoc id (parse:form-slot-kinds
                                    (type-of (location-node location)))))))))

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
        (if (not (eq (node-at (focus ui)) anchor))
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
    (assert (not (plain-body-location-p (focus ui))) ()
            "focused the body ~a of ~a" (location-id (focus ui))
            (type-of (location-node (focus ui))))))

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
                                              :body (list (node-at loc) (hole)))
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
    (when (bodylike-id id)
      (setf (selection ui)
            (make-instance 'selection
                           :context (stack ui)
                           :location loc
                           :point (id-index id))))))

(defun extend-selection (ui delta)
  (when-let (selection (or (current-selection ui) (begin-selection ui)))
    (let ((point (+ (selection-point selection) delta)))
      (when (<= 0 point (1- (length (elements
                                     (node-at (selection-list-location selection))))))
        ;; when the underlying range is reselected, forget the zipper
        (when (typep selection 'zipper)
          (change-class selection 'selection))
        (setf (selection-point selection) point)
        ;; keep selection active after move
        (refocus ui (append-id (selection-parent-id selection) point))
        (setf (selection-activep selection) t)))
    t))

(defun eval-list-position-p (location)
  "Whether LOCATION can accept a list of evaluated forms."
  (let ((id (location-id location)))
    (and (bodylike-id id)
         (or (eq 'parse:eval-form
                 (parse:location-sort (location-node location)
                                      (append-id (parent-id id) 0)))
             (and (typep (location-node location) 'parse:macro-call)
                  (not (eq (location-id location) 'parse:op)))))))

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

(defun goal-valid-p (ui)
  "Can we still access the same children through the last saved goal?"
  (when-let (goal (car (goal-stacks ui)))
    (let ((parent (parent-stack goal))
          (stack (stack ui)))
      (and (= (length parent) (length stack))
           (every #'location= parent stack)))))

(defun move-parent (ui)
  (end-selection-mode ui)
  (let ((old (stack ui)))
    (when-let (new-stack (parent-stack old))
      (slog* `(moving to ,new-stack))
      (setf (goal-stacks ui) (cons old (when (goal-valid-p ui)
                                         (goal-stacks ui)))
            (stack ui) new-stack))))

(defun move-child (ui)
  "Steps back down into the goal child."
  (end-selection-mode ui)
  (when (goal-valid-p ui)
    (setf (stack ui) (pop (goal-stacks ui)))))

(setf (gethash (tui-sys:make-event :kind #\p :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (move-parent ui)))

(setf (gethash (tui-sys:make-event :kind #\n :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (move-child ui)))

(setf (gethash (tui-sys:make-event :kind #\P :altp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        ;; this is really a do-while idiom
        (prog1 (move-parent ui)
          (loop for last-stack = nil then stack
                for stack = (slog (stack ui))
                for loc = (car stack)
                for node = (node-at loc)
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
  (or (alphanumericp c) (find c "+-*/=<>!?&%$_~^.@[]{}")))

(defmethod handle-key ((node hole) view location ui event)
  (let ((c (tui:event-kind event)))
    (when (is-regular-char-event event)
      (trivia:match (slog (locsort location))
        ((eql 'parse:binder)
         (when (and (symbol-char-p c) (not (digit-char-p c)))
           (swap-node location (make-instance 'parse:binder :name (string-upcase c)) ui)))
        ((eql 'parse:eval-form)
         (cond
           ((and (char= c #\() (not (typep (location-node location) 'parse:macro-call)))
            (swap-node location (ref-list) ui))
           ((and (symbol-char-p c) (not (digit-char-p c)))
            (let ((newnode (make-instance 'parse:symbol-ref :name (string-upcase c))))
              (swap-node location newnode ui)))
           ((digit-char-p c)
            (let ((newnode (make-instance 'parse:literal :str (string c))))
              (swap-node location newnode ui)))))
        ((or (eql 'parse:fun-designator) (eql 'parse:symbol-ref))
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
                 (when (plusp (length s)) ; now length 2, completion for if
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

(defparameter *symbol-mappings* '((<= . #\≤) (>= . #\≥) (* . #\⋅) (/= . #\≠) (lambda . #\λ)
                                  (read-quote . #\') (read-function . "#'")))

(defun symbol-ref-text (node)
  (let ((name (parse:name node)))
    (if (eq (parse:home-package node) (find-package :keyword))
        (format nil ":~(~a~)" name)
        (if-let (exp (assoc-value *symbol-mappings* name :test #'string=))
          (string exp)
          (string-downcase name)))))

(defmethod render-node ((node parse:symbol-ref) stack context rect &key)
  (let* ((location (car stack))
         (str (symbol-ref-text node))
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
  (let* ((text (if-let (exp (assoc-value *symbol-mappings* op :test #'string=))
                 (string exp)
                 (string-downcase op)))
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
;; a list as written, drawn as the elements it holds
(defparameter *list-delimiters* '("(" . ")"))

(defmethod parse:get-location ((node parse:ref-list) (id (eql :open)))
  node)

(defmethod parse:location-sort ((node parse:ref-list) (id (eql :open)))
  'parse:unevaluated)

(defun handle-focus-p (ui list-loc)
  "Is the focus the opening parenthesis handle of the list at `list-loc'?"
  (let ((stack (stack ui)))
    (and (eq (location-id (first stack)) :open)
         (location= (second stack) list-loc))))

(defun render-delimiter (text loc stack rect &key hoverable)
  (let ((view
          (make-instance 'ast-view
                         :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                         :location loc
                         :hoverable hoverable)))
    (tui:puts text 1 1 rect)
    (setf (view-stack view) stack)
    view))

(defparameter *horizontal* '((0 . nil)))
(defparameter *vertical* '((0 . t)))

(defun list-locator (location)
  "Locates the elements of the list at `location' by index."
  (lambda (index)
    (make-location :node (location-node location)
                   :id (append-id (location-id location) index))))

(defun render-elements (elements spec locate stack context rect keys)
  "Splits `count' elements into rows of (indent . n) following `spec', a list of
(indent . count) pairs where count is a number of elements or NIL for all remaining
elements on a single line. The last pair repeats, e.g. setq is (2 . 2),
T is a synonym for 1. `spec' is such a spec or a function from `elements' to specs."
  (let ((pairs (if (listp spec) spec (funcall spec elements)))
        (remaining elements)
        (transform (or (getf keys :transform) #'identity))
        (index 0))
    (tui:with-vertical (rect)
      (loop while (and remaining (not (tui:full)))
            do (destructuring-bind (indent . n) (car pairs)
                 (setf pairs (or (cdr pairs) pairs))
                 (tui:place (r)
                   (tui:with-horizontal (r)
                     (when (plusp indent)
                       (tui:pad indent))
                     (loop
                       with count = (case n
                                      ((t) 1)
                                      ((nil) (length remaining))
                                      (otherwise (max 1 n)))
                       for i from 0 below count
                       while remaining
                       do (let ((element (funcall transform (pop remaining)))
                                (loc (funcall locate index)))
                            (incf index)
                            (unless (zerop i)
                              (tui:pad 1))
                            (tui:place (r)
                              (apply #'render-node element (cons loc stack) context r
                                     keys)))))))))))

(defun view-end (view)
  "The column after and the line of the last cell `view' draws, following its last child.
Relies on children being in reading order, which `ordered-view' checks."
  (if-let (children (tui:children view))
    (view-end (lastcar children))
    (let ((r (tui:rect view)))
      (values (tui:rect-x2 r) (tui:rect-y r)))))

(defmethod render-node ((l parse:ref-list) stack context rect
                        &key (indent *horizontal*) delimited (transform #'identity))
  "A `delimited' list is drawn with `*list-delimiters*' around it, recursively unless
`delimited' is :shallow. It's opening parens is selectable."
  (let* ((location (car stack))
         (focused (location= location (focus context))))
    (flet ((contents (rect)
             (render-elements (elements l) indent (list-locator location)
                              (cdr stack) context rect
                              `(,@(when (eq delimited t) '(:delimited t))
                                ,@(when transform `(:transform ,transform))))))
      (if delimited
          (let* ((handle (make-location :node l :id :open))
                 (open (render-delimiter (car *list-delimiters*) handle (cons handle stack) rect
                                         :hoverable t))
                 (inner (contents (tui:clamp-rect
                                   (tui:copy-rect rect :x (tui:rect-x2 (tui:rect open)))
                                   rect)))
                 (close (multiple-value-bind (end-x end-y) (view-end inner)
                          (render-delimiter (cdr *list-delimiters*) location stack
                                            (tui:clamp-rect
                                             (tui:copy-rect rect :x end-x :y end-y)
                                             rect)))))
            (when (location= handle (focus context))
              (setf (tui:key-handler open) (global-key-handler l handle context)
                    (tui:focused open) t
                    (focus-rect context) (tui:rect open)))
            (make-instance 'ordered-view
                           :key-handler (global-key-handler l location context)
                           :focused focused
                           :rect (tui:copy-rect
                                  rect
                                  :rows (max 1 (tui:rect-rows (tui:rect inner)))
                                  :cols (- (max (tui:rect-x2 (tui:rect inner))
                                                (tui:rect-x2 (tui:rect close)))
                                           (tui:rect-x rect)))
                           :children (list open inner close)))
          (let ((view (if (elements l)
                          (contents rect)
                          (render-delimiter (format nil "~a~a" (car *list-delimiters*)
                                                    (cdr *list-delimiters*))
                                            location stack rect :hoverable t))))
            (setf (tui:key-handler view) (global-key-handler l location context)
                  (tui:focused view) focused)
            view)))))

;; for body lists
(defmethod render-node ((l list) stack context rect &key (indent *horizontal*))
  (let* ((location (car stack))
         (view (render-elements l indent (list-locator location)
                                (cdr stack) context rect nil)))
    (setf (tui:key-handler view) (global-key-handler l location context)
          (tui:focused view) (location= location (focus context)))
    view))

;;; function call
(defparameter *fun-key-handlers* (make-hash-table :test #'equal))

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
          (insert-or-jump (make-location :node fun-node :id 'parse:body) i ui (stack ui)))))


;; (define-constant +top-left+ (name-char "U1CE16") :test #'equal)
;; (define-constant +bot-left+ (name-char "U1CE17") :test #'equal)
;; (define-constant +top-right+ (name-char "U1CE18") :test #'equal)
;; (define-constant +bot-right+ (name-char "U1CE19") :test #'equal)
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
                      :children (list arg1-view line-view arg2-view)
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
                        :children (list arg1-view op-view arg2-view)
                        :key-handler (global-key-handler node location context)
                        :focused (location= location (focus context))))))
    (t
     (let* ((location (car stack))
            (name-view (render-node (parse:name node)
                                    (cons (make-location :node node :id 'parse:name) stack)
                                    context rect))
            (name-rect (tui:rect name-view))
            (args-view
              (render-elements (parse:body node) *vertical*
                               (list-locator
                                (make-location :node node :id 'parse:body))
                               stack context
                               (tui:clamp-rect
                                (tui:copy-rect rect :x (+ 1 (tui:rect-x2 name-rect)))
                                rect)
                               nil))
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

;;
;;; macro calls
;;
(defparameter *loop-clause-keywords*
  '("NAMED" "WITH" "FOR" "AS" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS"
    "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING"
    "COUNT" "COUNTING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
    "DO" "DOING" "RETURN" "INITIALLY" "FINALLY"
    "WHEN" "IF" "UNLESS" "ELSE" "END" "AND"))
(defparameter *loop-prefix-keywords* '("AND" "ELSE"))
(defparameter *loop-body-keywords* '("DO" "DOING" "INITIALLY" "FINALLY"))

(defun find-loop-keyword (element)
  (and (typep element 'parse:symbol-ref)
       (find (parse:name element) *loop-clause-keywords* :test #'string=)))

(defun loop-indentation (elements)
  "Indent spec for a loop form. The operator has its own line, then a line indented by one
for every clause keyword, except those following and/else. Forms of a do-like clause after
its first are aligned with the first, and a loop without keywords has a line per form."
  (let* ((indent 1)
         (rows (list (cons 0 1)))
         (column indent)
         (body-column nil)
         (body-started nil)
         (previous (first elements)))
    (loop for element in (rest elements)
          for first-element = t then nil
          for keyword = (find-loop-keyword element)
          do (cond (keyword
                    (if (member (find-loop-keyword previous) *loop-prefix-keywords*
                                :test #'equal)
                        (progn (incf (cdar rows))
                               (incf column (1+ (length (symbol-ref-text previous)))))
                        (progn (push (cons indent 1) rows)
                               (setf column indent)))
                    (setf body-column (when (member keyword *loop-body-keywords*
                                                    :test #'string=)
                                        (+ column (length (symbol-ref-text element)) 1))
                          body-started nil))
                   (first-element
                    (push (cons indent 1) rows)
                    (setf body-column indent
                          body-started t))
                   ((and body-column body-started)
                    (push (cons body-column 1) rows))
                   (t
                    (incf (cdar rows))
                    (setf body-started t)))
             (setf previous element))
    (reverse rows)))

(defun macro-call-indentation (op)
  "`loop-indentation' for loop, else TODO ask slynk."
  (if (and (typep op 'parse:symbol-ref) (eq (parse:resolve op) 'loop))
      #'loop-indentation
      *horizontal*))

(defmethod render-node ((node parse:macro-call) stack context rect &key)
  (let* ((location (car stack))
         (view (render-elements
                (cons (parse:op node) (parse:body node))
                (macro-call-indentation (parse:op node))
                (lambda (index)
                  (make-location :node node
                                 :id (if (zerop index)
                                         'parse:op
                                         (list 'parse:body (1- index)))))
                stack context rect
                `(:delimited t
                  :transform ,(lambda (elt)
                                (or (car (gethash elt (parse:subforms node)))
                                    elt))))))
    (make-instance 'ast-view
                   :location location
                   :children (list view)
                   :rect (tui:rect view)
                   :key-handler (global-key-handler node location context)
                   :focused (location= location (focus context)))))

(defparameter *macro-call-key-handlers* (make-hash-table :test #'equal))

(defmethod handle-key ((node parse:macro-call) view location ui event)
  (declare (ignore view))
  (when-let ((handler (gethash event *macro-call-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (stack ui)) ui)))))

(defun wrap-in-list (loc ui stack)
  "Wraps the value at `loc' in a syntax list, adding a hole afterwards unless it is
already and focuses the hole."
  (let ((holep (typep (node-at loc) 'parse:hole)))
    (edit-list loc (lambda (thing) (if holep (ref-list thing) (ref-list thing (hole))))
               (append-id (location-id loc) (if holep 0 1)) ui stack)))

(defun macro-body-id-p (id)
  (and (consp id) (eq (car id) 'parse:body)))

(setf (gethash (tui-sys:make-event :kind #\() *macro-call-key-handlers*)
      (lambda (stack ui)
        (let ((loc (car stack)))
          (when (macro-body-id-p (location-id loc))
            (save-history ui)
            (wrap-in-list loc ui stack)
            t))))

(setf (gethash (tui-sys:make-event :kind #\space) *macro-call-key-handlers*)
      (lambda (stack ui)
        (let* ((loc (car stack))
               (node (location-node loc))
               (id (location-id loc)))
          (multiple-value-bind (list-id index)
              (cond ((eq id 'parse:op) (values 'parse:body 0))
                    ((handle-focus-p ui loc) (values id 0))
                    (t (values (parent-id id) (1+ (lastcar id)))))
            (insert-or-jump (make-location :node node :id list-id) index ui stack))
          t)))

(setf (gethash (tui-sys:make-event :kind #\rubout) *macro-call-key-handlers*)
      (lambda (stack ui)
        (let* ((loc (car stack))
               (id (location-id loc)))
          (when (macro-body-id-p id)
            (let ((parent (make-location :node (location-node loc) :id (parent-id id))))
              (save-history ui)
              (if (= 1 (length (elements (node-at parent))))
                  (edit-list parent (constantly nil)
                             (if (eq (location-id parent) 'parse:body)
                                 'parse:op
                                 ;; focus a nested list
                                 (location-id parent))
                             ui stack)
                  (ast-delete parent (lastcar id) ui stack))
              t)))))

;;
;;; layout generation
;;
(defvar *default-expansions* (make-hash-table :test #'eq)
  "Operator symbol -> closure constructing a default node for that form.")

(defstruct (layout (:constructor make-layout (type rows templates)))
  "Describes the rendered shape of `type'.
`rows' describe horizontal order and `templates' contains (slot . constructor)
pairs building new elements for &rest and &body slots."
  type rows templates)

(defvar *layouts* (make-hash-table :test #'eq) "Form class -> its `layout'.")

(defun layout-slots (layout)
  (loop for row in (layout-rows layout)
        append (typecase row
                 (cons (loop for item in row
                             when (symbolp item) collect item
                             when (consp item) collect (car item)))
                 (symbol (list row)))))

(defun layout-template (layout slot)
  (cdr (assoc slot (layout-templates layout))))

(defun slot-kind (layout slot)
  (cdr (assoc slot (parse:form-slot-kinds (layout-type layout)))))

(defgeneric focus-order (node)
  (:method (node)
    (when-let (layout (gethash (type-of node) *layouts*))
      (layout-slots layout)))
  (:method ((node parse:function-call)) '(parse:name parse:body))
  (:method ((node parse:macro-call)) '(parse:op parse:body))
  (:method ((node parse:function-code))
    '(parse:lambda-list parse::docstring parse::declarations parse:body))
  (:documentation "The slots of `node' in reading order."))

(defun slot-end-position (node slot)
  "The last focusable position of `slot', or NIL."
  (let ((value (node-at (make-location :node node :id slot))))
    (cond ((null value) nil)
          ;; plain list bodies are never focused, see `deflayout'
          ((listp value) (append-id slot (1- (length value))))
          (t slot))))

(defun move-back (node id)
  "The position preceding `id' in `node' for deletions. Returns `id' if first."
  (flet ((previous-slot (slot)
           (let ((before (loop for s in (focus-order node) until (eq s slot) collect s)))
             (or (loop for s in (reverse before) thereis (slot-end-position node s))
                 slot))))
    (if (symbolp id)
        (previous-slot id)
        (let ((index (id-index id))
              (parent (parent-id id)))
          (cond ((< 0 index) (append-id parent (1- index)))
                ((symbolp parent) (previous-slot parent))
                (t parent))))))

(defun default-slot-value (kind template)
  (case kind
    ((&rest) (apply #'ref-list (when template (list (funcall template)))))
    ((&body parse::&declarations parse::&rest-qualifiers)
     (when template (list (funcall template))))
    ((parse::&lambda parse::&macro-lambda parse::&method-lambda)
     (make-instance 'parse:function-code
                    :lambda-list-kind (make-keyword (subseq (string kind) 1))
                    :lambda-list (ref-list)
                    :body (list (hole))))
    (t (hole))))

(defun default-node (layout)
  "A form of the class of `layout' with every slot in its default state."
  (apply #'make-instance (layout-type layout)
         (loop for slot in (layout-slots layout)
               unless (eq slot 'parse:op)
                 append (list (make-keyword slot)
                              (default-slot-value (slot-kind layout slot)
                                                  (layout-template layout slot))))))

(defun slot-render-keys (node slot)
  (case (cdr (assoc slot (parse:form-slot-kinds (type-of node))))
    (parse::&tree '(:delimited t))
    (&rest (list :indent *vertical* :delimited :shallow))
    (t (list :indent *vertical*))))

(defun render-layout-slot (node item stack context rect)
  (cond
    ((consp item)
     (destructuring-bind (slot spec) item
       (apply #'render-node (funcall slot node)
              (cons (make-location :node node :id slot) stack)
              context rect
              :indent spec
              (slot-render-keys node slot))))
    ((eq item 'parse:op)
     (render-symbol (funcall item node)
                    (make-location :node node :id 'parse:op)
                    stack context rect))
    ((symbolp item)
     (apply #'render-node (funcall item node)
            (cons (make-location :node node :id item) stack)
            context rect
            (slot-render-keys node item)))
    (t
     (cerror "unexpected layout element ~a" item))))

(defun render-horizontal-layout (node across-layout stack context rect)
  (tui:with-horizontal (rect)
    (dolist (item across-layout)
      (if (integerp item)
          (tui:pad item)
          (tui:place (r) (render-layout-slot node item stack context r))))))

(defun render-vertical-layout (node down-layout stack context rect)
  (tui:with-vertical (rect)
    (dolist (item down-layout)
      (cond ((integerp item)
             (tui:pad item))
            ((consp item)
             (tui:place (r) (render-horizontal-layout node item stack context r)))
            (t
             (tui:place (r) (render-layout-slot node item stack context r)))))))

(defmethod render-node ((node parse:irregular-form) stack context rect &key)
  (let* ((layout (gethash (type-of node) *layouts*))
         (vertical (render-vertical-layout node (layout-rows layout) stack context rect))
         (location (car stack)))
    (make-instance 'ast-view
                   :location location
                   :children (list vertical)
                   :rect (tui:rect vertical)
                   :key-handler (global-key-handler node location context)
                   :focused (location= location (focus context)))))

(defparameter *layout-key-handlers* (make-hash-table :test #'equal)
  "Map from event to callbacks (layout, child-stack, ui) -> bool whether to propagate.")

(defmethod handle-key ((node parse:irregular-form) view location ui event)
  (declare (ignore view))
  (when-let ((layout (gethash (type-of node) *layouts*))
             (handler (gethash event *layout-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
      (unless (zerop i)
        (funcall handler layout (nthcdr (1- i) (stack ui)) ui)))))

(defun layout-rubout (layout stack ui)
  "Deletes the focused element of a list slot, or restores the slot to its template."
  (let* ((loc (car stack))
         (node (location-node loc))
         (id (location-id loc))
         (slot (ensure-car id))
         (template (layout-template layout slot)))
    (cond
      (template
       (let ((rest-loc (make-location :node node :id slot)))
         (flet ((reset ()
                  (save-history ui)
                  (edit-list rest-loc (constantly (list (funcall template)))
                             (append-id slot 0) ui stack)
                  (focus-hole-in-slot ui)
                  t))
           (cond ((symbolp id)
                  (reset))
                 ((or (= 2 (length id)) (= 3 (length id)))
                  (if (= 1 (length (elements (node-at rest-loc))))
                      (reset)
                      (progn (save-history ui)
                             (ast-delete rest-loc (second id) ui stack))))))))
      ((eq (slot-kind layout slot) 'parse::&tree)
       (save-history ui)
       (if (consp id)
           (let ((parent (make-location :node node :id (parent-id id))))
             (if (= 1 (length (elements (node-at parent))))
                 (edit-list parent (constantly nil) (location-id parent) ui stack)
                 (ast-delete parent (lastcar id) ui stack)))
           (swap-node loc (hole) ui))))))

(defun layout-space (layout stack ui)
  "Graphically inserts after, or jumps to the next adjacent hole."
  (let* ((loc (car stack))
         (node (location-node loc))
         (id (location-id loc))
         (slot (ensure-car id))
         (next (when (symbolp id) (cadr (member id (layout-slots layout)))))
         (template (layout-template layout slot)))
    (cond
      ((handle-focus-p ui loc)
       (let ((item (if template (funcall template) (hole))))
         (insert-or-jump loc 0 ui stack
                         :item item :tail (when (typep item 'ref-list) '(0)))))
      (next
       (let ((next-loc (make-location :node node :id next))
             (next-template (layout-template layout next)))
         (if (and (member (cdr (assoc next (parse:form-slot-kinds (type-of node))))
                          '(&rest &body parse::&tree))
                  (gen-list-p (node-at next-loc)))
             (insert-or-jump next-loc 0 ui stack
                             :item (if next-template (funcall next-template) (hole)))
             (refocus ui next))))
      ((and template (consp id))
       (let ((rest-loc (make-location :node node :id slot))
             (nested (typep (funcall template) 'ref-list)))
         (case (length id)
           (2 (if nested ; e.g. (body i), or (rest i) which needs traverse
                  (insert-or-jump rest-loc (1+ (second id)) ui stack
                                  :item (funcall template) :tail '(0))
                  (insert-or-jump rest-loc (1+ (second id)) ui stack)))
           (3 (when nested
                (insert-or-jump (make-location :node node :id (list slot (second id)))
                                (1+ (third id)) ui stack))))))
      ((and (eq (slot-kind layout slot) 'parse::&tree) (consp id))
       (insert-or-jump (make-location :node node :id (parent-id id))
                       (1+ (lastcar id)) ui stack)))))

(defun layout-wrap (layout stack ui)
  "Wraps the focused part of an unevaluated slot in a syntax list."
  (let ((loc (car stack)))
    (when (eq (slot-kind layout (ensure-car (location-id loc))) 'parse::&tree)
      (save-history ui)
      (wrap-in-list loc ui stack)
      t)))

(setf (gethash (tui-sys:make-event :kind #\rubout) *layout-key-handlers*) #'layout-rubout
      (gethash (tui-sys:make-event :kind #\space) *layout-key-handlers*) #'layout-space
      (gethash (tui-sys:make-event :kind #\() *layout-key-handlers*) #'layout-wrap)

(defun register-layout (layout)
  (setf (gethash (layout-type layout) *layouts*) layout
        (gethash (parse:op (make-instance (layout-type layout))) *default-expansions*)
        (lambda () (default-node layout))))

(defmacro deflayout (type templates rows)
  "Lays out forms of class `type' in `rows' of slots, where `templates' are forms building
new elements for its &rest and &body slots.
ASSUMES that &rest slots accept (slot i j) even if the list is optional like let.
ASSUMES we never focus a plain list body."
  `(register-layout
    (make-layout ',type ',rows (list ,@(loop for (slot . template) in templates
                                             collect `(cons ',slot (lambda () ,template)))))))

(deflayout parse:let*-form ((parse:vars . (ref-list (hole) (hole)))
                            (parse:body . (hole)))
  ((parse:op 1 parse:vars)
   ;; parse:decls
   (1 parse:body)))

(deflayout parse:let-form ((parse:vars . (ref-list (hole) (hole)))
                           (parse:body . (hole)))
  ((parse:op 1 parse:vars)
   (1 parse:body)))

(deflayout parse:flet-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op 1 parse:funs)
   (1 parse:body)))

(deflayout parse:labels-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op 1 parse:funs)
   (1 parse:body)))

(deflayout parse:macrolet-form
    ((parse:macro-defs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :macro-lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op 1 parse:macro-defs)
   (1 parse:body)))

(deflayout parse:symbol-macrolet-form ((parse:macro-code . (ref-list (hole) (hole)))
                                       (parse:body . (hole)))
  ((parse:op 1 parse:macro-code)
   (1 parse:body)))

(deflayout parse:block-form ((parse:body . (hole)))
  ((parse:op 1 parse:name)
   (1 parse:body)))

(deflayout parse:catch-form ((parse:body . (hole)))
  ((parse:op 1 parse:tag)
   (1 parse:body)))

(deflayout parse:throw-form ()
  ((parse:op 1 parse:tag 1 parse:result)))

(deflayout parse:return-from-form ((parse:value . (hole)))
  ((parse:op 1 parse:name 1 parse:value)))

(deflayout parse:if-form ((parse:then-else . (hole)))
  ((parse:op 1 parse:test)
   (3 parse:then-else)))

(deflayout parse:setq-form ((parse:forms . (hole)))
  ((parse:op 1 (parse:forms ((0 . 2))))))

(deflayout parse:progn-form ((parse:forms . (hole)))
  (parse:op
   (1 parse:forms)))

(deflayout parse:tagbody-form ((parse:body . (hole)))
  (parse:op
   (1 parse:body)))

(deflayout parse:go-form ()
  ((parse:op 1 parse:tag)))

(deflayout parse:the-form ()
  ((parse:op 1 parse:type-specifier 1 parse:form)))

(deflayout parse:unwind-protect-form ((parse:cleanup . (hole)))
  ((parse:op)
   (3 parse:protected)
   (1 parse:cleanup)))

(deflayout parse:multiple-value-prog1-form ((parse:body . (hole)))
  ((parse:op 1 parse:values-form)
   (1 parse:body)))

(deflayout parse:multiple-value-call-form ((parse:body . (hole)))
  ((parse:op 1 parse:fun 1 parse:arg 1 parse:body)))

(deflayout parse:progv-form ((parse:body . (hole)))
  ((parse:op 1 parse:var-list 1 parse:val-list)
   (1 parse:body)))

(deflayout parse:locally-form ((parse:body . (hole)))
  (parse:op
   (1 parse:body)))

(deflayout parse:eval-when-form ((parse:body . (hole)))
  ((parse:op 1 parse:situations)
   (1 parse:body)))

(deflayout parse:load-time-value-form ((parse:read-only-p . (hole)))
  ((parse:op 1 parse:form 1 parse:read-only-p)))

(deflayout parse:function-form ()
  ((parse:op 1 parse:fun-designator)))

(deflayout parse:quote-form ()
  ((parse:op 1 parse:thing)))

(deflayout parse:defmacro-form ()
  ((parse:op 1 parse:name)
   (1 parse:macro-code)))

;; TODO (setf f) printing
(deflayout parse:defun-form ()
  ((parse:op 1 parse:name)
   (1 parse:fun-code)))

(deflayout parse:defmethod-form ()
  ((parse:op 1 parse:name 1 parse:qualifiers)
   (1 parse:fun-code)))

(deflayout parse:lambda-form ()
  ((parse:op 1 parse:fun-code)))
;; override for nonempty lambda list
(setf (gethash 'lambda *default-expansions*)
      (lambda ()
        (make-instance 'parse:lambda-form
                       :fun-code (make-instance 'parse:function-code
                                                :lambda-list-kind :lambda
                                                :lambda-list (ref-list (hole))
                                                :body (list (hole))))))

;;
;;; function-code
;;
(defparameter *fun-code-key-handlers* (make-hash-table :test #'equal))

(defmethod handle-key ((node parse:function-code) view location ui event)
  (declare (ignore view))
  (when-let ((handler (gethash event *fun-code-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack ui))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (stack ui)) ui)))))

(setf
 (gethash (tui-sys:make-event :kind #\rubout) *fun-code-key-handlers*)
 (lambda (stack ui)
   (declare (ignore stack ui))
   t))

(setf
 (gethash (tui-sys:make-event :kind #\space) *fun-code-key-handlers*)
 (lambda (stack ui)
   (let* ((loc (car stack))
          (node (location-node loc)))
     (flet ((insert-into (id index)
              (insert-or-jump (make-location :node node :id id) index ui stack))
            (shorter-than (id limit)
              (< (length (elements (node-at (make-location :node node :id id)))) limit)))
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
        (edit-list loc (lambda (param) (ref-list param (hole)))
                   (append-id (location-id loc) 1) ui stack)
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
        (if (= 1 (length (parse:elements (node-at ll-loc))))
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
(defmethod render-node ((node parse:function-code) stack context rect &key)
  (let* ((location (car stack))
         (lambda-list-view
           (render-node (parse:lambda-list node)
                        (cons (make-location :node node :id 'parse:lambda-list) stack)
                        context rect :delimited t))
         (ll-rect (tui:rect lambda-list-view))
         (body-view
           (render-elements (parse:body node) *vertical*
                            (list-locator (make-location :node node :id 'parse:body))
                            stack context
                            (tui:clamp-rect
                             (tui:copy-rect rect :x (tui:rect-x rect)
                                                 :y (tui:rect-y2 ll-rect))
                             rect)
                            nil))
         (body-rect (tui:rect body-view)))
    (make-instance 'ast-view
                   :location location
                   :children (list lambda-list-view body-view)
                   :rect (tui:copy-rect rect
                                        :rows (+ (tui:rect-rows ll-rect)
                                                 (tui:rect-rows body-rect))
                                        :cols (max (tui:rect-cols ll-rect)
                                                   (tui:rect-cols body-rect)))
                   :key-handler (global-key-handler node location context)
                   :focused (location= location (focus context)))))

;;; global key handlers
(defun build-arglist (fname &optional old-body)
  (let* ((arglist (when (fboundp fname) (slynk-backend:arglist fname)))
         (argcount (if (listp arglist)
                       (loop for a in arglist
                             until (member a lambda-list-keywords)
                             count t)
                       0)))
    (append old-body
            (loop for i below (max 1 (- argcount (length old-body)))
                  collect (hole)))))

(defun build-call (ref &optional old-body)
  "ref must be a symbol-ref"
  (assert (typep ref 'parse:symbol-ref))
  (let ((fname (parse:resolve ref)))
    (if-let (expansion (gethash fname *default-expansions*))
      (funcall expansion)
      (let ((syntax
              (apply #'ref-list (parse:copy-node ref)
                     (build-arglist fname
                                    (mapcar (lambda (form)
                                              (parse:copy-syntax (parse:to-syntax form)))
                                            old-body)))))
        (handler-case (slog (parse:parse-syntax syntax))
          (parse:ast-parse-error () nil))))))

(defun hole-path (node id)
  "The path from `node' to the first hole at `id' or below it, entering only lists
addressed by indices."
  (let ((value (node-at (make-location :node node :id id))))
    (cond ((typep value 'hole) (list id))
          ((typep value 'parse:function-code)
           (when-let (path (or (list-hole-path value 'parse:lambda-list)
                               (list-hole-path value 'parse:body)))
             (cons id path)))
          ((and (consp id) (gen-list-p value)) (list-hole-path node id)))))

(defun list-hole-path (node id)
  (let ((value (node-at (make-location :node node :id id))))
    (assert (gen-list-p value))
    (loop for i below (length (elements value))
          thereis (hole-path node (append-id id i)))))

(defun focus-hole-in-slot (ui)
  "Focuses the first hole at or below the focused location."
  (let* ((loc (focus ui))
         (node (location-node loc))
         (id (location-id loc)))
    (when-let (path (if (symbolp id)
                        (list-hole-path node id)
                        (hole-path node id)))
      (refocus ui (first path))
      (dolist (id (rest path))
        (descend ui id)))))

(defun focus-first-hole (ui)
  (let ((node (node-at (focus ui))))
    (loop for (slot . kind) in (parse:form-slot-kinds (type-of node))
          do (when-let (path
                        (if (and (member kind '(&rest &body parse::&tree))
                                 (gen-list-p (node-at (make-location :node node :id slot))))
                            (list-hole-path node slot)
                            (hole-path node slot)))
               (dolist (id path)
                 (descend ui id))
               (return)))))

(setf (gethash (tui-sys:make-event :kind #\newline) *global-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (when-let* ((state (completion-state ui))
                    (selection (nth (selection state) (candidates state))))
          (let ((s (find-symbol selection)))
            (cond
              ((fboundp s)
               (let ((name-node (make-instance 'parse:symbol-ref
                                               :name (symbol-name s)
                                               :home-package (symbol-package s))))
                 (cond
                   ((eq 'parse:eval-form (locsort (focus ui)))
                    (when-let (call (build-call name-node))
                      (swap-node (focus ui) call ui)
                      (focus-first-hole ui)))
                   ;; operator call
                   ((and (eq (locsort (focus ui)) 'parse:fun-designator)
                         (eq (locsort (second (stack ui))) 'parse:eval-form))
                    (when-let (call (build-call name-node
                                                (parse:body (location-node (focus ui)))))
                      (swap-node (second (stack ui)) call ui)
                      (focus-first-hole ui))))))
              ((gethash s *default-expansions*)
               (let ((focus (focus ui)))
                 ;; replace the whole parent function node if editing the name
                 (swap-node (if (and (typep (location-node focus) 'parse:function-call)
                                     (eq (location-id focus) 'parse:name))
                                (second (stack ui))
                                focus)
                            (funcall (gethash s *default-expansions*))
                            ui))
               (focus-first-hole ui))
              (t
               (swap-node (focus ui)
                          (make-instance 'parse:symbol-ref
                                         :name (symbol-name s)
                                         :home-package (symbol-package s))
                          ui))))
          (setf (completion-state ui) nil)
          t)))

(setf (gethash (tui-sys:make-event :kind #\newline) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let ((ref (node-at (focus ui))))
          (when (and (typep ref 'parse:symbol-ref)
                     (eq 'parse:eval-form (locsort (focus ui))))
            (when-let (call (build-call ref))
              (swap-node (focus ui) call ui)
              (focus-first-hole ui)
              t)))))

;; convert back to hole
(setf (gethash (tui-sys:make-event :kind #\rubout) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (let ((focused (node-at (focus ui))))
          (cond ((or (typep focused 'parse:eval-form)
                     (and (typep focused 'parse:ref-list)
                          (null (elements focused))
                          (eq 'parse:eval-form (locsort (focus ui)))))
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
                       (if (< 1 (length (elements (node-at body-loc))))
                           (ast-delete body-loc (id-index id) ui)
                           ;; slots before the emptied one are untouched by the delete
                           (edit-list body-loc
                                      (lambda (body) (list-remove (elements body) (id-index id)))
                                      (move-back parent id) ui (stack ui)))
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

(defun take-selection (ui selection)
  "Reifies selection state into the cutbuffer without editing."
  (if (typep selection 'zipper)
      (setf (zipper ui) selection
            (cutbuffer ui) nil)
      (setf (cutbuffer ui) (copy-locations (selection-locations selection))
            (zipper ui) nil))
  t)

(defun empty-list-placeholder (loc)
  (let* ((node (location-node loc))
         (id (location-id loc))
         (value (node-at loc))
         (layout (gethash (type-of node) *layouts*))
         (template (when (and layout (symbolp id)) (layout-template layout id))))
    (cond (template (if (listp value)
                        (list (funcall template))
                        (ref-list (funcall template))))
          ((listp value) (list (hole)))
          (t (assert (typep value 'ref-list))
             nil))))

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
                  (edit-list (make-location :node (location-node hole) :id slot)
                             (lambda (body)
                               (let ((elts (elements body)))
                                 `(,@(subseq elts 0 index)
                                   ,@forms
                                   ,@(subseq elts (1+ index)))))
                             (append-id slot index) ui stack)))
              (progn
                (save-history ui)
                (ast-replace hole (constantly (first forms)) ui stack))))
        (multiple-value-bind (low high) (selection-range selection)
          (save-history ui)
          (let* ((list-loc (selection-list-location selection))
                 (slot (selection-parent-id selection))
                 (elts (elements (node-at list-loc)))
                 (new (or `(,@(subseq elts 0 low) ,@(subseq elts (1+ high)))
                          (empty-list-placeholder list-loc))))
            (edit-list list-loc (constantly new)
                       (if (elements new) (append-id slot (max 0 (1- low))) slot)
                       ui (stack ui)))))
    t))

(setf (gethash (tui-sys:make-event :kind #\c :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (if-let (selection (selection-at-focus ui))
          (take-selection ui selection)
          (let ((loc (focus ui)))
            (when (locsort loc)
              (setf (cutbuffer ui) (copy-locations (list loc))
                    (zipper ui) nil))))
        t))

(setf (gethash (tui-sys:make-event :kind #\x :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        (if-let (selection (selection-at-focus ui))
          (cut-selection ui selection)
          (let ((loc (focus ui)))
            (when (locsort loc)
              (setf (cutbuffer ui) (copy-locations (list loc))
                    (zipper ui) nil)
              (swap-node loc (hole) ui))))
        t))

(defun paste-conversion (from to)
  "Controls copying between location sorts."
  (cond ((and from (equal from to)) #'parse:copy-node)
        ((and (eq from 'parse:eval-form) (eq to 'parse:unevaluated))
         (lambda (form) (parse:to-syntax (parse:copy-node form))))))

(defun paste-forms (ui buffer)
  "Pastes the forms of `buffer' after the focused list element, over the selection at
the focus, or over a focused hole.
Each form is checked for convertibility against the sort of the position it's pasted into,
forms past the end of a selection taking the sort of its last position."
  (let* ((focus (focus ui))
         (id (location-id focus))
         (selection (when-let (selection (selection-at-focus ui))
                      (when (= (id-index id) (selection-range selection))
                        selection)))
         (targets (if selection
                      (mapcar #'locsort (selection-locations selection))
                      (list (locsort focus))))
         (converts (loop for sort in (slog (cutbuffer-sorts buffer))
                         for i from 0
                         collect (paste-conversion
                                  sort (nth (min i (1- (length targets))) targets)))))
    (when (every #'identity converts)
      (let ((forms (mapcar #'funcall converts (cutbuffer-forms buffer))))
        (cond ((bodylike-id id)
               (multiple-value-bind (start end)
                   (cond (selection
                          (multiple-value-bind (low high) (selection-range selection)
                            (values low (1+ high))))
                         ((typep (node-at focus) 'parse:hole)
                          (values (id-index id) (1+ (id-index id))))
                         (t
                          (values (1+ (id-index id)) (1+ (id-index id)))))
                 (let ((slot (parent-id id)))
                   (save-history ui)
                   (setf (selection ui) nil)
                   (edit-list (make-location :node (location-node focus) :id slot)
                              (lambda (body)
                                (let ((elts (elements body)))
                                  `(,@(subseq elts 0 start) ,@forms ,@(subseq elts end))))
                              (append-id slot (+ start (length forms) -1)) ui (stack ui)))))
              ((= 1 (length forms))
               (swap-node focus (first forms) ui)))))))

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
            (edit-list (selection-list-location selection)
                       (lambda (body)
                         (let ((elts (elements body)))
                           `(,@(subseq elts 0 low)
                             ,(fill-zipper zipper forms)
                             ,@(subseq elts (1+ high)))))
                       (append-id slot low) ui (stack ui))))
        (when (eq 'parse:eval-form (locsort focus))
          (swap-node focus (fill-zipper zipper (list (node-at focus))) ui)))
    t))

(setf (gethash (tui-sys:make-event :kind #\v :controlp t) *default-key-handlers*)
      (lambda (view ui)
        (declare (ignore view))
        ;; these are mutually exclusive as cutting a zipper nulls the cutbuffer
        (cond ((zipper ui) (paste-zipper ui (zipper ui)))
              ((cutbuffer ui) (paste-forms ui (cutbuffer ui))))
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
      (tui:with-vertical ((tui:clamp-rect (tui:make-rect :x (tui:rect-x anchor-rect)
                                                         :y (tui:rect-y2 anchor-rect)
                                                         :rows (tui:rows ui)
                                                         :cols (max maxlen (tui:cols ui)))
                                          (ui-rect ui)))
        (loop for candidate in (candidates state)
              for index from 0
              until (tui:full)
              do (tui:place (r) (render-completion candidate index state r)))))))

;;
;;; main loop
;;

(defmethod tui:render ((ui ui))
  ;; do not allow use of old caches
  (clrhash (node-views ui))
  (clrhash (redisplay-cache ui))
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
     'ordered-view
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
  (let* ((ast (parse:parse-from-string
               "(lambda (a &key (b a supplied-p))
                  (loop for i from 1 to 10 do (print i)))"))
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
  (if (interactive-stream-p *standard-output*)
      (tui-main)
      (progn
        (bt:make-thread (lambda () (tui-main)))
        (loop :for (form . value) = (sb-concurrency:receive-message *log*)
              :until (eq value *log-stop*)
              :do (if form
                      (format t "~a~%|> ~s~%" form value)
                      (format t "~a~%" value))
                  (force-output)))))
