;;;;
;;;; terminal frontend
;;;;
;;

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
(defvar *state* #.nil)
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

(defrecord ordered-view (tui:view)
  ()
  (:documentation "A mixin view whose children must be in reading order. Checked at
construction, do not modify."))

(defmethod initialize-instance :after ((view ordered-view) &key)
  (assert (reading-order-p (tui:children view)) ()
          "Children of ~a are not in reading order: ~a" view (tui:children view)))

(defrecord ast-view (ordered-view)
  ((location :mutable t :type location)
   (view-stack :init nil :mutable t :type list)
   (hoverable :init nil :mutable t :type boolean)))

(defgeneric handle-key (node view location ui event)
  (:documentation "event is a uncursed key event. If this node is active then
adjust cursor state in the context, specialized for the `cursor'.
If this returns NIL, propagate up the cursor stack.")
  (:method (node view location context event) nil))

(defrecord completion-state ()
  ((anchor)
   (candidates :type list)
   (selection :init 0 :type (integer 0))))

(defun copy-completion-state (state &key (anchor (anchor state))
                                        (candidates (candidates state))
                                        (selection (selection state)))
  (make-completion-state :anchor anchor :candidates candidates :selection selection))

(defrecord selection ()
  ((buffer-id)
   (context :type list)
   (location :type location)
   (point :type integer)
   (activep :init t :type boolean))
  (:representation :struct))

(defrecord zipper (selection)
  ((stack :type list))
  (:representation :struct))

(defun copy-selection (selection &key (point (selection-point selection))
                                   (activep (selection-activep selection))
                                   (stack (when (typep selection 'zipper)
                                            (zipper-stack selection))))
  "Returns a selection with updated fields. A nonempty `stack' makes it into a zipper."
  (apply (if stack #'make-zipper #'make-selection)
         :buffer-id (selection-buffer-id selection)
         :context (selection-context selection)
         :location (selection-location selection)
         :point point :activep activep
         (when stack (list :stack stack))))

(defrecord undo-state ()
  ((ast)
   (stack :type list))
  (:representation :struct))

(defun end-selection-mode (buffer)
  (when-let (selection (selection buffer))
    (setf (selection buffer) (copy-selection selection :activep nil))))

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

(defun zipper-top-p (buffer stack)
  (when-let (selection (selection buffer))
    (and (typep selection 'zipper)
         (eq (node-at (car stack)) ; top of zipper
             (location-node (lastcar (zipper-stack selection)))))))

(defun zipper-depth (zipper)
  (length (zipper-stack zipper)))

(defun location-selected-p (buffer stack)
  (when-let* ((selection (selection buffer))
              (id (location-id (car stack))))
    (and (eq (location-node (selection-location selection))
             (location-node (car stack)))
         (bodylike-id id)
         (equal (selection-parent-id selection) (parent-id id))
         (multiple-value-bind (low high)
             (selection-range selection)
           (<= low (id-index id) high)))))

(defun selection-at-focus (buffer)
  (when (location-selected-p buffer (stack buffer))
    (selection buffer)))

(defrecord cutbuffer ()
  ((forms :type list)
   (sorts :type list))
  (:representation :struct))

;; computed during redisplay from current toplevel
(defrecord segment ()
  ((index :type (integer 0))
   (start :type (integer 0))
   (rows :type (integer 1))
   (buffer :type (array tui::cell (* *)))
   (view :type tui:view))
  (:representation :struct))

;; segment index + relative row offset
(defrecord scroll-position ()
  ((index :type (integer 0))
   (row :init 0 :type (integer 0)))
  (:representation :struct))

(defrecord scroll-state ()
  ((position :type scroll-position)
   ;; total number of rows
   (rows :type (integer 0))
   ;; start of viewport
   (viewport-row :type (integer 0))
   (segments :type list))
  (:representation :struct))

(defun copy-locations (locations)
  (make-cutbuffer :forms (mapcar (lambda (loc) (parse:copy-node (node-at loc))) locations)
                  :sorts (mapcar #'locsort locations)))

(defrecord pane-drag ()
  ((buffer-id) (offset-x :type integer) (offset-y :type integer))
  (:representation :struct))

(defrecord ui (tui:elemental)
  ((pane-drag :init nil :mutable t :type (or null pane-drag))
   (buffers :init nil :mutable t :type list)
   (focused-buffer :init nil :mutable t)
   (selection :init nil :mutable t :type (or null selection))
   (zipper :init nil :mutable t :type (or null zipper))
   (cutbuffer :init nil :mutable t :type (or null cutbuffer))
   (redisplayed-since-input :init nil :mutable t :type boolean)
   (revision :init 0 :mutable t :type (integer 0)))
  (:representation :class))

;; MUST BE shallow copyable, always update immutably
(defrecord buffer-state ()
  ((floating :init nil :mutable t :type boolean)
   (buffer-package :init *package* :mutable t :type package)
   (buffer-source :init nil :mutable t :type (or null pathname))
   (buffer-x :init 0 :mutable t :type (integer 0))
   (buffer-y :init 0 :mutable t :type (integer 0))
   (ast :mutable t)
   (edit-loc :init nil :mutable t :type (or null location))
   (goal-col :init 1 :mutable t :type positive-fixnum)
   (completion-state :init nil :mutable t :type (or null completion-state))
   (history :init nil :mutable t :type list)
   (future :init nil :mutable t :type list)
   (stack :init nil :mutable t :type list)
   (goal-stacks :init nil :mutable t :type list))
  (:representation :class))

(defun copy-buffer-state (state)
  (shallow-copy-record state))

;; mutable per-render cache, needed for movement commands
(defrecord render-state ()
  ((floating-rect :init nil :mutable t :type (or null tui:rect))
   (buffer-view :init nil :mutable t :type (or null tui:view))
   (stage-cache :init (make-hash-table :test #'eq) :type hash-table)
   (redisplay-cache :init (make-hash-table :test #'equal) :type hash-table)
   (scroll-state :init (make-scroll-state :position (make-scroll-position :index 0)
                                          :rows 0 :viewport-row 0 :segments nil)
                 :mutable t :type scroll-state)
   (focus-rect :init nil :mutable t :type (or null tui:rect))
   (node-views :init (make-hash-table) :type hash-table))
  (:representation :class))

(defrecord buffer ()
  ((buffer-ui)
   (buffer-id :init (gensym "BUFFER-"))
   (buffer-state :mutable t :type buffer-state)
   (render-state :init (make-render-state) :mutable t :type render-state))
  (:representation :class))

(macrolet ((buffer-state-accessors (state readers &rest accessors)
             `(progn
                ,@(loop for name in (append readers accessors)
                        collect `(defmethod ,name ((buffer buffer))
                                   (,name (,state buffer))))
                ,@(loop for name in accessors
                        collect `(defmethod (setf ,name) (value (buffer buffer))
                                   (setf (,name (,state buffer)) value))))))

  (buffer-state-accessors
   buffer-state ()
   floating buffer-package buffer-source buffer-x buffer-y ast edit-loc goal-col
   completion-state
   history future stack goal-stacks)
  (buffer-state-accessors
   render-state (stage-cache redisplay-cache node-views)
   floating-rect buffer-view scroll-state focus-rect))

(defun add-buffer (ui &key (ast (list (hole))) (x 0) (y 0)
                        source (package "CL-USER") floating)
  "Adds a buffer containing `ast' to `ui', optionally associated with `source'."
  (check-type ast cons)
  (let ((buffer (make-buffer
                 :buffer-ui ui
                 :buffer-state (make-buffer-state
                                :floating floating :ast ast
                                :buffer-source (when source (pathname source))
                                :buffer-x x :buffer-y y
                                :buffer-package (or (find-package package)
                                                    (error "Unknown package ~s" package))))))
    (setf (stack buffer) (list (make-location :node ast :id 0))
          (buffers ui) (append (buffers ui) (list buffer)))
    (unless (focused-buffer ui)
      (setf (focused-buffer ui) buffer))
    buffer))

(defun buffer-focused-p (buffer)
  (eq (buffer-id buffer) (buffer-id (focused-buffer (buffer-ui buffer)))))

(defmethod (setf focused-buffer) :before (buffer (ui ui))
  (assert (member buffer (buffers ui)))
  (setf (redisplayed-since-input ui) nil)
  (when (and (not (eq buffer (focused-buffer ui))) (selection ui))
    (setf (selection ui) (copy-selection (selection ui) :activep nil))))

(defmethod selection ((buffer buffer))
  (let ((selection (selection (buffer-ui buffer))))
    (when (and selection
               (eq (buffer-id buffer) (selection-buffer-id selection)))
      selection)))
(defmethod (setf selection) (value (buffer buffer))
  (setf (selection (buffer-ui buffer)) value))

(defmethod zipper ((buffer buffer)) (zipper (buffer-ui buffer)))
(defmethod (setf zipper) (value (buffer buffer))
  (setf (zipper (buffer-ui buffer)) value))

(defmethod cutbuffer ((buffer buffer)) (cutbuffer (buffer-ui buffer)))
(defmethod (setf cutbuffer) (value (buffer buffer))
  (setf (cutbuffer (buffer-ui buffer)) value))

(defun focus (buffer)
  (car (stack buffer)))

(defun refocus (buffer id)
  "Moves the focus to another location in the node it is already within."
  (end-selection-mode buffer)
  (let ((loc (make-location :node (location-node (focus buffer)) :id id)))
    (setf (stack buffer) (cons loc (cdr (stack buffer))))
    loc))

(defun descend (buffer id)
  "Moves the focus to a location-id inside the currently focused node."
  (end-selection-mode buffer)
  (let ((loc (make-location :node (node-at (focus buffer)) :id id)))
    (push loc (stack buffer))
    loc))

(defun parent-stack (stack)
  "relies on ids being either 'symbol or ('symbol integer*) for list addressing.
Plain list bodies are skipped since they are never focused."
  (let* ((loc (car stack))
         (id (location-id loc)))
    (cond ((and (listp id)
                (listp (node-at (make-location :node (location-node loc)
                                               :id (parent-id id)))))
           (cdr stack))
          ((listp id)
           (cons (make-location :node (location-node loc) :id (parent-id id))
                 (cdr stack)))
          ;; do not reach toplevel
          ((= 1 (length stack)) nil)
          (t
           (cdr stack)))))

;;
;;; editing operations
;;
;; - a stack tracks locations from the focus outward to the indexed top-level form
;; - ast-delete/insert/replace are stack-respecting edit operations
;;
(defun rebuild-spine (loc updater stack)
  "Requires that (location-node `loc') = (location-node (car `stack'))
Functionally rebuilds the path from `loc' out to the root, applying `updater' to
the value at `loc', but retaining the current focus. Returns the new stack and root."
  (assert (eq (location-node loc) (location-node (car stack))))
  (labels ((rebuild (stack newnode newstack)
             (if (null stack)
                 (values (nreverse newstack) newnode)
                 (let* ((this (car stack))
                        (id (location-id this))
                        (new-parent (update (location-node this) id newnode)))
                   (rebuild (cdr stack)
                            new-parent
                            (cons (make-location :node new-parent :id id)
                                  newstack))))))
    (rebuild (cons loc (cdr stack))
             (funcall updater (node-at loc))
             (list))))

(defun commit-edit (buffer stack root)
  (end-selection-mode buffer)
  (setf (ast buffer) root
        (stack buffer) stack)
  (focus buffer))

(defun save-history (buffer)
  (push (make-undo-state :stack (stack buffer) :ast (ast buffer))
        (history buffer)))

(defun ast-replace (loc updater buffer &optional (stack (stack buffer)))
  (if (typep (location-node loc) 'parse:macro-call)
      (replace-in-macro-call loc (funcall updater (node-at loc)) buffer)
      (multiple-value-call #'commit-edit buffer (rebuild-spine loc updater stack))))

(defun edit-list (loc updater focus-id buffer stack)
  "Replaces the list at `loc' using `updater' and focuses `focus-id'."
  (if (typep (location-node loc) 'parse:macro-call)
      (replace-in-macro-call loc (funcall updater (node-at loc)) buffer focus-id)
      (progn (multiple-value-call #'commit-edit buffer (rebuild-spine loc updater stack))
             (refocus buffer focus-id))))

(defun ast-insert (item loc index buffer &optional (stack (stack buffer)) tail)
  "Focuses the inserted item, optionally descends the path `tail'."
  (edit-list loc (lambda (body) (list-insert (elements body) item index))
             (reduce #'append-id (cons index tail) :initial-value (location-id loc))
             buffer stack))

(defun ast-delete (loc index buffer &optional (stack (stack buffer)))
  "assumes that list has length > 1"
  (edit-list loc (lambda (body) (list-remove (elements body) index))
             (append-id (location-id loc) (max 0 (1- index)))
             buffer stack))

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

(defun insert-or-jump (loc index buffer stack &key (item (hole)) tail)
  "Moves to the hole at `index' of the list at `loc' and down the indices `tail' if passed,
otherwise inserts `item' at `index' and focuses down `tail' within it. `stack' is the
part of the buffer stack starting from the node of `loc'."
  (let ((path (cons index tail)))
    (if (typep (value-at-path loc path) 'parse:hole)
        (progn
          (setf (stack buffer) stack)
          (refocus buffer (reduce #'append-id path :initial-value (location-id loc))))
        (progn
          (save-history buffer)
          (ast-insert item loc index buffer stack tail)))))

(defun replace-in-macro-call (location newnode buffer &optional focus-id)
  "Replaces the syntax at `location' in a macro call, which may reparse part of it into a
form. Pop the cursor to the call's location so the parsed call can be inserted,
then descend to the original cursor position, or to the syntax at `focus-id' if given."
  (let* ((call (location-node location))
         (outer (cdr (member-if (lambda (l) (eq (location-node l) call)) (stack buffer)))))
    (multiple-value-bind (new-call path)
        (parse:update call (location-id location) newnode)
      (multiple-value-call #'commit-edit buffer
        (rebuild-spine (car outer) (constantly new-call) outer))
      ;;
      (dolist (id (if focus-id
                      (parse:path-at new-call focus-id)
                      (or path ; translated path or vvv fast path
                          (list (location-id location)))))
        (push (make-location :node (node-at (focus buffer)) :id id) (stack buffer)))
      (focus buffer))))

(defun swap-node (location newnode buffer)
  "saves undo history"
  (when (eq (location-id location) :open)
    (setf location (second (member location (stack buffer) :test #'location=))))
  (when (or (null (edit-loc buffer)) (not (location= location (edit-loc buffer))))
    (save-history buffer))
  ;; perform the insertion
  (let ((newloc (if (typep (location-node location) 'parse:macro-call)
                    (replace-in-macro-call location newnode buffer)
                    (ast-replace location (constantly newnode) buffer
                                 (member-if (lambda (l) (eq (location-node location)
                                                       (location-node l)))
                                            (stack buffer))))))
    (setf (future buffer) nil
          (edit-loc buffer) newloc)))

;;; rendering helpers
(defgeneric render-node (node stack context rect &key &allow-other-keys))

(defmacro with-redisplay-cache ((buffer key) &body body)
  "The value of `body', computed once per redisplay of `buffer' for `key'."
  (once-only (buffer key)
    (with-gensyms (value found)
      `(multiple-value-bind (,value ,found) (gethash ,key (redisplay-cache ,buffer))
         (if ,found
             ,value
             (setf (gethash ,key (redisplay-cache ,buffer)) (progn ,@body)))))))

(defun environment-at (stack)
  (let ((env (parse:make-env)))
    (dolist (loc (reverse stack) env)
      (when (member (locsort loc) '(parse:eval-form parse:function-code))
        (setf env (parse:location-env (location-node loc) (location-id loc) env))))))

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

(defun compute-focus-binder (node stack buffer)
  (declare (ignore buffer))
  (let ((loc (car stack)))
    (when (lexical-symbol-ref node loc)
      (let ((kind (cond ((function-position-p loc) :function)
                        ((block-position-p loc) :block)
                        (t :variable)))
            (env (environment-at stack)))
        (multiple-value-bind (entry kind) (parse:env-lookup node kind env)
          (let ((binder (ensure-car entry)))
            (when (and (eq kind :lexical) (typep binder 'parse:binder)) binder)))))))

(defun focus-binder (buffer)
  "The binder referenced by the symbol-ref under the cursor."
  (with-redisplay-cache (buffer :focus-binder)
    (compute-focus-binder (node-at (focus buffer)) (stack buffer) buffer)))

(defun symbol-ref-boundp (node stack buffer)
  (declare (ignore buffer))
  (or (eq :lexical
          (nth-value 1 (parse:env-lookup node
                                         (cond ((function-position-p (car stack)) :function)
                                               ((block-position-p (car stack)) :block)
                                               (t :variable))
                                         (environment-at stack))))
      ;; blocks are never global
      (and (not (block-position-p (car stack)))
           (multiple-value-bind (symbol found)
               (parse:resolve node)
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
         (view (first vals)))
    (setf (gethash node (node-views context)) view)
    ;; occasionally draws body lists
    (when (typep view 'ast-view)
      (setf (view-stack view) stack))
    (when (location= (car stack) (focus context))
      (setf (gethash :focus-view (redisplay-cache context)) view))
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

(defun global-key-handler (node location)
  "Handler receives its working buffer explicitly at dispatch time."
  (lambda (view event buffer)
    (slog event)
    (assert (location= location (focus buffer)))
    (slog (butlast (stack buffer))) ; suppress toplevel list
    (or (when-let (handler (gethash event *global-key-handlers*))
          (funcall handler view buffer))
        ;; propagate
        (loop for thisnode = node then (location-node location)
              for location in (stack buffer)
              thereis (when (handle-key thisnode view location buffer event)
                        (slog* `(handled-at ,node))
                        t))
        ;; contextual handlers
        (when-let (handler (gethash event *default-key-handlers*))
          (slog* "default handler called")
          (funcall handler view buffer)))
    ;; state updates here
    (when-let (completion (completion-state buffer))
      (setf (completion-state buffer)
            (when (eq (node-at (focus buffer)) (anchor completion))
              (let* ((name (parse:name (anchor completion)))
                     (valid (delete-if-not (lambda (s) (starts-with-subseq name (string s)))
                                           (copy-list (candidates completion)))))
                (copy-completion-state
                 completion
                 :candidates (if (find name valid :test #'string=)
                                 (sort valid #'<
                                       :key (lambda (s)
                                              (mk-string-metrics:damerau-levenshtein
                                               name (string s))))
                                 valid))))))
    (assert (not (plain-body-location-p (focus buffer))) ()
            "focused the body ~a of ~a" (location-id (focus buffer))
            (type-of (location-node (focus buffer))))))

(defun fname (node)
  (parse:name (parse:name node)))

;; note: assumes length 1 symbol mapping for <= and >=
(defparameter *arb-arity-binops* #(/ * + - < > <= >= = /=))
(defun binop-precedence (op-ref)
  (trivia:ematch (parse::ref-coerce-symbol op-ref)
    ((or *) 3)
    ((or / + -) 2)
    ((or < > <= >= = /=) 1)))

(defun is-binop-call (node)
  (and (typep node 'parse:function-call)
       (typep (parse:name node) 'parse:symbol-ref)
       (find (parse:resolve (parse:name node)) *arb-arity-binops*)))

(defun wrap-arith (op buffer)
  "take first arithmetic operator with greater precedence than `op'
if none, surround current atom"
  (when (eq (locsort (car (stack buffer))) 'parse:eval-form)
    (loop for stack = (stack buffer) then (cdr stack)
          for loc in (stack buffer)
          for node = (location-node loc)
          while (and (is-binop-call node)
                     (> (binop-precedence (parse:name node)) (binop-precedence op)))
          finally (let ((sym (make-instance 'parse:symbol-ref
                                            :name (symbol-name op)
                                            :home-package (symbol-package op)))
                        (loc (car stack)))
                    (swap-node loc
                               (make-instance 'parse:function-call
                                              :name sym
                                              :body (list (node-at loc) (hole)))
                               buffer)
                    (descend buffer '(parse:body 1)))
                  (return t))))

(loop for sym across *arb-arity-binops*
      for s = (string s)
      do (when (= 1 (length (string s)))
           (setf (gethash (tui-sys:make-event :kind (schar s 0) :altp t)
                          *global-key-handlers*)
                 ;; oops old bug if we don't compute from s prior to its mutation by loop
                 (lambda (view buffer)
                   (declare (ignore view))
                   (wrap-arith sym buffer)))))

(setf (gethash (tui-sys:make-event :kind #\i :controlp t) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (when-let (state (completion-state buffer))
          (setf (completion-state buffer)
                (copy-completion-state
                 state :selection (mod (1+ (selection state))
                                       (length (candidates state))))))))

(setf (gethash (tui-sys:make-event :kind #\tab :shiftp t) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (when-let (state (completion-state buffer))
          (setf (completion-state buffer)
                (copy-completion-state
                 state :selection (mod (1- (selection state))
                                       (length (candidates state))))))))


;; ASSUME leaf nodes are non-overlapping
;; we index them into an array of blocks sorted by line then column
(defun build-atom-array (tree rows)
  (let ((rows (make-array rows :initial-element (list))))
    (tui:view-traverse tree (lambda (view)
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

(defun atom-move (view-finder view buffer)
  (end-selection-mode buffer)
  (let* ((rows (scroll-state-rows (scroll-state buffer)))
         (atom-array (build-atom-array (buffer-view buffer) rows))
         (this-rect (tui:rect view)))
    (multiple-value-bind (new-view new-goal)
        (funcall view-finder atom-array this-rect)
      (when new-view ; assumes atoms are all ast-views
        (setf (stack buffer) (view-stack new-view))
        (when new-goal
          (slog* (format nil "goal col is ~d" new-goal))
          (setf (goal-col buffer) new-goal))))))

(defun move-down (view buffer)
  (atom-move (lambda (atom-array this-rect)
               (view-below atom-array (1+ (tui:rect-y this-rect)) (goal-col buffer)))
             view buffer))

(setf (gethash (tui-sys:make-event :kind #\n :controlp t) *default-key-handlers*)
      #'move-down)
(setf (gethash (tui-sys:make-event :kind :down-arrow) *default-key-handlers*)
      #'move-down)

(defun move-up (view buffer)
  (atom-move (lambda (atom-array this-rect)
               (view-above atom-array (tui:rect-y this-rect) (goal-col buffer)))
             view buffer))

(setf (gethash (tui-sys:make-event :kind #\p :controlp t) *default-key-handlers*)
      #'move-up)
(setf (gethash (tui-sys:make-event :kind :up-arrow) *default-key-handlers*)
      #'move-up)

(defun move-left (view buffer)
  (atom-move
   (lambda (atom-array this-rect)
     (let ((view (view-left atom-array
                            (tui:rect-y this-rect) (tui:rect-x this-rect))))
       (values view (when view (tui:rect-x2 (tui:rect view))))))
   view buffer))

(setf (gethash (tui-sys:make-event :kind #\b :controlp t) *default-key-handlers*)
      #'move-left)
(setf (gethash (tui-sys:make-event :kind :left-arrow) *default-key-handlers*)
      #'move-left)

(defun move-right (view buffer)
  (atom-move
   (lambda (atom-array this-rect)
     (let ((view (view-right atom-array
                             (tui:rect-y this-rect) (tui:rect-x2 this-rect))))
       (values view (when view (tui:rect-x2 (tui:rect view))))))
   view buffer))

(setf (gethash (tui-sys:make-event :kind #\f :controlp t) *default-key-handlers*)
      #'move-right)
(setf (gethash (tui-sys:make-event :kind :right-arrow) *default-key-handlers*)
      #'move-right)

(defun selection-list-location (selection)
  (make-location :node (location-node (selection-location selection))
                 :id (selection-parent-id selection)))

(defun current-selection (buffer)
  (when-let (selection (selection buffer))
    (and (selection-activep selection) selection)))

(defun begin-selection (buffer)
  (let* ((loc (focus buffer))
         (id (location-id loc)))
    (when (bodylike-id id)
      (setf (selection buffer)
            (make-selection :buffer-id (buffer-id buffer)
                            :context (stack buffer)
                            :location loc
                            :point (id-index id))))))

(defun extend-selection (buffer delta)
  (when-let (selection (or (current-selection buffer) (begin-selection buffer)))
    (let ((point (+ (selection-point selection) delta)))
      (when (<= 0 point (1- (length (elements
                                     (node-at (selection-list-location selection))))))
        ;; when the underlying range is reselected, forget the zipper
        ;; keep selection active after move
        (refocus buffer (append-id (selection-parent-id selection) point))
        (setf (selection buffer)
              (copy-selection selection :point point :activep t :stack nil))))
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

(defun expand-selection (buffer)
  (when-let (selection (or (current-selection buffer) (begin-selection buffer)))
    (let ((depth (if (typep selection 'zipper) (zipper-depth selection) 0)))
      ;; skip positions where we cannot copy a usefully pastable zipper
      ;; e.g. nonevaluated let bindings need more sophisticated sort tracking
      (loop for location in (nthcdr (1+ depth) (selection-context selection))
            for level from (1+ depth)
            do (when (zipper-target-p location)
                 (let ((stack (subseq (selection-context selection) 0 level)))
                   (setf (selection buffer) (copy-selection selection :stack stack)))
                 (return t))))))

(defun shrink-selection (buffer)
  (when-let (selection (current-selection buffer))
    (when (typep selection 'zipper)
      (loop for level from (1- (zipper-depth selection)) downto 1
            for location = (nth level (selection-context selection))
            do (when (zipper-target-p location)
                 (setf (selection buffer)
                       (copy-selection
                        selection :stack (subseq (selection-context selection) 0 level)))
                 (return t))
            finally ;; normal selection
                    (setf (selection buffer) (copy-selection selection :stack nil))
                    (return t)))))

(setf (gethash (tui-sys:make-event :kind :up-arrow :shiftp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (expand-selection buffer)))

(setf (gethash (tui-sys:make-event :kind :down-arrow :shiftp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (shrink-selection buffer)))

(setf (gethash (tui-sys:make-event :kind :left-arrow :shiftp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (extend-selection buffer -1)))

(setf (gethash (tui-sys:make-event :kind :right-arrow :shiftp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (extend-selection buffer 1)))

(defun goal-valid-p (buffer)
  "Can we still access the same children through the last saved goal?"
  (when-let (goal (car (goal-stacks buffer)))
    (let ((parent (parent-stack goal))
          (stack (stack buffer)))
      (and (= (length parent) (length stack))
           (every #'location= parent stack)))))

(defun move-parent (buffer)
  (end-selection-mode buffer)
  (let ((old (stack buffer)))
    (when-let (new-stack (parent-stack old))
      (slog* `(moving to ,(butlast new-stack)))
      (setf (goal-stacks buffer) (cons old (when (goal-valid-p buffer)
                                             (goal-stacks buffer)))
            (stack buffer) new-stack))))

(defun move-child (buffer)
  "Steps back down into the goal child."
  (end-selection-mode buffer)
  (when (goal-valid-p buffer)
    (setf (stack buffer) (pop (goal-stacks buffer)))))

(setf (gethash (tui-sys:make-event :kind #\p :altp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (move-parent buffer)))

(setf (gethash (tui-sys:make-event :kind #\n :altp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (move-child buffer)))

(setf (gethash (tui-sys:make-event :kind #\P :altp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        ;; this is really a do-while idiom
        (prog1 (move-parent buffer)
          (loop for last-stack = nil then stack
                for stack = (slog (stack buffer))
                for loc = (car stack)
                for node = (node-at loc)
                while (typep node 'parse:eval-form)
                do (move-parent buffer)
                finally (when (slog last-stack)
                          (setf (stack buffer) last-stack))))))

;;; undo

;; can't pop the one remaining thing
(defun undo (buffer)
  (unless (null (history buffer))
    (let ((prev (pop (history buffer))))
      (slog 'undo)
      (push (make-undo-state :ast (ast buffer) :stack (stack buffer))
            (future buffer))
      (setf (ast buffer) (undo-state-ast prev)
            (stack buffer) (undo-state-stack prev)))))

(defun redo (buffer)
  (unless (null (future buffer))
    (let ((next (pop (future buffer))))
      (slog 'redo)
      (push (make-undo-state :ast (ast buffer) :stack (stack buffer))
            (history buffer))
      (setf (ast buffer) (undo-state-ast next)
            (stack buffer) (undo-state-stack next)))))

(setf (gethash (tui-sys:make-event :kind #\u :controlp t) *global-key-handlers*)
      (lambda (view buffer) view (undo buffer)))
(setf (gethash (tui-sys:make-event :kind #\r :controlp t) *global-key-handlers*)
      (lambda (view buffer) view (redo buffer)))

;;; hole
;; note: holes only replace symbols or evaluation contexts

(defmethod render-node ((node hole) stack context rect &key)
  (with-accessors ((text text)) node
    (let* ((location (car stack))
           (text (if (string= text "") "hole" text))
           (focused (location= location (focus context))))
      (tui:puts* text 1 1 rect (if (and focused (buffer-focused-p context))
                                   (tui:make-style :fg #x0 :underlinep t)
                                   (tui:make-style :fg (tui:color 30 200 0) :underlinep t)))
      (make-instance 'ast-view
                     :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                     :location location
                     :hoverable t
                     ;; leaf handler never called unless focused
                     :key-handler (when focused
                                    (global-key-handler node location))
                     :focused focused))))

(defun is-regular-char-event (event)
  (and (characterp (tui:event-kind event))
       (not (or (tui:event-controlp event)
                (tui:event-altp event) (tui:event-metap event)))))

(defun completion-candidates (context)
  (declare (ignore context))
  (loop for s being the symbols of (find-package "CL") collect s))

(defun symbol-char-p (c)
  (or (alphanumericp c) (find c "+-*/=<>!?&%$_~^.:@[]{}")))

(defmethod handle-key ((node hole) view location buffer event)
  (let ((c (tui:event-kind event)))
    (when (is-regular-char-event event)
      (trivia:match (slog (locsort location))
        ((eql 'parse:binder)
         (when (and (symbol-char-p c) (not (digit-char-p c)))
           (swap-node location (make-instance 'parse:binder
                                              :name (string-upcase c)
                                              :home-package (buffer-package buffer))
                      buffer)))
        ((eql 'parse:eval-form)
         (cond
           ((and (char= c #\() (not (typep (location-node location) 'parse:macro-call)))
            (swap-node location (ref-list) buffer))
           ((and (symbol-char-p c) (not (digit-char-p c)))
            (let ((newnode (make-instance 'parse:symbol-ref
                                          :name (string-upcase c)
                                          :home-package (buffer-package buffer))))
              (swap-node location newnode buffer)))
           ((digit-char-p c)
            (let ((newnode (make-instance 'parse:literal :str (string c))))
              (swap-node location newnode buffer)))
           ;; needs to deal with comma depth and insertion
           ;; ((char= c #\`)
           ;;  (let ((newnode (make-instance 'parse:quasiquote-form :thing (hole))))
           ;;    (swap-node location newnode buffer)
           ;;    (focus-first-hole buffer)
           ;;    t))
           ((char= c #\')
            (let ((newnode (make-instance 'parse:quote-form :op 'parse:read-quote
                                                            :thing (hole))))
              (swap-node location newnode buffer)
              (focus-first-hole buffer)
              t))))
        ((or (eql 'parse:fun-designator) (eql 'parse:symbol-ref))
         (when (and (symbol-char-p c) (not (digit-char-p c)))
           (let ((newnode (make-instance 'parse:symbol-ref
                                         :name (string-upcase c)
                                         :home-package (buffer-package buffer))))
             (swap-node location newnode buffer))))
        ((eql 'parse:unevaluated)
         (cond
           ((char= c #\()
            (swap-node location (ref-list (hole)) buffer)
            (refocus buffer (append-id (location-id (focus buffer)) 0)))
           ((and (symbol-char-p c) (not (digit-char-p c)))
            (swap-node location
                       (make-instance 'parse:symbol-ref
                                      :name (string-upcase c)
                                      :home-package (buffer-package buffer))
                       buffer))
           ((digit-char-p c)
            (swap-node location (make-instance 'parse:literal :str (string c)) buffer))))))))

;;; symbol-references
(defmethod handle-key ((node parse:symbol-ref) view location buffer event)
  (when (is-regular-char-event event)
    (let ((s (parse:name node))
          (c (tui:event-kind event)))
      (cond ((and (symbol-char-p c) (not (char= c #\:)))
             (let ((newnode
                     (make-instance 'parse:symbol-ref
                                    :name (format nil "~a~a" s (string-upcase c))
                                    :home-package (parse:home-package node))))
               (swap-node location newnode buffer)
               (if-let (state (completion-state buffer))
                 (setf (completion-state buffer)
                       (copy-completion-state state :anchor newnode))
                 (when (plusp (length s)) ; now length 2, completion for if
                   (setf (completion-state buffer)
                         (make-completion-state
                          :anchor newnode
                          :candidates (completion-candidates buffer))))))
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (let ((newnode (make-instance 'parse:symbol-ref
                                                 :name (string-drop s 1)
                                                 :home-package (parse:home-package node))))
                     (swap-node location newnode buffer)
                     (when (completion-state buffer)
                       (setf (completion-state buffer)
                             (make-completion-state
                              :anchor newnode
                              :candidates (completion-candidates buffer)))))
                   (swap-node location (hole) buffer)))
             t)))))

(defparameter *symbol-mappings* '((<= . #\≤) (>= . #\≥) (/= . #\≠) (lambda . "λ")))

(defparameter *reader-punctuation* '((parse:read-quote . "'")
                                     (parse:read-function . "#'")
                                     (parse:read-quasiquote . "`")
                                     (unquote . ",") (unquote-splicing . ",@")
                                     (parse:read-eval . "#.")))

(defun to-read-syntax (name)
  (case name
    (cl:quote 'parse:read-quote)
    (cl:function 'parse:read-function)
    (eclector.reader:quasiquote 'parse:read-quasiquote)
    (eclector.reader:unquote 'unquote)
    (eclector.reader:unquote-splicing 'unquote-splicing)
    (t name)))

(defun string-and-package= (node sym)
  "Suitable as a comparison predicate between symbol-ref, symbol"
  (and (string= (parse:name node) sym)
       (eq (parse:home-package node) (symbol-package sym))))

(defun symbol-ref-text (node package &optional use-mappings)
  (let ((name (parse:name node))
        (home (parse:home-package node)))
    (cond
      ((typep node 'parse:reader-marker)
       (or (assoc-value *reader-punctuation* (to-read-syntax (parse:resolve node)))
           (string name)))
      ((eq home (find-package :keyword)) (format nil ":~(~a~)" name))
      ((and use-mappings
            (assoc-value *symbol-mappings* node :test #'string-and-package=))
       (string (assoc-value *symbol-mappings* node :test #'string-and-package=)))
      ((null home) (format nil "#:~(~a~)" name))
      ((or (eq home package)
           (multiple-value-bind (symbol found) (find-symbol name package)
             (and found
                  (multiple-value-bind (original present) (find-symbol name home)
                    (and present (eq symbol original))))))
       (string-downcase name))
      (t
       (let ((prefix (or (car (rassoc home (slynk-backend:package-local-nicknames package)))
                         (first (sort (cons (package-name home)
                                            (copy-list (package-nicknames home)))
                                      #'< :key #'length)))))
         (format nil "~(~a~a~a~)" prefix
                 (if (external-symbol-p name home) ":" "::") name))))))

(defmethod render-node ((node parse:symbol-ref) stack context rect &key)
  (let* ((location (car stack))
         (str (symbol-ref-text node (buffer-package context) t))
         (focused (location= location (focus context))))
    (tui:puts* str 1 1 rect)
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

;;; binders
(defmethod handle-key ((node parse:binder) view location buffer event)
  (when (is-regular-char-event event)
    (let ((s (parse:name node))
          (c (tui:event-kind event)))
      (cond ((and (symbol-char-p c) (not (char= c #\:)))
             (swap-node location
                        (make-instance 'parse:binder
                                       :name (format nil "~a~a" s (string-upcase c))
                                       :home-package (parse:home-package node))
                        buffer)
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (swap-node location
                              (make-instance 'parse:binder
                                             :name (string-drop s 1)
                                             :home-package (parse:home-package node))
                              buffer)
                   (swap-node location (hole) buffer)))
             t)))))

(defmethod render-node ((node parse:binder) stack context rect &key)
  (let* ((location (car stack))
         (str (symbol-ref-text node (buffer-package context)))
         (focused (location= location (focus context))))
    (tui:puts* str 1 1 rect (tui:make-style :italicp t))
    (make-instance 'ast-view
                   :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

(defun render-symbol-space (op loc stack context rect)
  "Renders `loc' as a bare focusable token for `op', a string designator,
with a space after it unless specified."
  (let* ((text (if-let (exp (assoc-value *reader-punctuation* op))
                 (string exp)
                 (if-let (exp (assoc-value *symbol-mappings* op))
                   (format nil "~(~a~) " exp)
                   (format nil "~(~a~) " op))))
         (focused (location= loc (focus context)))
         (view
           (make-instance 'ast-view
                          :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                          :location loc
                          :hoverable t
                          :key-handler (when focused (global-key-handler op loc))
                          :focused focused)))
    (tui:puts* text 1 1 rect)
    (setf (view-stack view) (cons loc stack))
    (when focused (setf (gethash :focus-view (redisplay-cache context)) view))
    view))

;;; syntactic list
(defparameter *list-delimiters* '("(" . ")"))

(defmethod parse:get-location ((node parse:ref-list) (id (eql :open)))
  node)

(defmethod parse:location-sort ((node parse:ref-list) (id (eql :open)))
  'parse:unevaluated)

(defun handle-focus-p (buffer list-loc)
  "Is the focus the opening parenthesis handle of the list at `list-loc'?"
  (let ((stack (stack buffer)))
    (and (eq (location-id (first stack)) :open)
         (location= (second stack) list-loc))))

(defun render-delimiter (text loc stack rect &key hoverable)
  (let ((view
          (make-instance 'ast-view
                         :rect (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                         :location loc
                         :hoverable hoverable)))
    (tui:puts* text 1 1 rect)
    (setf (view-stack view) stack)
    view))

(defun list-locator (location)
  "Locates the elements of the list at `location' by index."
  (lambda (index)
    (make-location :node (location-node location)
                   :id (append-id (location-id location) index))))

(defun render-elements (elements spec locate stack context rect keys
                        &optional (render #'render-node))
  "Splits `elements' into rows of (indent . n) following `spec', a list of
(indent . count) pairs where count is a number of elements or nil for all remaining
elements on a single line. A zero count produces a blank row. The last pair repeats,
e.g. setq is (2 . 2), and t is a synonym for 1. `spec' may instead be a function from
`elements' to specs. `render' renders each element with the render-node calling
convention."
  (let ((pairs (if (listp spec) spec (funcall spec elements)))
        (remaining elements)
        (transform (or (getf keys :transform) #'identity))
        (index 0))
    (tui:with-vertical (rect)
      (loop while (and remaining (not (tui:full)))
            do (destructuring-bind (indent . n) (car pairs)
                 (setf pairs (or (cdr pairs) pairs))
                 (if (eql n 0)
                     (tui:pad 1)
                     (tui:place (rect)
                       (tui:with-horizontal (rect)
                         (when (plusp indent)
                           (tui:pad indent))
                         (loop
                           with count = (case n
                                          ((t) 1)
                                          ((nil) (length remaining))
                                          (otherwise n))
                           for i from 0 below count
                           with last-elt = nil
                           while remaining
                           do (let ((element (funcall transform (pop remaining)))
                                    (loc (funcall locate index)))
                                (incf index)
                                (unless (or (zerop i) (typep last-elt 'parse:reader-marker))
                                  (tui:pad 1))
                                (tui:place (rect)
                                  (apply render element (cons loc stack) context rect keys))
                                (setf last-elt element)))))))))))

(defun view-end (view)
  "For placing parens."
  (if-let (children (tui:children view))
    (view-end (lastcar children))
    (let ((r (tui:rect view)))
      (values (tui:rect-x2 r) (tui:rect-y r)))))

(defmethod render-node ((l parse:ref-list) stack context rect
                        &key (indent *horizontal*) delimited (transform #'identity))
  "A `delimited' list is drawn with `*list-delimiters*' around it, recursively unless
`delimited' is :shallow. It's opening parens is selectable."
  (let* ((location (car stack))
         (focused (location= location (focus context)))
         (head (first (elements l))))
    (flet ((contents (rect)
             (render-elements
              (elements l) indent (list-locator location)
              (cdr stack) context rect
              `(,@(when (eq delimited t) '(:delimited t))
                ,@(when transform `(:transform ,transform))
                ;; raw specs are not meant to be recursively applied
                ;; to nested syntax. note: propagation should stop at any
                ;; evaluated form, which supplies its own drawing rules
                ,@(when (functionp indent) `(:indent ,indent))))))
      (if (and delimited (not (typep head 'parse:reader-marker)))
          (let* ((handle (make-location :node l :id :open))
                 (open (render-delimiter (car *list-delimiters*) handle
                                         (cons handle stack) rect
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
              (setf (tui:key-handler open) (global-key-handler l handle)
                    (tui:focused open) t
                    (gethash :focus-view (redisplay-cache context)) open))
            (make-instance 'ast-view
                           :location (car stack)
                           :key-handler (global-key-handler l location)
                           :focused focused
                           :rect (tui:copy-rect
                                  rect :rows (max 1 (tui:rect-rows (tui:rect inner)))
                                  :cols (- (max (tui:rect-x2 (tui:rect inner))
                                                (tui:rect-x2 (tui:rect close)))
                                           (tui:rect-x rect)))
                           :children (list open inner close)))
          (let* ((content (contents rect))
                 (view (if (elements l)
                           (make-instance
                            'ast-view
                            :location (car stack)
                            :key-handler (global-key-handler l location)
                            :focused focused
                            :rect (tui:copy-rect
                                   rect :rows (max 1 (tui:rect-rows (tui:rect content)))
                                        :cols (- (tui:rect-x2 (tui:rect content))
                                                 (tui:rect-x rect)))
                            :children (list content))
                           (render-delimiter (format nil "~a~a" (car *list-delimiters*)
                                                     (cdr *list-delimiters*))
                                             location stack rect :hoverable t))))
            (setf (tui:key-handler view) (global-key-handler l location)
                  (tui:focused view) focused)
            view)))))

;; for body lists
(defmethod render-node ((l list) stack context rect
                        &key (indent *horizontal*) delimited)
  (let* ((location (car stack))
         (view (render-elements l indent (list-locator location)
                                (cdr stack) context rect
                                (when delimited (list :delimited delimited)))))
    (setf (tui:key-handler view) (global-key-handler l location)
          (tui:focused view) (location= location (focus context)))
    view))

;;; literals - no cursor state needed
(defmethod handle-key ((node parse:literal) view location buffer event)
  (when (is-regular-char-event event)
    (let ((s (parse:str node))
          (c (tui:event-kind event)))
      (cond ((digit-char-p c)
             (swap-node location
                        (make-instance 'parse:literal :str (format nil "~a~a" s c))
                        buffer)
             t)
            ((char= c #\Rubout)
             (let ((s (string s)))
               (if (< 1 (length s))
                   (swap-node location
                              (make-instance 'parse:literal
                                             :str (string-drop s 1))
                              buffer)
                   (swap-node location (hole) buffer)))
             t)))))

(defun render-literal-line (line stack context rect &key style)
  (declare (ignore stack context))
  (tui:puts* line 1 1 rect style)
  (make-instance 'tui:view :rect (tui:copy-rect rect :rows 1
                                                     :cols (tui:display-width line))))

(defparameter *horizontal* '((0 . nil)))
(defparameter *vertical* '((0 . t)))

(defun render-literal-text (node text stack context rect)
  (let* ((location (car stack))
         (focused (location= location (focus context)))
         (style (if (and focused (buffer-focused-p context))
                    (tui:make-style :fg #x0)
                    (tui:make-style :fg #x2aa198)))
         (contents (render-elements (split-string text #\newline) *vertical*
                                    (constantly location) (cdr stack) context rect
                                    `(:style ,style) #'render-literal-line)))
    (make-instance 'ast-view
                   :rect (tui:rect contents)
                   :location location
                   :children (list contents)
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

(defmethod render-node ((node parse:literal) stack context rect &key)
  (render-literal-text node (format nil "~a" (parse:str node)) stack context rect))

(defmethod render-node ((node string) stack context rect &key)
  (render-literal-text node (format nil "~s" node) stack context rect))

;;; function call
(defparameter *fun-key-handlers* (make-hash-table :test #'equal))

(defmethod handle-key ((node parse:function-call) view location buffer event)
  ;; I don't expect function calls to have any special binds percolated up the tree
  (when-let ((handler (gethash event *fun-key-handlers*)))
    (when (and (< 1 (length (stack buffer)))
               (location= location (second (stack buffer)))) ; we need to be a direct child
      (funcall handler buffer))))

(setf (gethash (tui-sys:make-event :kind #\space) *fun-key-handlers*)
      (lambda (buffer)
        (let* ((fun-loc (car (stack buffer)))
               (fun-node (location-node fun-loc))
               (id (location-id fun-loc))
               (i (if (bodylike-id id) (1+ (id-index id)) 0)))
          (insert-or-jump (make-location :node fun-node :id 'parse:body) i
                          buffer (stack buffer)))))

(defun rubout-eval-form (buffer)
  "Replaces a focused evaluated form with a hole, or deletes a focused body hole."
  (let* ((location (focus buffer))
         (focused (node-at location)))
    (cond ((or (typep focused 'parse:eval-form)
               (and (typep focused 'parse:ref-list)
                    (null (elements focused))
                    (eq 'parse:eval-form (locsort location))))
           (swap-node location (hole) buffer))
          ((and (eq 'parse:eval-form (locsort location))
                (typep focused 'hole))
           (let ((id (location-id location))
                 (parent (location-node location)))
             ;; By default only delete children in a body
             (when (bodylike-id id)
               (let ((body-loc (make-location :node parent :id (parent-id id))))
                 (save-history buffer)
                 (if (< 1 (length (elements (node-at body-loc))))
                     (ast-delete body-loc (id-index id) buffer)
                     (edit-list body-loc
                                (lambda (body)
                                  (list-remove (elements body) (id-index id)))
                                (move-back parent id) buffer (stack buffer)))
                 t)))))))

(setf (gethash (tui-sys:make-event :kind #\rubout) *fun-key-handlers*)
      #'rubout-eval-form)

;; (define-constant +top-left+ (name-char "U1CE16") :test #'equal)
;; (define-constant +bot-left+ (name-char "U1CE17") :test #'equal)
;; (define-constant +top-right+ (name-char "U1CE18") :test #'equal)
;; (define-constant +bot-right+ (name-char "U1CE19") :test #'equal)
;; - unary operators have space removed
;; - for 2 arguments, draw *arb-arity-binops* infix
;;   - higher arity duplicated op views break invariant: always a single focused view
;; - for 2 arg division, draw horizontally. maybe future: (f)floor/ceiling/truncate
(defmethod render-node ((node parse:function-call) stack context rect &key)
  (trivia:match (and (= 2 (length (parse:body node))) (is-binop-call node))
    ;; note this case must go first since division is a binop
    ('cl:/
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
                                                              div-loc)
                             :focused (location= div-loc (focus context)))))
       (setf (gethash (parse:name node) (node-views context)) line-view)
       (setf (view-stack line-view) (cons div-loc stack))
       (when (location= div-loc (focus context))
         (setf (gethash :focus-view (redisplay-cache context)) line-view))
       (tui:puts* (make-string width :initial-element #\─)
                  (1+ (tui:rect-rows arg1-rect)) 1 rect)
       ;;
       (make-instance 'ast-view
                      :location location
                      :rect (tui:copy-rect rect :cols width
                                                :rows (+ (tui:rect-rows arg1-rect)
                                                         1 (tui:rect-rows arg2-rect)))
                      :children (list arg1-view line-view arg2-view)
                      :key-handler (global-key-handler node location)
                      :focused (location= location (focus context)))))
    ;; need wrapper for args
    ((and op (not nil))
     (flet ((is-bracketed (arg)
              (when-let (arg-op (is-binop-call arg))
                (or (> (binop-precedence op) (binop-precedence arg-op))
                    (and (eq op 'cl:-) (member arg-op '(cl:+ cl:-)))))))
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
                   (tui:put* #\( 1 1 rect)
                   (tui:put* #\) 1 rcol rect))
                 (progn
                   (tui:put* #\⎛ 1 1 rect)
                   (tui:put* #\⎝ (tui:rect-rows arg1-rect) 1 rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put* #\⎜ y 1 rect))
                   (tui:put* #\⎞ 1 rcol rect)
                   (tui:put* #\⎠ (tui:rect-rows arg1-rect) rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put* #\⎟ y rcol rect))))))
         (when arg2-bracketed
           (let ((lcol (- (tui:rect-x arg2-rect) (tui:rect-x rect)))
                 (rcol (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                          (1+ (tui:rect-cols arg2-rect)))))
             (if (= 1 (tui:rect-rows arg2-rect))
                 (progn
                   (tui:put* #\( 1 lcol rect)
                   (tui:put* #\) 1 (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                      (1+ (tui:rect-cols arg2-rect)))
                             rect))
                 (progn
                   (tui:put* #\⎛ 1 lcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put* #\⎜ y (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                      rect))
                   (tui:put* #\⎝ (tui:rect-rows arg2-rect) lcol rect)
                   (tui:put* #\⎞ 1 rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put* #\⎟ y rcol rect))
                   (tui:put* #\⎠ (tui:rect-rows arg2-rect) rcol rect)))))
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
                        :key-handler (global-key-handler node location)
                        :focused (location= location (focus context))))))
    (_
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
                      :key-handler (global-key-handler node location)
                      :focused (location= location (focus context)))))))

;;; macro calls
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

(defun loop-indentation (elements &optional (package *package*))
  "Indent spec for a loop form. The operator has its own line, then a line indented by one
for every clause keyword, except those following and/else. Forms of a do-like clause after
its first are aligned with the first, and a loop without keywords has a line per form."
  (let* ((indent 1)
         (rows (list '(0 . 1)))
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
                               (incf column
                                     (1+ (length (symbol-ref-text previous package)))))
                        (progn (push (cons indent 1) rows)
                               (setf column indent)))
                    (setf body-column (when (member keyword *loop-body-keywords*
                                                    :test #'string=)
                                        (+ column
                                           (length (symbol-ref-text element package)) 1))
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

(defun macro-call-indentation (op &optional (package *package*))
  "`loop-indentation' for loop. Note `op' can be a hole"
  (if-let (s (and (typep op 'parse:symbol-ref) (parse:resolve op)))
    (cond ((eq s 'loop) (lambda (elements) (loop-indentation elements package)))
          ((member s '(setf psetf psetq)) `((0 . 3) (,(1+ (length (string s))) . 2)))
          ((fboundp s)
           (let* ((arglist (slynk-backend:arglist s)))
             (when (proper-list-p arglist)
               (let ((arglist (remove '&optional arglist)))
                 (if-let (index (position-if (lambda (s) (member s '(&rest &body)))
                                             arglist))
                   (progn
                     (when-let (env-pos (position '&environment arglist))
                       (when (< env-pos index)
                         (decf index 2)))
                     (when (member '&whole arglist)
                       (decf index 2))
                     (case index
                       (0 `((0 . 2) (,(1+ (length (string s))) . t)))
                       (1 `((0 . ,(1+ index)) (1 . t)))
                       (2 (if (uiop:string-prefix-p "DEF" (string s))
                              `((0 . 3) (1 . t)) ; e.g. defgeneric, deftype
                              `((0 . 2) (3 . 1) (1 . t))))
                       (3 `((0 . 3) (1 . t))) ; e.g. defclass define-condition
                       (t `((0 . ,(1+ index)) (1 . t)))))
                   (progn
                     *horizontal*))))))
          (t *horizontal*))
    *horizontal*))

(defmethod render-node ((node parse:macro-call) stack context rect &key)
  (let* ((location (car stack))
         (view (render-elements
                (cons (parse:op node) (parse:body node))
                (macro-call-indentation (parse:op node) (buffer-package context))
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
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

(defparameter *macro-call-key-handlers* (make-hash-table :test #'equal))

(defmethod handle-key ((node parse:macro-call) view location buffer event)
  (declare (ignore view))
  (when-let ((handler (gethash event *macro-call-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack buffer))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (stack buffer)) buffer)))))

(defun wrap-in-list (loc buffer stack)
  "Wraps the value at `loc' in a syntax list, adding a hole afterwards unless it is
already and focuses the hole."
  (let ((holep (typep (node-at loc) 'parse:hole)))
    (edit-list loc (lambda (thing) (if holep (ref-list thing) (ref-list thing (hole))))
               (append-id (location-id loc) (if holep 0 1)) buffer stack)))

(defun macro-body-id-p (id)
  (and (consp id) (eq (car id) 'parse:body)))

(setf (gethash (tui-sys:make-event :kind #\() *macro-call-key-handlers*)
      (lambda (stack buffer)
        (let ((loc (car stack)))
          (when (macro-body-id-p (location-id loc))
            (save-history buffer)
            (wrap-in-list loc buffer stack)
            t))))

(setf (gethash (tui-sys:make-event :kind #\space) *macro-call-key-handlers*)
      (lambda (stack buffer)
        (let* ((loc (car stack))
               (node (location-node loc))
               (id (location-id loc)))
          (multiple-value-bind (list-id index)
              (cond ((eq id 'parse:op) (values 'parse:body 0))
                    ((handle-focus-p buffer loc) (values id 0))
                    (t (values (parent-id id) (1+ (lastcar id)))))
            (insert-or-jump (make-location :node node :id list-id) index buffer stack))
          t)))

(setf (gethash (tui-sys:make-event :kind #\rubout) *macro-call-key-handlers*)
      (lambda (stack buffer)
        (let* ((loc (car stack))
               (id (location-id loc)))
          (when (macro-body-id-p id)
            (let ((parent (make-location :node (location-node loc) :id (parent-id id))))
              (save-history buffer)
              (if (= 1 (length (elements (node-at parent))))
                  (edit-list parent (constantly nil)
                             (if (eq (location-id parent) 'parse:body)
                                 'parse:op
                                 ;; focus a nested list
                                 (location-id parent))
                             buffer stack)
                  (ast-delete parent (lastcar id) buffer stack))
              t)))))

;;; quasiquote
(defmethod render-node ((node parse:quasiquote-form) stack context rect &key)
  (let* ((location (car stack))
         (op-view (render-symbol-space 'parse:read-quasiquote
                                       (make-location :node node :id 'parse:op)
                                       stack context rect))
         (op-rect (tui:rect op-view))
         (view (render-node (parse:thing node)
                            (cons (make-location :node node :id 'parse:thing) stack)
                            context
                            (tui:clamp-rect (tui:copy-rect rect :x (tui:rect-x2 op-rect))
                                            rect)
                            :delimited t
                            :indent (lambda (elements)
                                      (let ((spec (macro-call-indentation
                                                   (first elements)
                                                   (buffer-package context))))
                                        (if (functionp spec)
                                            (funcall spec elements)
                                            spec)))))
         (view-rect (tui:rect view)))
    (make-instance 'ast-view
                   :location location
                   :children (list op-view view)
                   :rect (tui:clamp-rect
                          (tui:copy-rect rect :rows (tui:rect-rows view-rect)
                                              :cols (+ (tui:rect-cols op-rect)
                                                       (tui:rect-cols view-rect)))
                          rect)
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

;;
;;; layout generation
;;
(defvar *default-expansions* (make-hash-table :test #'eq)
  "Operator symbol -> closure constructing a default node for that form.")

(defrecord layout ()
  ((op :type symbol)
   (rows :type list)
   (templates :type list)) ;alist
  (:representation :struct)
  (:documentation
   "Describes the rendered shape of a call to `op'.
`rows' are laid out vertically and `templates' contains (slot . constructor)
pairs building new elements of &rest and &body slots."))

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
  (cdr (assoc slot (parse:form-slot-kinds (layout-op layout)))))

(defgeneric focus-order (node)
  (:method (node)
    (when-let (layout (gethash (type-of node) *layouts*))
      (layout-slots layout)))
  (:method ((node parse:function-call)) '(parse:name parse:body))
  (:method ((node parse:macro-call)) '(parse:op parse:body))
  (:method ((node parse:function-code))
    '(parse:lambda-list parse:docstring parse:declarations parse:body))
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
  "Initializes the slots of `layout', leaving unused alternative slots unbound."
  (apply #'make-instance (layout-op layout)
         (loop with visible = (layout-slots layout)
               with optional = (parse:form-optional-slots (layout-op layout))
               for (slot . kind) in (remove-duplicates
                                     (parse:form-slot-kinds (layout-op layout))
                                     :key #'car :from-end t)
               unless (and (member slot optional) (not (member slot visible)))
                 append (list (make-keyword slot)
                              (default-slot-value kind (layout-template layout slot))))))

(defun slot-render-keys (node slot)
  (case (cdr (assoc slot (parse:form-slot-kinds (type-of node))))
    (parse::&tree '(:delimited t))
    (parse::&declarations (list :indent *vertical* :delimited t))
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
     (render-symbol-space (parse:op node)
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
          (tui:place (rect) (render-layout-slot node item stack context rect))))))

(defun render-vertical-layout (node down-layout stack context rect)
  (tui:with-vertical (rect)
    (dolist (item down-layout)
      (cond ((integerp item)
             (tui:pad item))
            ((consp item)
             (tui:place (rect) (render-horizontal-layout node item stack context rect)))
            (t
             (tui:place (rect) (render-layout-slot node item stack context rect)))))))

(defmethod render-node ((node parse:irregular-form) stack context rect &key)
  (let* ((layout (gethash (type-of node) *layouts*))
         (vertical (render-vertical-layout node (layout-rows layout) stack context rect))
         (location (car stack)))
    (make-instance 'ast-view
                   :location location
                   :children (list vertical)
                   :rect (tui:rect vertical)
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

(defparameter *layout-key-handlers* (make-hash-table :test #'equal)
  "Map from event to callbacks (layout, child-stack, buffer) -> bool whether to propagate.")

(defmethod handle-key ((node parse:irregular-form) view location buffer event)
  (declare (ignore view))
  (when-let ((layout (gethash (type-of node) *layouts*))
             (handler (gethash event *layout-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack buffer))))
      (unless (zerop i)
        (funcall handler layout (nthcdr (1- i) (stack buffer)) buffer)))))

(defun rubout-tree-slot (loc stack buffer)
  "Deletes at `loc', keeping focus valid when a declaration list becomes empty."
  (save-history buffer)
  (let* ((id (location-id loc))
         (node (location-node loc)))
    (if (consp id)
        (let* ((parent-id (parent-id id))
               (parent (make-location :node node :id parent-id)))
          (if (= 1 (length (elements (node-at parent))))
              (edit-list parent (constantly nil)
                         (if (member parent-id '(parse:decls parse:declarations))
                             (move-back node parent-id)
                             parent-id)
                         buffer stack)
              (ast-delete parent (lastcar id) buffer stack)))
        (swap-node loc (hole) buffer))))

(defun layout-rubout (layout stack buffer)
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
                  (save-history buffer)
                  (edit-list rest-loc (constantly (list (funcall template)))
                             (append-id slot 0) buffer stack)
                  (focus-hole-in-slot buffer)
                  t))
           (cond ((symbolp id)
                  (reset))
                 ((or (= 2 (length id)) (= 3 (length id)))
                  (if (= 1 (length (elements (node-at rest-loc))))
                      (reset)
                      (progn (save-history buffer)
                             (ast-delete rest-loc (second id) buffer stack))))))))
      ((member (slot-kind layout slot) '(parse::&tree parse::&declarations))
       (rubout-tree-slot loc stack buffer)))))

(defun layout-space (layout stack buffer)
  "Graphically inserts after, or jumps to the next adjacent hole."
  (let* ((loc (car stack))
         (node (location-node loc))
         (id (location-id loc))
         (slot (ensure-car id))
         (next (when (symbolp id)
                 (find-if (lambda (next)
                            (or (not (eq (slot-kind layout next) 'parse::&declarations))
                                (funcall next node)))
                          (cdr (member id (layout-slots layout))))))
         (template (layout-template layout slot)))
    (cond
      ((handle-focus-p buffer loc)
       (let ((item (if template (funcall template) (hole))))
         (insert-or-jump loc 0 buffer stack
                         :item item :tail (when (typep item 'ref-list) '(0)))))
      (next
       (let ((next-loc (make-location :node node :id next))
             (next-template (layout-template layout next)))
         (if (and (member (cdr (assoc next (parse:form-slot-kinds (type-of node))))
                          '(&rest &body parse::&tree parse::&declarations))
                  (gen-list-p (node-at next-loc)))
             (insert-or-jump next-loc 0 buffer stack
                             :item (if next-template (funcall next-template) (hole)))
             (refocus buffer next))))
      ((and template (consp id))
       (let ((rest-loc (make-location :node node :id slot))
             (nested (typep (funcall template) 'ref-list)))
         (case (length id)
           (2 (if nested ; e.g. (body i), or (rest i) which needs traverse
                  (insert-or-jump rest-loc (1+ (second id)) buffer stack
                                  :item (funcall template) :tail '(0))
                  (insert-or-jump rest-loc (1+ (second id)) buffer stack)))
           (3 (when nested
                (insert-or-jump (make-location :node node :id (list slot (second id)))
                                (1+ (third id)) buffer stack)))
           (t (slog* (format nil "no space action for ~a" (length id)))))))
      ((and (member (slot-kind layout slot) '(parse::&tree parse::&declarations))
            (consp id))
       (insert-or-jump (make-location :node node :id (parent-id id))
                       (1+ (lastcar id)) buffer stack)))))

(defun layout-wrap (layout stack buffer)
  "Wraps the focused part of an unevaluated slot in a syntax list."
  (let ((loc (car stack)))
    (when (member (slot-kind layout (ensure-car (location-id loc)))
                  '(parse::&tree parse::&declarations))
      (save-history buffer)
      (wrap-in-list loc buffer stack)
      t)))

(setf (gethash (tui-sys:make-event :kind #\rubout) *layout-key-handlers*) #'layout-rubout
      (gethash (tui-sys:make-event :kind #\space) *layout-key-handlers*) #'layout-space
      (gethash (tui-sys:make-event :kind #\() *layout-key-handlers*) #'layout-wrap)

(defun register-layout (layout)
  (assert (ends-with-subseq "-FORM" (string (layout-op layout))))
  (let ((len (length "-FORM")))
    (setf (gethash (layout-op layout) *layouts*) layout
          (gethash (find-symbol (string-drop (string (layout-op layout)) len)
                                "WEAVE-PARSER")
                   *default-expansions*)
          (lambda () (default-node layout)))))

(defmacro deflayout (type templates rows)
  "Lays out forms of class `type' in `rows' of slots, where `templates' are forms building
new elements for its &rest and &body slots.
ASSUMES that &rest slots accept (slot i j) even if the list is optional like let.
ASSUMES we never focus a plain list body."
  `(register-layout
    (make-layout :op ',type
                 :rows ',rows
                 :templates (list ,@(loop for (slot . template) in templates
                                          collect `(cons ',slot (lambda () ,template)))))))

(deflayout parse:let*-form ((parse:vars . (ref-list (hole) (hole)))
                            (parse:body . (hole)))
  ((parse:op parse:vars)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:let-form ((parse:vars . (ref-list (hole) (hole)))
                           (parse:body . (hole)))
  ((parse:op parse:vars)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:flet-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:funs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:labels-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:funs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:macrolet-form
    ((parse:macro-defs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :macro-lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:macro-defs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:symbol-macrolet-form ((parse:macro-code . (ref-list (hole) (hole)))
                                       (parse:body . (hole)))
  ((parse:op parse:macro-code)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:block-form ((parse:body . (hole)))
  ((parse:op parse:name)
   (1 parse:body)))

(deflayout parse:catch-form ((parse:body . (hole)))
  ((parse:op parse:tag)
   (1 parse:body)))

(deflayout parse:throw-form ()
  ((parse:op parse:tag 1 parse:result)))

(deflayout parse:return-from-form ((parse:value . (hole)))
  ((parse:op parse:name 1 parse:value)))

(deflayout parse:if-form ((parse:then-else . (hole)))
  ((parse:op parse:test)
   (3 parse:then-else)))

(deflayout parse:setq-form ((parse:forms . (hole)))
  ((parse:op (parse:forms ((0 . 2))))))

(deflayout parse:progn-form ((parse:forms . (hole)))
  (parse:op
   (1 parse:forms)))

(deflayout parse:tagbody-form ((parse:body . (hole)))
  (parse:op
   (1 parse:body)))

(deflayout parse:go-form ()
  ((parse:op parse:tag)))

(deflayout parse:the-form ()
  ((parse:op parse:type-specifier 1 parse:form)))

(deflayout parse:unwind-protect-form ((parse:cleanup . (hole)))
  ((parse:op)
   (3 parse:protected)
   (1 parse:cleanup)))

(deflayout parse:multiple-value-prog1-form ((parse:body . (hole)))
  ((parse:op parse:values-form)
   (1 parse:body)))

(deflayout parse:multiple-value-call-form ((parse:body . (hole)))
  ((parse:op parse:fun 1 parse:arg 1 parse:body)))

(deflayout parse:progv-form ((parse:body . (hole)))
  ((parse:op parse:var-list 1 parse:val-list)
   (1 parse:body)))

(deflayout parse:locally-form ((parse:body . (hole)))
  (parse:op
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:eval-when-form ((parse:body . (hole)))
  ((parse:op parse:situations)
   (1 parse:body)))

(deflayout parse:load-time-value-form ()
  ((parse:op parse:form 1 parse:read-only-p)))

(deflayout parse:function-form ()
  ((parse:op parse:fun-designator)))

(deflayout parse:quote-form ()
  ((parse:op parse:thing)))

(deflayout parse:defmacro-form ()
  ((parse:op parse:name)
   (1 parse:macro-code)))

;; TODO (setf f) printing
(deflayout parse:defun-form ()
  ((parse:op parse:name)
   (1 parse:fun-code)))

(deflayout parse:defmethod-form ()
  ((parse:op parse:name 1 parse:qualifiers)
   (1 parse:fun-code)))

(deflayout parse:lambda-form ()
  ((parse:op parse:fun-code)))
;; override for nonempty lambda list
(setf (gethash 'lambda *default-expansions*)
      (lambda ()
        (make-instance 'parse:lambda-form
                       :fun-code (make-instance 'parse:function-code
                                                :lambda-list-kind :lambda
                                                :lambda-list (ref-list (hole))
                                                :body (list (hole))))))

;;; function-code
(defparameter *fun-code-key-handlers* (make-hash-table :test #'equal))

(defmethod handle-key ((node parse:function-code) view location buffer event)
  (declare (ignore view))
  (when-let ((handler (gethash event *fun-code-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack buffer))))
      (unless (zerop i)
        (funcall handler (nthcdr (1- i) (stack buffer)) buffer)))))

(setf
 (gethash (tui-sys:make-event :kind #\rubout) *fun-code-key-handlers*)
 (lambda (stack buffer)
   (declare (ignore stack buffer))
   t))

(setf
 (gethash (tui-sys:make-event :kind #\space) *fun-code-key-handlers*)
 (lambda (stack buffer)
   (let* ((loc (car stack))
          (node (location-node loc)))
     (flet ((insert-into (id index)
              (insert-or-jump (make-location :node node :id id) index buffer stack))
            (shorter-than (id limit)
              (< (length (elements (node-at (make-location :node node :id id)))) limit)))
       (trivia:cmatch (location-id loc)
         ((list* (eql 'parse:declarations) path)
          (insert-into (parent-id (location-id loc)) (1+ (lastcar path))))
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
         ((eql 'parse:docstring)
          (insert-into 'parse:body 0))
         ((list (eql 'parse:body) (and (type integer) i))
          (insert-into 'parse:body (1+ i))))))))

(setf
 (gethash (tui-sys:make-event :kind #\() *fun-code-key-handlers*)
 (lambda (stack buffer)
   (let ((loc (car stack)))
     (trivia:match (location-id loc)
       ((list* (eql 'parse:declarations) _)
        (save-history buffer)
        (wrap-in-list loc buffer stack)
        t)
       ((or (list (eql 'parse:lambda-list) (type integer))
            (list (eql 'parse:lambda-list) (type integer) (type integer)))
        (save-history buffer)
        (edit-list loc (lambda (param) (ref-list param (hole)))
                   (append-id (location-id loc) 1) buffer stack)
        t)))))

(setf
 (gethash (tui-sys:make-event :kind #\rubout) *fun-code-key-handlers*)
 (lambda (stack buffer)
   (let* ((loc (car stack))
          (node (location-node loc))
          (ll-loc (make-location :node node :id 'parse:lambda-list)))
     (trivia:match (location-id loc)
       ((list* (eql 'parse:declarations) _)
        (rubout-tree-slot loc stack buffer))
       ((list (eql 'parse:lambda-list) (and (type integer) i))
        (save-history buffer)
        (if (= 1 (length (parse:elements (node-at ll-loc))))
            (swap-node ll-loc (parse:ref-list) buffer)
            (ast-delete ll-loc i buffer stack)))
       ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j))
        (unless (zerop j)
          (save-history buffer)
          (ast-delete (make-location :node node :id `(parse:lambda-list ,i)) j
                      buffer stack)))
       ((list (eql 'parse:lambda-list) (and (type integer) i) (and (type integer) j)
              (and (type integer) k))
        (unless (zerop k)
          (save-history buffer)
          (ast-delete (make-location :node node :id `(parse:lambda-list ,i ,j))
                      k buffer stack)))
       ((eql 'parse:lambda-list)
        (save-history buffer)
        (swap-node ll-loc (parse:ref-list) buffer))))))

;;; function-code (lambda-list, documentation and body, shared by defun/defmacro/
;;; defmethod/lambda/flet/labels), including declarations.
(defmethod render-node ((node parse:function-code) stack context rect &key)
  (let* ((location (car stack))
         (lambda-list-view
           (render-node (parse:lambda-list node)
                        (cons (make-location :node node :id 'parse:lambda-list) stack)
                        context rect :delimited t))
         (ll-rect (tui:rect lambda-list-view))
         (docstring (parse:docstring node))
         (docstring-view
           (when docstring
             (render-node docstring
                          (cons (make-location :node node :id 'parse:docstring) stack)
                          context
                          (tui:clamp-rect
                           (tui:copy-rect rect :x (tui:rect-x rect)
                                               :y (tui:rect-y2 ll-rect))
                           rect))))
         (declarations-y (if docstring-view
                             (tui:rect-y2 (tui:rect docstring-view))
                             (tui:rect-y2 ll-rect)))
         (declarations-view
           (when (parse:declarations node)
             (render-node (parse:declarations node)
                          (cons (make-location :node node :id 'parse:declarations) stack)
                          context
                          (tui:clamp-rect (tui:copy-rect rect :y declarations-y) rect)
                          :indent *vertical* :delimited t)))
         (body-y (if declarations-view
                     (tui:rect-y2 (tui:rect declarations-view))
                     declarations-y))
         (body-view
           (render-elements (parse:body node) *vertical*
                            (list-locator (make-location :node node :id 'parse:body))
                            stack context
                            (tui:clamp-rect
                             (tui:copy-rect rect :x (tui:rect-x rect) :y body-y)
                             rect)
                            nil))
         (children (remove nil (list lambda-list-view docstring-view
                                     declarations-view body-view))))
    (make-instance
     'ast-view
     :location location
     :children children
     :rect (tui:copy-rect
            rect
            :rows (reduce #'+   children :key (lambda (c) (tui:rect-rows (tui:rect c))))
            :cols (reduce #'max children :key (lambda (c) (tui:rect-cols (tui:rect c)))))
     :key-handler (global-key-handler node location)
     :focused (location= location (focus context)))))

;;; global key handlers
(defun build-arglist (fname &optional old-body)
  (let* ((arglist (when (fboundp fname) (slynk-backend:arglist fname)))
         (argcount (if (proper-list-p arglist)
                       (loop for a in arglist
                             until (member a lambda-list-keywords)
                             count t)
                       0)))
    (append old-body
            (loop for i below (max 1 (- argcount (length old-body)))
                  collect (hole)))))

(defun build-call (ref &optional old-body)
  "ref must be a symbol-ref"
  (check-type ref parse:symbol-ref)
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

(defun focus-hole-in-slot (buffer)
  "Focuses the first hole at or below the focused location."
  (let* ((loc (focus buffer))
         (node (location-node loc))
         (id (location-id loc)))
    (when-let (path (if (symbolp id)
                        (list-hole-path node id)
                        (hole-path node id)))
      (refocus buffer (first path))
      (dolist (id (rest path))
        (descend buffer id)))))

(defun focus-first-hole (buffer)
  (let ((node (node-at (focus buffer))))
    (loop for (slot . kind) in (parse:form-slot-kinds (type-of node))
          do (when-let (path
                        (if (and (member kind
                                         '(&rest &body parse::&tree parse::&declarations))
                                 (gen-list-p (node-at (make-location :node node :id slot))))
                            (list-hole-path node slot)
                            (hole-path node slot)))
               (dolist (id path)
                 (descend buffer id))
               (return)))))

(setf (gethash (tui-sys:make-event :kind #\space) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (let ((stack (stack buffer)))
          (when (= (length stack) 1)
            (let* ((toplevel (car stack))
                   (i (location-id toplevel)))
              (assert (integerp i))
              (save-history buffer)
              (let ((newroot (list-insert (location-node toplevel) (hole) (1+ i))))
                (commit-edit buffer
                             (cons (make-location :node newroot :id (1+ i))
                                   (rest stack))
                             newroot)))))))

(setf (gethash (tui-sys:make-event :kind #\newline) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (when-let* ((state (completion-state buffer))
                    (selection (nth (selection state) (candidates state))))
          (let ((s selection))
            (cond
              ((fboundp s)
               (let ((name-node (make-instance 'parse:symbol-ref
                                               :name (symbol-name s)
                                               :home-package (symbol-package s)))
                     (sort (locsort (focus buffer))))
                 (case sort
                   (parse:eval-form
                    (when-let (call (build-call name-node))
                      (swap-node (focus buffer) call buffer)
                      (focus-first-hole buffer)))
                   (parse:unevaluated
                    (when-let (call (build-call name-node))
                      (swap-node (focus buffer) (parse:to-syntax call) buffer)
                      (focus-first-hole buffer)))
                   ;; operator call
                   (parse:fun-designator
                    (when (eq (locsort (second (stack buffer))) 'parse:eval-form)
                      (when-let (call (build-call name-node
                                                  (parse:body
                                                   (location-node (focus buffer)))))
                        (swap-node (second (stack buffer)) call buffer)
                        (focus-first-hole buffer))))
                   (t t))))
              ((gethash s *default-expansions*)
               (let ((focus (focus buffer)))
                 ;; replace the whole parent function node if editing the name
                 (swap-node (if (and (typep (location-node focus) 'parse:function-call)
                                     (eq (location-id focus) 'parse:name))
                                (second (stack buffer))
                                focus)
                            (funcall (gethash s *default-expansions*))
                            buffer))
               (focus-first-hole buffer))
              (t
               (swap-node (focus buffer)
                          (make-instance 'parse:symbol-ref
                                         :name (symbol-name s)
                                         :home-package (symbol-package s))
                          buffer))))
          (setf (completion-state buffer) nil)
          t)))

(setf (gethash (tui-sys:make-event :kind #\newline) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (let ((ref (node-at (focus buffer))))
          (when (and (typep ref 'parse:symbol-ref)
                     (eq 'parse:eval-form (locsort (focus buffer))))
            (when-let (call (build-call ref))
              (swap-node (focus buffer) call buffer)
              (focus-first-hole buffer)
              t)))))

(defun reify (object)
  (typecase object
    (number (make-instance 'parse:literal :str (princ-to-string object)))
    (string (make-instance 'parse:literal :str (princ-to-string object)))
    (character (make-instance 'parse:literal :str (princ-to-string object)))
    (list (make-instance 'parse:quote-form
                         :op 'parse:read-quote
                         :thing (apply #'ref-list (mapcar #'reify object))))
    (t (ref-list))))

;; TODO capture i/o using gray streams
(setf (gethash (tui-sys:make-event :kind #\e :altp t) *global-key-handlers*)
      (lambda (view buffer)
        (let ((stack (stack buffer))
              (rect (transform-rect-to-screen
                     (tui:rect view) (scroll-state-viewport-row (scroll-state buffer))
                     (buffer-rect buffer))))
          (handler-bind ((warning #'muffle-warning))
            (parse::with-temporary-interning (interned)
              (let* ((raw (parse::strip-wrappers
                           (parse:to-syntax (node-at (car stack)))
                           interned))
                     (*standard-output* (make-broadcast-stream))
                     (res (eval raw)))
                (slog* (format nil "eval produced ~a" res))
                (add-buffer
                 (buffer-ui buffer)
                 :ast (list (reify res)) :floating t
                 :package (buffer-package buffer)
                 :x (tui:rect-x2 rect) :y (tui:rect-y rect))))))))

;; convert back to hole
(setf (gethash (tui-sys:make-event :kind #\rubout) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (rubout-eval-form buffer)))

(setf (gethash (tui-sys:make-event :kind :right-arrow :controlp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (let* ((stack (stack buffer))
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
                       buffer)
            (descend buffer '(parse:body 1))
            (descend buffer '(parse:body 0))))))

(setf (gethash (tui-sys:make-event :kind :left-arrow :controlp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (let* ((stack (stack buffer))
               (loc (car stack))
               (up (location-node loc))
               (ploc (cadr (stack buffer)))
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
                       buffer)
            (descend buffer '(parse:body 0))
            (descend buffer '(parse:body 1))))))

(defun take-selection (buffer selection)
  "Reifies selection state into the cutbuffer without editing."
  (if (typep selection 'zipper)
      (setf (zipper buffer) selection
            (cutbuffer buffer) nil)
      (setf (cutbuffer buffer) (copy-locations (selection-locations selection))
            (zipper buffer) nil))
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
          (t (check-type value ref-list)))))

(defun cut-selection (buffer selection)
  "Cuts the current selection or zipper, modifying the ast."
  (let ((forms (selection-forms selection)))
    (take-selection buffer selection)
    (if (typep selection 'zipper)
        (let* ((stack (nthcdr (zipper-depth selection) (stack buffer)))
               (hole (car stack))
               (id (location-id hole)))
          (if (> (length forms) 1)
              ;; a zipper may top out at any eval-form, which is not always a place
              ;; several forms can be left in e.g. dotimes counter
              (when (eval-list-position-p hole)
                (save-history buffer)
                (let ((slot (parent-id id))
                      (index (id-index id)))
                  (edit-list (make-location :node (location-node hole) :id slot)
                             (lambda (body)
                               (let ((elts (elements body)))
                                 `(,@(subseq elts 0 index)
                                   ,@forms
                                   ,@(subseq elts (1+ index)))))
                             (append-id slot index) buffer stack)))
              (progn
                (save-history buffer)
                (ast-replace hole (constantly (first forms)) buffer stack))))
        (multiple-value-bind (low high) (selection-range selection)
          (save-history buffer)
          (let* ((list-loc (selection-list-location selection))
                 (slot (selection-parent-id selection))
                 (elts (elements (node-at list-loc)))
                 (new (or `(,@(subseq elts 0 low) ,@(subseq elts (1+ high)))
                          (empty-list-placeholder list-loc))))
            (edit-list list-loc (constantly new)
                       (if (elements new) (append-id slot (max 0 (1- low))) slot)
                       buffer (stack buffer)))))
    t))

(setf (gethash (tui-sys:make-event :kind #\c :controlp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (if-let (selection (selection-at-focus buffer))
          (take-selection buffer selection)
          (let ((loc (focus buffer)))
            (when (locsort loc)
              (setf (cutbuffer buffer) (copy-locations (list loc))
                    (zipper buffer) nil))))
        t))

(setf (gethash (tui-sys:make-event :kind #\x :controlp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (if-let (selection (selection-at-focus buffer))
          (cut-selection buffer selection)
          (let ((loc (focus buffer)))
            (when (locsort loc)
              (setf (cutbuffer buffer) (copy-locations (list loc))
                    (zipper buffer) nil)
              (swap-node loc (hole) buffer))))
        t))

(defun paste-conversion (from to)
  "Controls copying between location sorts."
  (cond ((and from (equal from to)) #'parse:copy-node)
        ((and (eq from 'parse:eval-form) (eq to 'parse:unevaluated))
         (lambda (form) (parse:to-syntax (parse:copy-node form))))))

(defun paste-forms (buffer clipboard)
  "Pastes the forms of `clipboard' after the focused list element, over the selection at
the focus, or over a focused hole.
Each form is checked for convertibility against the sort of the position it's pasted into,
forms past the end of a selection taking the sort of its last position."
  (let* ((focus (focus buffer))
         (id (location-id focus))
         (selection (when-let (selection (selection-at-focus buffer))
                      (when (= (id-index id) (selection-range selection))
                        selection)))
         (targets (if selection
                      (mapcar #'locsort (selection-locations selection))
                      (list (locsort focus))))
         (converts (loop for sort in (slog (cutbuffer-sorts clipboard))
                         for i from 0
                         collect (paste-conversion
                                  sort (nth (min i (1- (length targets))) targets)))))
    (when (every #'identity converts)
      (let ((forms (mapcar #'funcall converts (cutbuffer-forms clipboard))))
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
                   (save-history buffer)
                   (setf (selection buffer) nil)
                   (edit-list (make-location :node (location-node focus) :id slot)
                              (lambda (body)
                                (let ((elts (elements body)))
                                  `(,@(subseq elts 0 start) ,@forms ,@(subseq elts end))))
                              (append-id slot (+ start (length forms) -1))
                              buffer (stack buffer)))))
              ((= 1 (length forms))
               (swap-node focus (first forms) buffer)))))))

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

(defun paste-zipper (buffer zipper)
  "Pastes from (zipper buffer) around the current selection using fill-zipper,
modifying the ast."
  (let ((focus (focus buffer))
        (selection (selection-at-focus buffer)))
    (if selection
        (multiple-value-bind (low high) (selection-range selection)
          (let ((forms (selection-forms selection))
                (slot (selection-parent-id selection)))
            (save-history buffer)
            (edit-list (selection-list-location selection)
                       (lambda (body)
                         (let ((elts (elements body)))
                           `(,@(subseq elts 0 low)
                             ,(fill-zipper zipper forms)
                             ,@(subseq elts (1+ high)))))
                       (append-id slot low) buffer (stack buffer))))
        (when (eq 'parse:eval-form (locsort focus))
          (swap-node focus (fill-zipper zipper (list (node-at focus))) buffer)))
    t))

(setf (gethash (tui-sys:make-event :kind #\v :controlp t) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        ;; these are mutually exclusive as cutting a zipper nulls the cutbuffer
        (cond ((zipper buffer) (paste-zipper buffer (zipper buffer)))
              ((cutbuffer buffer) (paste-forms buffer (cutbuffer buffer))))
        t))

;;; completions
(defun render-completion (sym index state rect)
  (when (plusp (tui:rect-rows rect))
    (let ((prefix (parse:name (anchor state)))
          (selectedp (= index (selection state))))
      (flet ((style (bold)
               (tui:make-style :fg #xeeeeee
                               :bg (if selectedp #x2aa198 #x0)
                               :boldp bold
                               :italicp nil
                               :reversep nil
                               :underlinep nil)))
        (tui:fill-rect (style nil) (tui:copy-rect rect :x 0 :y 0 :rows 1) rect
                       :char #\space)
        (tui:puts* (string-downcase prefix) 1 1 rect (style t))
        (tui:puts* (string-downcase
                    (nth-value 1 (starts-with-subseq prefix (string sym) :return-suffix t)))
                   1 (+ 1 (length prefix)) rect (style nil)))
      (make-instance 'tui:view :rect (tui:copy-rect rect :rows 1)))))

(defun render-completion-window (buffer)
  (when-let (state (completion-state buffer))
    (let* ((bounds (tui::screen-rect))
           (anchor (anchor state))
           (anchor-rect (node-screen-rect buffer anchor))
           (maxlen (loop for candidate in (candidates state)
                         maximize (tui:display-width (string candidate))))
           (rect (tui:clamp-rect
                  (tui:make-rect :x (tui:rect-x anchor-rect)
                                 :y (tui:rect-y2 anchor-rect)
                                 :rows (tui:rect-rows bounds)
                                 :cols maxlen)
                  bounds)))
      (tui:with-vertical (rect)
        (loop for candidate in (candidates state)
              for index from 0
              until (tui:full)
              do (tui:place (rect) (render-completion candidate index state rect)))))))

(defun make-cell-buffer (rows cols)
  (let ((buffer (make-array (list rows cols))))
    (dotimes (i (array-total-size buffer) buffer)
      (setf (row-major-aref buffer i) (tui::make-cell)))))

(defun render-one-toplevel (index start forms buffer rows cols)
  "Renders the form at `index' into a segment placed at `start'."
  (loop with form = (nth index forms)
        for capacity = (max 2 (gethash form (stage-cache buffer) rows))
          then (* 2 capacity)
        for cells = (make-cell-buffer capacity cols)
        do (let* ((tui::*put-buffer* cells)
                  (rect (tui:make-rect :x 0 :y 0 :rows capacity :cols cols))
                  (location (make-location :node forms :id index)))
             (tui:fill-rect (tui:make-style :bg #x0) rect rect)
             (let* ((view (render-node form (list location) buffer rect))
                    (used (tui:rect-y2 (tui:rect view))))
               ;; equality can mean clipping; require spare space
               (when (< used capacity)
                 (setf (gethash form (stage-cache buffer)) (ceiling (* 1.2 used)))
                 (return (make-segment :index index :start start :rows used
                                       :buffer cells :view view)))))))

(defun segment-end (segment toplevel-count)
  "Returns the absolute buffer row after `segment', including its inter-form separator."
  (+ (segment-start segment) (segment-rows segment)
     (if (< (segment-index segment) (1- toplevel-count)) 1 0)))

(defun stage-toplevel-window (buffer scroll focus-index screen-rows screen-cols)
  "Returns staged segments and the viewport row revealing the focus."
  (let* ((forms (ast buffer))
         (count (length forms))
         (index (scroll-position-index scroll)))
    ;; keep a predecessor above both anchor and focus for upward movement.
    (loop with viewport = 0
          for i from (max 0 (1- (min index focus-index))) below count
          for start = 0 then (segment-end segment count)
          for segment = (render-one-toplevel i start forms buffer
                                             screen-rows screen-cols)
          do (translate-view-y (segment-view segment) start)
             (when (and (= i index) (<= index focus-index))
               (setf viewport (+ start (min (scroll-position-row scroll)
                                            (segment-rows segment)))))
             (when (= i focus-index)
               (let ((rect (tui:rect (gethash :focus-view (redisplay-cache buffer)))))
                 (setf viewport (if (< focus-index index)
                                    (tui:rect-y rect)
                                    (reveal-focus-row viewport rect screen-rows)))))
          collect segment into segments
          ;; include one whole successor beyond the resolved viewport and focus.
          until (and (> i focus-index) (>= start (+ viewport screen-rows)))
          finally (return (values segments viewport)))))

(defun scroll-position-at-row (segments row)
  "Returns the file position at staged `row' in `segments'."
  (let* ((segment (or (find-if (lambda (segment)
                                 (<= row (+ (segment-start segment) (segment-rows segment))))
                               segments)
                      (lastcar segments)))
         (offset (max 0 (- row (segment-start segment)))))
    (make-scroll-position :index (segment-index segment)
                          :row (min offset (segment-rows segment)))))

(defun top-level-focus-index (buffer)
  "Returns the top-level index containing the focus in `buffer'. Assumes the toplevel
is a list and never focused."
  (location-id (lastcar (stack buffer))))

(defun translate-view-y (view delta)
  "Moves `view' and its descendants down by `delta' rows."
  (tui:view-traverse
   view (lambda (v)
          (setf (tui:rect v)
                (tui:copy-rect (tui:rect v) :y (+ delta (tui:rect-y (tui:rect v)))))))
  view)

(defun reveal-focus-row (viewport rect rows)
  "Returns the minimum viewport adjustment needed to show `rect'."
  (cond ((< (tui:rect-y rect) viewport) (tui:rect-y rect))
        ((> (tui:rect-y2 rect) (+ viewport rows))
         (if (> (tui:rect-rows rect) rows)
             (tui:rect-y rect)
             (- (tui:rect-y2 rect) rows)))
        (t viewport)))

(defun render-toplevel-window (buffer bounds)
  "Stages a window, resolves focus geometry and copies its visible cells to uncursed's
buffer."
  (let* ((forms (ast buffer))
         (rows (tui:rect-rows bounds))
         (cols (tui:rect-cols bounds))
         (state (scroll-state buffer))
         (focus-index (top-level-focus-index buffer))
         (scroll (if (not (find focus-index (scroll-state-segments state)
                                :key #'segment-index))
                     ;; for large forward jumps, reset
                     (make-scroll-position :index focus-index)
                     (scroll-state-position state))))
    (multiple-value-bind (segments initial-viewport)
        (stage-toplevel-window buffer scroll focus-index rows cols)
      (let* ((height (segment-end (lastcar segments) (length forms)))
             (location (lastcar (stack buffer)))
             (root (make-instance 'ordered-view
                                  :rect (tui:make-rect :x (tui:rect-x bounds)
                                                       :y (tui:rect-y bounds)
                                                       :rows height :cols cols)
                                  :children (mapcar #'segment-view segments)
                                  :key-handler (global-key-handler forms location)
                                  :focused nil))
             (focus-view (gethash :focus-view (redisplay-cache buffer)))
             (rect (tui:rect focus-view))
             (viewport (max 0 (min initial-viewport (- height rows)))))
        ;; copy segments to the screen
        (flet ((blit-segment (segment viewport)
                 (let ((start (- viewport (segment-start segment))))
                   (loop
                     for row from (max 0 start)
                       below (min (segment-rows segment) (+ rows start))
                     do (loop for col below cols
                              do (setf (aref tui::*put-buffer*
                                             (- (+ row (tui:rect-y bounds)) start)
                                             (+ (tui:rect-x bounds) col))
                                       (aref (segment-buffer segment) row col)))))))
          (dolist (segment segments)
            (blit-segment segment viewport)))
        (setf (scroll-state buffer)
              (make-scroll-state :position (scroll-position-at-row segments viewport)
                                 :rows height
                                 :segments segments
                                 :viewport-row viewport))
        ;; edit: split up setf
        (setf (focus-rect buffer) rect
              (gethash forms (node-views buffer)) root)
        root))))

;;
;;; main render loop
;;

;; using oklab
(defparameter *early-grayramp* #(0 7 #xb))
(defparameter *mid-grayramp* #(#x10 #x16 #x1c #x20 #x28 #x2e #x34 #x3b #x41 #x48 #x4e #x50
                               #x5c #x63 #x6a #x71))

(defun transform-rect-to-screen (rect offset screen-rect)
  (let* ((origin (tui:rect-y screen-rect))
         (y (max origin (+ origin (- (tui:rect-y rect) offset))))
         (bottom (+ origin (- (tui:rect-y2 rect) offset))))
    ;; rect coordinates are unsigned; clip first
    (tui:clamp-rect
     (tui:copy-rect rect :x (+ (tui:rect-x screen-rect) (tui:rect-x rect))
                         :y y :rows (max 0 (- bottom y)))
     screen-rect)))

(defun node-screen-rect (buffer node)
  "Returns the clipped screen rect for `node' in `buffer' or nil if not staged."
  (when-let (view (gethash node (node-views buffer)))
    (transform-rect-to-screen
     (tui:rect view) (scroll-state-viewport-row (scroll-state buffer))
     (buffer-rect buffer))))

(defun draw-semantic-backgrounds (buffer view offset screen-rect)
  (tui:view-traverse
   view
   (lambda (view)
     (when (typep view 'ast-view)
       (let* ((stack (view-stack view))
              (node (node-at (car stack)))
              (rect (transform-rect-to-screen (tui:rect view) offset screen-rect))
              (real-stack-len (1- (length stack)))
              (i (cond
                   ((<= real-stack-len 2)
                    (truncate (svref *early-grayramp* (min 2 real-stack-len))))
                   ((<= 3 real-stack-len (1- (length *mid-grayramp*)))
                    (truncate (svref *mid-grayramp* (min (1- (length *mid-grayramp*))
                                                         real-stack-len))))
                   (t
                    (min 255 (truncate (+ (last-elt *mid-grayramp*)
                                          (* (- real-stack-len (1- (length *mid-grayramp*)))
                                             10))))))))
         (unless (parse:is-atom node)
           (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                          (tui:copy-rect rect :x 0 :y 0) rect))
         ;;
         (if (eq node (focus-binder buffer))
             (tui:fill-rect (tui:make-style :bg (tui:color #x85 #x99 #x00))
                            (tui:copy-rect rect :x 0 :y 0) rect)
             (when (and (lexical-symbol-ref node (car stack))
                        (not (symbol-ref-boundp node stack buffer)))
               (tui:fill-rect (tui:make-style :bg (tui:color #xcb #x4b #x16))
                              (tui:copy-rect rect :x 0 :y 0) rect)))
         ;;
         (when (unexpanded-macro-op-p (car stack))
           (tui:fill-rect (tui:make-style :bg (tui:color #xdc #x32 #x2f))
                          (tui:copy-rect rect :x 0 :y 0) rect))))
     t)))

(defun draw-highlights (buffer view offset screen-rect)
  (tui:view-traverse
   view
   (lambda (view)
     (when (typep view 'ast-view)
       (let ((stack (view-stack view))
             (rect (transform-rect-to-screen (tui:rect view) offset screen-rect)))
         (when (zipper-top-p buffer stack)
           (tui:fill-rect (tui:make-style :bg #xb58900)
                          (tui:copy-rect rect :x 0 :y 0) rect :blend 0.35))
         (when (location-selected-p buffer stack)
           (tui:fill-rect (tui:make-style :bg #x6c71c4)
                          (tui:copy-rect rect :x 0 :y 0) rect :blend 0.35))))
     t))
  (when-let (focus-rect (and (buffer-focused-p buffer) (focus-rect buffer)))
    (let ((rect (transform-rect-to-screen focus-rect offset screen-rect)))
      (tui:fill-rect (tui:make-style :bg #xb58900)
                     (tui:copy-rect rect :x 0 :y 0) rect :blend 0.7))))

(defun buffer-outer-rect (buffer)
  "The screen area available to `buffer'"
  (when (and (floating buffer) (floating-rect buffer))
    (return-from buffer-outer-rect (floating-rect buffer)))
  (let ((ui (buffer-ui buffer)))
    (tui:make-rect :x (min (buffer-x buffer) (tui:cols ui))
                   :y (min (buffer-y buffer) (tui:rows ui))
                   :rows (max 0 (- (tui:rows ui) (buffer-y buffer)))
                   :cols (max 0 (- (tui:cols ui) (buffer-x buffer))))))

(defun buffer-rect (buffer)
  (let* ((rect (buffer-outer-rect buffer))
         (inset (if (floating buffer) 1 0)))
    (tui:copy-rect rect
                   :x (+ (tui:rect-x rect) (min inset (tui:rect-cols rect)))
                   :y (+ (tui:rect-y rect) (min inset (tui:rect-rows rect)))
                   :rows (max 0 (- (tui:rect-rows rect) (* 2 inset)))
                   :cols (max 0 (- (tui:rect-cols rect) (* 2 inset))))))

(defun fit-floating-buffer (buffer)
  "Fits `buffer' to its contents, keeping its border inside the screen."
  (let* ((ui (buffer-ui buffer))
         (rows (max 0 (- (tui:rows ui) 2)))
         (cols (max 0 (- (tui:cols ui) 2)))
         (height 0)
         (width 0))
    (when (and (plusp rows) (plusp cols))
      ;; measurement uses separate caches; the final render establishes node views.
      (let ((measuring (make-buffer :buffer-ui ui :buffer-id (buffer-id buffer)
                                    :buffer-state (buffer-state buffer))))
        (loop for index below (length (ast buffer))
              for segment = (render-one-toplevel index 0 (ast buffer) measuring rows cols)
              do (incf height (+ (segment-rows segment) (if (zerop index) 0 1)))
                 (setf width (max width (tui:rect-x2 (tui:rect (segment-view segment))))))))
    (let ((height (min (tui:rows ui) (+ height 2)))
          (width (min (tui:cols ui) (+ width 2))))
      (setf (floating-rect buffer)
            (tui:make-rect :x (min (buffer-x buffer) (- (tui:cols ui) width))
                           :y (min (buffer-y buffer) (- (tui:rows ui) height))
                           :rows height :cols width)))))

(defun draw-buffer-border (buffer)
  (let* ((rect (buffer-outer-rect buffer))
         (rows (tui:rect-rows rect))
         (cols (tui:rect-cols rect))
         (style (tui:make-style :fg (if (buffer-focused-p buffer) #xb0b0b0 #x606060)
                                :bg 0)))
    (when (and (>= rows 2) (>= cols 2))
      (loop for col from 2 below cols
            do (tui:put #\─ 1 col rect style)
               (tui:put #\─ rows col rect style))
      (loop for row from 2 below rows
            do (tui:put #\│ row 1 rect style)
               (tui:put #\│ row cols rect style))
      (tui:put #\┌ 1 1 rect style)
      (tui:put #\┐ 1 cols rect style)
      (tui:put #\└ rows 1 rect style)
      (tui:put #\┘ rows cols rect style))))

(defun draw-buffer (buffer)
  (when (floating buffer)
    (fit-floating-buffer buffer)
    (draw-buffer-border buffer))
  (let ((bounding-rect (buffer-rect buffer)))
    (when (and (plusp (tui:rect-rows bounding-rect))
               (plusp (tui:rect-cols bounding-rect)))
      (tui:fill-rect (tui:make-style :bg #x0)
                     (tui:copy-rect bounding-rect :x 0 :y 0)
                     bounding-rect
                     :char #\space)
      (let* ((content (render-toplevel-window buffer bounding-rect))
             (offset (scroll-state-viewport-row (scroll-state buffer))))
        (draw-semantic-backgrounds buffer content offset bounding-rect)
        (draw-highlights buffer content offset bounding-rect)
        (setf (buffer-view buffer) content)))))

(defun render-buffer (buffer)
  (let* ((old (render-state buffer))
         (working (make-buffer
                   :buffer-ui (buffer-ui buffer)
                   :buffer-id (buffer-id buffer)
                   :buffer-state (buffer-state buffer)
                   :render-state (make-render-state
                                  :stage-cache (copy-hash-table (stage-cache old))
                                  :scroll-state (scroll-state old)))))
    (draw-buffer working)
    (setf (render-state buffer) (render-state working))))

(defmethod tui:render ((ui ui))
  (tui::clear-buffer tui::*put-buffer*)
  (dolist (buffer (buffers ui))
    (render-buffer buffer))
  ;; buffer views use local coordinates. Global overlays use screen coordinates.
  (let ((completion (when-let (buffer (focused-buffer ui))
                      (when (buffer-view buffer)
                        (render-completion-window buffer)))))
    (make-instance 'tui:view :rect (tui::screen-rect)
                             :children (when completion (list completion)))))

;; note: must go below definitions, next to tui:redisplay
(let ((render-node-methods (closer-mop:generic-function-methods #'render-node))
      (old-render-method (find-method #'tui:render nil '(ui)))
      (fnames '(render-buffer draw-buffer draw-buffer-border fit-floating-buffer buffer-rect
                buffer-outer-rect render-toplevel-window
                symbol-ref-text render-literal-text render-literal-line
                render-delimiter render-symbol-space render-elements
                render-layout-slot slot-render-keys
                macro-call-indentation loop-indentation
                draw-semantic-backgrounds draw-highlights
                render-completion-window render-completion))
      (funs (make-hash-table :test 'eq)))
  (loop for name in fnames
        do (setf (gethash name funs) (fdefinition name)))
  (defun reset-ui ()
    (remove-method #'tui:render (find-method #'tui:render nil '(ui)))
    (add-method #'tui:render old-render-method)
    (loop for method in (closer-mop:generic-function-methods #'render-node)
          do (remove-method #'render-node method))
    (loop for method in render-node-methods
          do (add-method #'render-node method))
    (loop for name in fnames
          do (setf (fdefinition name) (gethash name funs)))))

(let ((redisplay-failing nil))
  (defmethod tui:redisplay-internal :around ((ui ui))
    (restart-case
        (unless (redisplayed-since-input ui)
          (call-next-method)
          (setf redisplay-failing nil)
          (slog* (format nil "~a~a" (make-string 70 :initial-element #\-) 'redisplay-done)))
      (restore-ui-fallback ()
        :report "restore broken redisplay methods, exit if still failing"
        (reset-ui)
        (if redisplay-failing
            (progn (slog 'failed)
                   (tui:stop ui))
            (progn (setf redisplay-failing t)
                   (tui:redisplay ui)))))))

(defun translate-mouse-event (event row-offset &optional (col-offset 0))
  (if (tui:mouse-event-p event)
      (let ((data (copy-structure (tui:event-kind event))))
        (incf (tui:mouse-data-row data) row-offset)
        (incf (tui:mouse-data-col data) col-offset)
        (tui-sys:make-event :kind data
                            :shiftp (tui:event-shiftp event)
                            :altp (tui:event-altp event)
                            :controlp (tui:event-controlp event)
                            :metap (tui:event-metap event)))
      event))

(defun rect-contains-point-p (rect row col)
  "Whether `rect' contains the one-based screen point `row', `col'."
  (and (< (tui:rect-y rect) row (1+ (tui:rect-y2 rect)))
       (< (tui:rect-x rect) col (1+ (tui:rect-x2 rect)))))

(defun buffer-at-point (ui row col)
  "Finds the last drawn buffer containing the screen point `row', `col' (one-based)."
  (find-if (lambda (buffer)
             (when-let (view (buffer-view buffer))
               (rect-contains-point-p
                (if (floating buffer)
                    (buffer-outer-rect buffer)
                    (tui:clamp-rect (tui:rect view) (buffer-rect buffer))) row col)))
           (reverse (buffers ui))))

(defrecord ui-effects ()
  ((pane-drag :init nil :type (or null pane-drag))
   (selection :type (or null selection))
   (zipper :type (or null zipper))
   (cutbuffer :type (or null cutbuffer))
   (focused-buffer)
   (new-buffers :init nil :type list))
  (:representation :class))

(defun handle-ui-event (ui event)
  "Returns working buffers and global effects without changing the published `ui'."
  (let* ((working (shallow-copy-record ui))
         (originals (buffers ui))
         (copies (mapcar (lambda (buffer)
                           (make-buffer :buffer-ui working :buffer-id (buffer-id buffer)
                                        :buffer-state
                                        (copy-buffer-state (buffer-state buffer))
                                        :render-state (render-state buffer)))
                         originals)))
    (setf (buffers working) copies
          (slot-value working 'focused-buffer) (nth (position (focused-buffer ui) originals)
                                                    copies))
    (dispatch-ui-event working event)
    (let ((new (mapcar (lambda (buffer)
                         (make-buffer :buffer-ui ui :buffer-id (buffer-id buffer)
                                      :buffer-state (buffer-state buffer)))
                       (nthcdr (length copies) (buffers working)))))
      (values copies
              (make-ui-effects
               :selection (selection working) :zipper (zipper working)
               :cutbuffer (cutbuffer working) :new-buffers new
               :pane-drag (pane-drag working)
               :focused-buffer (find (buffer-id (focused-buffer working))
                                     (append originals new) :key #'buffer-id))))))

(defmethod tui:dispatch-event ((ui ui) event)
  (let ((revision (revision ui)))
    (multiple-value-bind (copies effects) (handle-ui-event ui event)
      ;; This is necessary to handle reentrancy as recursive dispatch may
      ;; already have published a newer state, in which case we don't commit
      (when (= revision (revision ui))
        (loop for buffer in (buffers ui) for copy in copies
              do (setf (buffer-state buffer) (buffer-state copy)))
        (setf (buffers ui) (append (buffers ui) (new-buffers effects)))
        (setf (pane-drag ui) (pane-drag effects)
              (selection ui) (selection effects)
              (zipper ui) (zipper effects)
              (cutbuffer ui) (cutbuffer effects)
              (slot-value ui 'focused-buffer) (focused-buffer effects))
        (incf (revision ui))))))

(defun handle-pane-drag (ui event target)
  "Moves a captured pane until mouse release, consuming the associated events."
  (when (tui:mouse-event-p event)
    (let* ((mouse (tui:event-kind event))
           (state (tui:mouse-data-state mouse))
           (drag (pane-drag ui)))
      (cond
        ((eq state :release)
         (setf (pane-drag ui) nil)
         (not (null drag)))
        (drag
         (when (eq state :drag)
           (when-let (buffer (find (pane-drag-buffer-id drag) (buffers ui)
                                  :key #'buffer-id))
             (let ((rect (buffer-outer-rect buffer)))
               (setf (buffer-x buffer)
                     (max 0 (min (- (tui:mouse-data-col mouse) (pane-drag-offset-x drag))
                                 (- (tui:cols ui)
                                    (if (floating buffer) (tui:rect-cols rect) 1)))))
               (setf (buffer-y buffer)
                     (max 0 (min (- (tui:mouse-data-row mouse) (pane-drag-offset-y drag))
                                 (- (tui:rows ui)
                                    (if (floating buffer) (tui:rect-rows rect) 1))))))))
         t)
        ((and target (eq state :click) (eq (tui:mouse-data-button mouse) :left)
              (tui:event-altp event))
         (let ((rect (buffer-outer-rect target)))
           (setf (focused-buffer ui) target)
           (setf (pane-drag ui)
                 (make-pane-drag
                  :buffer-id (buffer-id target)
                  :offset-x (- (tui:mouse-data-col mouse) (tui:rect-x rect))
                  :offset-y (- (tui:mouse-data-row mouse) (tui:rect-y rect)))))
         t)))))

(defun dispatch-ui-event (ui event)
  (let* ((mouse (when (tui:mouse-event-p event) (tui:event-kind event)))
         ;; currently only completion
         (overlay (when (and mouse (tui:root-view ui))
                    (find-if (lambda (view)
                               (rect-contains-point-p (tui:rect view)
                                                      (tui:mouse-data-row mouse)
                                                      (tui:mouse-data-col mouse)))
                             (tui:children (tui:root-view ui)))))
         (target (when mouse
                   (buffer-at-point ui (tui:mouse-data-row mouse)
                                    (tui:mouse-data-col mouse)))))
    (when (handle-pane-drag ui event (unless overlay target))
      (return-from dispatch-ui-event))
    ;; completion should capture events early
    (when overlay
      (tui:view-traverse overlay (lambda (view)
                                   (funcall (tui:mouse-handler view) view event)
                                   t))
      (return-from dispatch-ui-event))
    ;;
    (when (and target (eq (tui:mouse-data-state mouse) :click)
               (member (tui:mouse-data-button mouse) '(:left :middle :right)))
      (setf (focused-buffer ui) target))
    ;;
    (when-let* ((buffer (focused-buffer ui))
                (view (buffer-view buffer)))
      (when (or (not mouse) (eq target buffer))
        (let ((event (translate-mouse-event
                      event (- (scroll-state-viewport-row (scroll-state buffer))
                               (tui:rect-y (buffer-rect buffer)))
                      (- (tui:rect-x (buffer-rect buffer))))))
          (tui:view-traverse
           view (lambda (view)
                  (if mouse
                      (funcall (tui:mouse-handler view) view event)
                      (when (tui:focused view)
                        (funcall (tui:key-handler view) view event buffer)))
                  t)))))))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (nil "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\q)
             (tui:event-controlp event))
        (tui:stop ui)
        (call-next-method))
    (slog* (format nil "~a~a" (make-string 70 :initial-element #\-) 'event-handled))))

(defmethod tui:dispatch-event :after ((ui ui) event)
  (declare (ignore event))
  (setf (redisplayed-since-input ui) nil)
  (setf (redisplayed-since-input ui) (tui:redisplay ui)))

(defmethod tui:handle-resize :after ((ui ui))
  (setf (redisplayed-since-input ui) nil))
