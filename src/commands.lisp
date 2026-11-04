(in-package #:weave-tui)

;;; basic

;; ASSUME leaf nodes are non-overlapping
;; we index them into an array of blocks sorted by line then column
(defun build-atom-array (tree rows)
  "Coordinates produced are in staging coordinates."
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
  "Takes a y-offset from 0 to rows (relative to staging coordinates),
this way it's possible to search the first row. `col' should indicate some preferred
column. Returns NIL if not found."
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
                        (aref atom-array y)
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

(setf (gethash (tui-sys:make-event :kind #\e :controlp t) *default-key-handlers*)
      (lambda (&rest args)
        (declare (ignore args))
        (break)))

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
          (t (cdr stack)))))

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

;;; buffer

(defun add-buffer (ui &key (ast (list (hole))) (x 0) (y 0)
                        source (package "CL-USER") floating)
  "Adds a buffer containing `ast' to `ui', optionally associated with `source'.
The anchor coordinates X, Y are absolute."
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

;;; nodes

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
                 ;; Macro reanalysis may have copied descendants. Resolve the saved
                 ;; path ids through the new tree
                 (values (loop for loc in newstack
                               for parent = newnode then (node-at fresh)
                               for fresh = (make-location :node parent :id (location-id loc))
                               collect fresh into path
                               finally (return (nreverse path)))
                         newnode)
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

(defun ast-replace (loc updater buffer &optional (stack (stack buffer)))
  (if (typep (location-node loc) 'parse:macro-call)
      (replace-in-macro-call loc (funcall updater (node-at loc)) buffer)
      (multiple-value-call #'commit-edit buffer (rebuild-spine loc updater stack))))

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

(defun insert-or-jump (loc index buffer stack &key (item (hole)) tail)
  "Moves to the hole at `index' of the list at `loc' and down the ids in `tail' if passed,
otherwise inserts `item' at `index' and focuses down `tail' within it. `stack' is the
part of the buffer stack starting from the node of `loc'."
  (let ((path (cons index tail)))
    (if (typep (parse:value-at-path loc path) 'parse:hole)
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

(defun symbol-char-p (c)
  (or (alphanumericp c) (find c "+-*/=<>!?&%$_~^.:@[]{}")))

(defgeneric handle-key (node view location ui event)
  (:documentation "event is a uncursed key event. If this node is active then
adjust cursor state in the context, specialized for the `cursor'.
If this returns NIL, propagate up the cursor stack.")
  (:method (node view location context event) nil))

;;; holes

(defmethod handle-key ((node hole) view location buffer event)
  (let ((c (tui:event-kind event)))
    (flet ((start-completion (newnode)
             (setf (completion-state buffer)
                   (make-completion-state
                    :anchor newnode
                    :candidates (completion-candidates buffer newnode)))))
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
                (start-completion newnode)
                (swap-node location newnode buffer)))
             ((digit-char-p c)
              (let ((newnode (make-instance 'parse:integer-literal :str (string c))))
                (swap-node location newnode buffer)))
             ;; TODO needs to deal with comma depth and insertion
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
               (start-completion newnode)
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
              (swap-node location (make-instance 'parse:integer-literal :str (string c))
                         buffer)))))))))

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
          (when (and (= (length stack) 1)
                     (not (typep (node-at (focus buffer))
                                 '(or parse:string-literal parse:character-literal))))
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
                    (selection (and (< (selection state) (length (candidates state)))
                                    (aref (candidates state) (selection state)))))
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
                   (parse:fun-designator
                    (typecase (location-node (focus buffer))
                      (parse:function-form
                       (if (eq s 'lambda)
                           (progn
                             (swap-node (focus buffer) (build-call name-node) buffer)
                             (focus-first-hole buffer))
                           (swap-node (focus buffer) name-node buffer)))
                      (parse:function-call
                       (when (eq (locsort (second (stack buffer))) 'parse:eval-form)
                         (when-let (call (build-call name-node
                                                     (parse:body
                                                      (location-node (focus buffer)))))
                           (swap-node (second (stack buffer)) call buffer)
                           (focus-first-hole buffer))))))
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
          (when (and (eq 'parse:eval-form (locsort (focus buffer)))
                     (or (typep ref 'parse:symbol-ref)
                         (and (typep ref 'parse:hole)
                              (or (string= (parse:text ref) "+")
                                  (string= (parse:text ref) "-")))))
            (when (typep ref 'parse:hole)
              (setf ref (make-instance 'parse:symbol-ref
                                       :name (parse:text ref)
                                       :home-package (buffer-package buffer))))
            ;;
            (when-let (call (build-call ref))
              (swap-node (focus buffer) call buffer)
              (focus-first-hole buffer)
              t)))))

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

(setf (gethash (tui-sys:make-event :kind #\rubout) *default-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (rubout-eval-form buffer)))

;;; holes

(defun start-literal (node location buffer c)
  "Convert generic holes to the appropriate literal type"
  (let* ((prefix (text node))
         (new
           (cond
             ((string= prefix "#")
              (case (char-downcase c)
                (#\\ (make-instance 'parse:character-literal :str "#\\"))
                (#\* (make-instance 'parse:bitvector-literal :str "#*"))
                (#\' (make-instance 'parse:function-form :op 'parse:read-function
                                                         :fun-designator (hole)))
                ((#\b #\o #\x)
                 (make-instance 'parse:integer-literal
                                :radix (ecase (char-downcase c) (#\b 2) (#\o 8) (#\x 16))
                                :str (strcat "#" (string c))))
                (#\( (make-instance 'parse:ref-list :kind :vector))
                (otherwise nil)))
             ((find c "#+-") (hole (string c)))
             ((char= c #\") (make-instance 'parse:string-literal :str "\"\""))
             ((digit-char-p c)
              (make-instance 'parse:integer-literal
                             :str (strcat prefix (string c))))
             ((and (member prefix '("-" "+") :test #'string=) (symbol-char-p c))
              (make-instance 'parse:symbol-ref
                             :name (string-upcase (strcat prefix (string c)))
                             :home-package (buffer-package buffer))))))
    (when new
      (let ((funquotep (typep new 'parse:function-form)))
        (swap-node location
                   (if (and funquotep (eq (locsort location) 'parse:unevaluated))
                       (parse:to-syntax new) new)
                   buffer)
        (when funquotep
          (if (typep (node-at (focus buffer)) 'parse:function-form)
              (descend buffer 'parse:fun-designator)
              (refocus buffer (append-id (location-id (focus buffer)) 1)))))
      t)))

(defmethod handle-key :around ((node hole) view location buffer event)
  (let ((c (tui:event-kind event)))
    (if (and (is-regular-char-event event)
             (member (locsort location) '(parse:eval-form parse:unevaluated string))
             (start-literal node location buffer c))
        t
        (call-next-method))))

(defmethod handle-key ((node parse:integer-hole) view location buffer event)
  (declare (ignore view))
  (when (is-regular-char-event event)
    (let ((c (tui:event-kind event)))
      (cond ((digit-char-p c (parse::radix node))
             (swap-node location (make-instance 'parse:integer-literal
                                                :radix (parse::radix node)
                                                :str (strcat (text node) (string c)))
                        buffer))
            ((find c "+-")
             (swap-node location (make-instance 'parse:integer-hole
                                                :radix (parse::radix node)
                                                :text (string c))
                        buffer))
            ((char= c #\Rubout)
             (unless (zerop (length (text node)))
               (swap-node location (make-instance 'parse:integer-hole
                                                  :radix (parse::radix node))
                          buffer))
             t)
            ;; eat invalid input before it propagates up
            (t t)))))

(defgeneric literal-input (node character)
  (:method ((node parse:literal) character) nil))

(defmethod literal-input ((node parse:string-literal) c)
  (let* ((*read-eval* nil)
         (value (read-from-string (parse:str node)))
         (new (if (char= c #\Rubout)
                  (subseq value 0 (max 0 (1- (length value))))
                  (strcat value (string c)))))
    (make-instance 'parse:string-literal :str (write-to-string new :escape t))))

(defmethod literal-input ((node parse:character-literal) c)
  (let ((s (parse:str node)))
    (make-instance 'parse:character-literal
                   :str (if (char= c #\Rubout)
                            (subseq s 0 (max 2 (1- (length s))))
                            (strcat s (string c))))))

(defmethod literal-input ((node parse:bitvector-literal) c)
  (when (or (find c "01") (char= c #\Rubout))
    (let ((s (parse:str node)))
      (make-instance 'parse:bitvector-literal
                     :str (if (char= c #\Rubout)
                              (subseq s 0 (max 2 (1- (length s))))
                              (strcat s (string c)))))))

(defmethod literal-input ((node parse:integer-literal) c)
  (let* ((s (parse:str node))
         (base (parse::radix node))
         (prefix-end (if (char= (char s 0) #\#)
                         (if-let (r (position #\r s :test #'char-equal))
                           (1+ r) 2)
                         0)))
    (cond ((and (find c "+-") (= (length s) prefix-end))
           (make-instance 'parse:integer-literal :radix base
                                                 :str (strcat s (string c))))
          ((digit-char-p c base)
           (if (and (zerop prefix-end) (find #\. s))
               (make-instance 'parse:float-literal
                              :str (strcat s (string c)))
               (make-instance 'parse:integer-literal
                              :radix base
                              :str (strcat s (string c)))))
          ((char= c #\Rubout)
           (let ((new (subseq s 0 (max prefix-end (1- (length s))))))
             (if (member new '("" "+" "-") :test #'string=)
                 (make-instance 'parse:integer-hole :radix base :text new)
                 (make-instance 'parse:integer-literal :radix base :str new))))
          ((and (zerop prefix-end) (find c ".eEsSfFdDlL")
                (not (and (char= c #\.) (find #\. s))))
           (make-instance 'parse:float-literal :str (strcat s (string c)))))))

(defmethod literal-input ((node parse:float-literal) c)
  (let ((s (parse:str node)))
    (cond ((or (digit-char-p c)
               (and (find c "+-") (find (char s (1- (length s))) "eEsSfFdDlL"))
               (and (find c "eEsSfFdDlL")
                    (not (find-if (lambda (c) (find c "eEsSfFdDlL")) s)))
               (and (char= c #\.) (not (find #\. s))
                    (not (find-if (lambda (c) (find c "eEsSfFdDlL")) s))))
           (make-instance 'parse:float-literal :str (strcat s (string c))))
          ((char= c #\Rubout)
           (let ((new (subseq s 0 (max 0 (1- (length s))))))
             (if (zerop (length new)) (make-instance 'parse:integer-hole)
                 (make-instance (if (find-if (lambda (c) (find c ".eEsSfFdDlL")) new)
                                    'parse:float-literal 'parse:integer-literal)
                                :str new)))))))

(defmethod handle-key ((node parse:literal) view location buffer event)
  (declare (ignore view))
  (when (is-regular-char-event event)
    (let ((c (tui:event-kind event)))
      (cond
        ((and (typep node 'parse:integer-literal) (char= c #\/)
              (member (locsort location) '(parse:eval-form parse:unevaluated)))
         (swap-node location
                    (make-instance
                     'parse:ratio-literal :str ""
                                          :numerator-node
                                          (if (ends-with #\. (parse:str node))
                                              (make-instance 'parse:integer-literal
                                                             :radix (parse::radix node)
                                                             :str (string-right-trim
                                                                   "." (parse:str node)))
                                              node)
                                          :denominator-node
                                          (make-instance 'parse:integer-hole
                                                         :radix (parse::radix node)))
                    buffer)
         (descend buffer 'parse:denominator-node)
         t)
        (t
         (when-let (new (literal-input node c))
           (when (and (eq (locsort location) 'parse:integer-literal)
                      (not (typep new '(or parse:integer-literal parse:integer-hole))))
             (return-from handle-key t))
           ;;
           (swap-node location
                      (if (and (typep new 'parse:integer-hole)
                               (not (eq (locsort location) 'parse:integer-literal)))
                          (hole (text new)) (parse::copy-anchors node new))
                      buffer)
           t))))))

(defun completion-candidates (buffer &optional (node (node-at (focus buffer))))
  "Collects visible symbols, or the visibility suggested by `node's package prefix."
  (let* ((prefix (and (typep node 'parse:symbol-ref)
                      (symbol-ref-prefix node (buffer-package buffer))))
         (package (if prefix
                      (parse:home-package node)
                      (buffer-package buffer)))
         (result (make-array 0 :adjustable t :fill-pointer 0))
         (seen (make-hash-table :test #'eq)))
    (unless package
      (return-from completion-candidates result))
    ;;
    (do-symbols (symbol package)
      (when (and (not (gethash symbol seen))
                 (or (null prefix)
                     (eq (nth-value 1 (find-symbol (symbol-name symbol) package))
                         (if (ends-with-subseq "::" prefix) :internal :external))))
        (setf (gethash symbol seen) t)
        (unless (zerop (length (string symbol)))
          (vector-push-extend symbol result))))
    result))

(defmethod handle-key ((node parse:symbol-ref) view location buffer event)
  (declare (ignore view))
  (when (is-regular-char-event event)
    (let* ((name (parse:name node))
           (prefix (symbol-ref-prefix node (buffer-package buffer)))
           (home (if prefix (parse:home-package node) (buffer-package buffer)))
           (c (tui:event-kind event)))
      (labels ((replace-ref (name home prefix &optional (refilter t) (complete t))
                 (let ((new (make-instance 'parse:symbol-ref :name name :home-package home
                                                             :input-prefix prefix)))
                   (swap-node location new buffer)
                   (setf (completion-state buffer)
                         (when complete
                           (if (and (not refilter) (completion-state buffer))
                               (copy-completion-state (completion-state buffer)
                                                      :anchor new :selection 0)
                               (make-completion-state
                                :anchor new
                                :candidates (completion-candidates buffer new)))))
                   t)))
        (cond
          ((char= c #\:)
           (cond
             ((null prefix)
              (let* ((context (buffer-package buffer))
                     (package (or (cdr (assoc name
                                              (slynk-backend:package-local-nicknames context)
                                              :test #'string-equal))
                                  (find-package name))))
                (when package
                  (replace-ref "" package (strcat name ":") t t))))
             ((and (zerop (length name))
                   (not (ends-with-subseq "::" prefix)))
              ;; there can be many internal symbols, don't consider them all
              (replace-ref "" home (strcat prefix ":") t nil)))
           t)
          ((symbol-char-p c)
           (if (string= ":" name) ; keyword
               (replace-ref (string (char-upcase c)) (find-package "KEYWORD") ":" t t)
               (replace-ref (strcat name (string (char-upcase c))) home prefix nil)))
          ((char= c #\Rubout)
           (cond
             ((plusp (length name))
              (if (and (= 1 (length name)) (null prefix))
                  (swap-node location (hole) buffer)
                  (replace-ref
                   (subseq name 0 (1- (length name))) home prefix t
                   ;; do not complete without at least one filtered letter,
                   ;; unless external non-keyword, in which case matches should be few
                   (or (< 1 (length name))
                       (let ((plen (length prefix)))
                         (and prefix (char= #\: (char prefix (- plen 1)))
                              (> plen 1) (char/= #\: (char prefix (- plen 2)))))))))
             ((and prefix (ends-with-subseq "::" prefix))
              (replace-ref "" home (subseq prefix 0 (1- (length prefix)))))
             (prefix
              (if (= 1 (length prefix))
                  (swap-node location (hole) buffer)
                  (replace-ref (subseq prefix 0 (1- (length prefix)))
                               (buffer-package buffer) nil nil nil))))
           t))))))

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

;;; functions
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

(setf (gethash (tui-sys:make-event :kind #\rubout) *fun-key-handlers*)
      #'rubout-eval-form)

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
      for s = (string sym)
      do (when (= 1 (length (string s)))
           (setf (gethash (tui-sys:make-event :kind (schar s 0) :altp t)
                          *global-key-handlers*)
                 ;; oops bug if we don't compute from s prior to its mutation by loop
                 (let ((sym* sym))
                   (lambda (view buffer)
                     (declare (ignore view))
                     (wrap-arith sym* buffer))))))

(setf (gethash (tui-sys:make-event :kind #\i :controlp t) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (when-let* ((state (completion-state buffer))
                    (count (plusp (length (candidates state)))))
          (setf (completion-state buffer)
                (copy-completion-state
                 state :selection (mod (1+ (selection state))
                                       (length (candidates state))))))))

(setf (gethash (tui-sys:make-event :kind #\tab :shiftp t) *global-key-handlers*)
      (lambda (view buffer)
        (declare (ignore view))
        (when-let* ((state (completion-state buffer))
                    (count (plusp (length (candidates state)))))
          (setf (completion-state buffer)
                (copy-completion-state
                 state :selection (mod (1- (selection state))
                                       (length (candidates state))))))))

;;; macros

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

;;; arbitrary

(defparameter *layout-key-handlers* (make-hash-table :test #'equal)
  "Map from event to callbacks (layout, child-stack, buffer) -> bool whether to propagate.")
(defmethod handle-key ((node parse:irregular-form) view location buffer event)
  (declare (ignore view))
  (when-let ((layout (gethash (type-of node) *layouts*))
             (handler (gethash event *layout-key-handlers*)))
    (let ((i (position-if (lambda (l) (location= location l)) (stack buffer))))
      (unless (zerop i)
        (funcall handler layout (nthcdr (1- i) (stack buffer)) buffer)))))

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

;;; lambdas

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

;;; misc

;; TODO inspector
(defun reify (object)
  (typecase object
    ((or number string character bit-vector)
     (parse:reader-literal object (write-to-string object :escape t)))
    (list (make-instance 'parse:quote-form
                         :op 'parse:read-quote
                         :thing (apply #'ref-list (mapcar #'reify object))))
    (boolean (make-instance 'parse:symbol-ref
                            :home-package #.(find-package "CL")
                            :name (princ-to-string object)))
    (t (ref-list))))

;; TODO capture i/o using gray streams
(setf (gethash (tui-sys:make-event :kind #\e :altp t) *global-key-handlers*)
      (lambda (view buffer)
        (let ((stack (stack buffer))
              (rect (staging-to-absolute-coordinates ; screen-absolute coordinates
                     (tui:rect view) (scroll-state-viewport (scroll-state buffer))
                     (buffer-rect buffer))))
          (handler-bind ((warning #'muffle-warning))
            (parse::with-temporary-interning (interned)
              (let* ((raw (parse::strip-wrappers
                           (parse:to-syntax (node-at (car stack)))
                           interned))
                     (res (let ((*standard-output* (make-broadcast-stream)))
                            (eval raw))))
                (slog* (format nil "eval produced ~a" res))
                (add-buffer
                 (buffer-ui buffer)
                 :ast (list (reify res)) :floating t
                 :package (buffer-package buffer)
                 :x (tui:rect-x2 rect) :y (tui:rect-y rect))))))))

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

(defun close-focused-buffer (ui)
  "Closes the focused buffer in `ui' and clears related context."
  (when-let (buffer (focused-buffer ui))
    (when (typep (buffer-state buffer) 'debug-state)
      (when-let* ((captured (find 'abort (first (debug-restarts buffer))
                                  :key #'restart-name))
                  (restart (find-restart captured)))
        (invoke-restart-interactively restart))
      (return-from close-focused-buffer))
    ;; careful
    (let* ((remaining (remove buffer (buffers ui)))
           (next (or (cadr (member buffer (buffers ui))) (lastcar remaining))))
      (dolist (candidate (buffers ui))
        (when (typep (buffer-state candidate) 'buffer-state)
          (setf (completion-state candidate) nil)))
      ;; nullify global state
      (when (and (selection ui)
                 (eq (selection-buffer-id (selection ui)) (buffer-id buffer)))
        (setf (selection ui) nil))
      (when (and (pane-drag ui)
                 (eq (pane-drag-buffer-id (pane-drag ui)) (buffer-id buffer)))
        (setf (pane-drag ui) nil))
      ;;
      (setf (buffers ui) remaining
            (focused-buffer ui) next)
      (unless remaining
        (add-buffer ui :ast (demo-ast) :package *package*)))))

(setf (gethash (tui-sys:make-event :kind #\w :controlp t) *ui-key-handlers*)
      #'close-focused-buffer)
