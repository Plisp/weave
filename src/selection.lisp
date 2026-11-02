(in-package #:weave-tui)

(defun end-selection-mode (buffer)
  (when-let (selection (selection buffer))
    (setf (selection buffer) (copy-selection selection :activep nil))))

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

(defun zipper-depth (zipper)
  (length (zipper-stack zipper)))

(defun selection-at-focus (buffer)
  (when (location-selected-p buffer (stack buffer))
    (selection buffer)))

(defun copy-locations (locations)
  (make-cutbuffer :forms (mapcar (lambda (loc) (parse:copy-node (node-at loc))) locations)
                  :sorts (mapcar #'locsort locations)))

(defmethod (setf selection) (value (buffer buffer))
  (setf (selection (buffer-ui buffer)) value))

(defmethod zipper ((buffer buffer)) (zipper (buffer-ui buffer)))
(defmethod (setf zipper) (value (buffer buffer))
  (setf (zipper (buffer-ui buffer)) value))

(defmethod cutbuffer ((buffer buffer)) (cutbuffer (buffer-ui buffer)))
(defmethod (setf cutbuffer) (value (buffer buffer))
  (setf (cutbuffer (buffer-ui buffer)) value))

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
