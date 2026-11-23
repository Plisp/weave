(in-package #:weave-tests)

;;; syntax

(defun sym (name &optional (package "COMMON-LISP"))
  (make-instance 'parse:symbol-ref :name name :home-package (find-package package)))

(defun sx (x)
  "A readable rendering of syntax or AST `x', holes as _."
  (typecase x
    (parse:hole "_")
    (parse:symbol-ref (parse:name x))
    (parse:literal (parse:str x))
    (parse:ref-list (format nil "(~{~a~^ ~})" (mapcar #'sx (parse:elements x))))
    (list (format nil "(~{~a~^ ~})" (mapcar #'sx x)))
    (symbol (symbol-name x))
    (t (sx (parse:to-syntax x)))))

(defun parse (source)
  (parse:parse-from-string (parse:make-client source)))

;;; ui

(defun ui-for (source)
  (let* ((parsed (if (stringp source) (parse source) source))
         (ast (cond ((null parsed) (list (parse:hole)))
                    ((listp parsed) parsed)
                    (t (list parsed))))
         (ui (make-instance 'w::ui)))
    (w::add-buffer ui :ast ast :package *package*)))

(defun goto (ui &rest path)
  "Focuses `path' from the first root"
  (setf (w::stack ui) (last (w::stack ui)))
  (w::refocus ui (if (= 1 (length (w::ast ui))) 0 (pop path)))
  (dolist (id path ui)
    (w::descend ui id)))

(defun render-buffer (ui &key (rows 12) (cols 70))
  (let ((buffer (make-array (list rows cols))))
    (dotimes (i (array-total-size buffer))
      (setf (row-major-aref buffer i) (tui::make-cell)))
    (setf (slot-value (w::buffer-ui ui) 'tui::%canvas) buffer)
    (let ((tui::*put-buffer* buffer))
      (setf (slot-value (w::buffer-ui ui) 'tui::%root-view) (tui:render (w::buffer-ui ui))))
    buffer))

(defun cell-bgs (ui n &key (row 0))
  "The background colours of the first `n' cells of `row'."
  (let ((buffer (render-buffer ui)))
    (loop for x below n collect (tui-sys:bg (tui::cell-style (aref buffer row x))))))

(defun lines (ui &key (rows 12) (cols 70))
  (let ((buffer (render-buffer ui :rows rows :cols cols)))
    (loop for y below rows
          for line = (string-right-trim
                      " " (with-output-to-string (s)
                            (dotimes (x cols)
                              (write-string (tui::cell-string (aref buffer y x)) s))))
          unless (zerop (length line))
            collect line)))

(defun draw (ui)
  "Render `ui', rows separated by /."
  (format nil "~{~a~^ / ~}" (lines ui)))

(defun code (ui)
  (let ((forms (w::ast ui)))
    (sx (if (= 1 (length forms)) (first forms) forms))))

(defun event (key)
  "`key' is a character, :space, :rubout, :enter, or (modifier key) with modifier one of
:shift, :ctrl and :alt, and key a character or one of :left, :right, :up, :down."
  (flet ((kind (key)
           (case key
             (:space #\space) (:rubout #\rubout) (:enter #\newline)
             (:left :left-arrow) (:right :right-arrow) (:up :up-arrow) (:down :down-arrow)
             (t key))))
    (if (consp key)
        (destructuring-bind (modifier key) key
          (ecase modifier
            (:shift (tui-sys:make-event :kind (kind key) :shiftp t))
            (:ctrl (tui-sys:make-event :kind (kind key) :controlp t))
            (:alt (tui-sys:make-event :kind (kind key) :altp t))))
        (tui-sys:make-event :kind (kind key)))))

(defun press (ui &rest keys)
  (dolist (key keys ui)
    (lines ui)
    (let ((focused-view nil))
      (tui:view-traverse (w::buffer-view ui)
       (lambda (view)
         (when (and (typep view 'w::ast-view)
                    (tui:focused view)
                    (w::location= (w::location view) (w::focus ui)))
           (setf focused-view view))
         t))
      (funcall (w::make-key-handler (w::node-at (w::focus ui)) (w::focus ui))
               focused-view (event key) ui))))

(defun focus-path (ui)
  (let ((path (reverse (mapcar #'w::location-id (w::stack ui)))))
    (if (= 1 (length (w::ast ui))) (rest path) path)))

(defun focused (ui)
  (w::node-at (w::focus ui)))

(defun history-length (ui)
  (length (w::history ui)))

(defun located (call &optional sort)
  (let ((nodes (list)))
    (parse::map-node
     call
     (lambda (child id spec)
       (parse::spec-traverse
        call child spec id
        (lambda (sub child-sort id)
          (declare (ignore id))
          (when (or (null sort) (eq sort child-sort))
            (push sub nodes))
          nil))))
    (nreverse nodes)))

(defun subform (call &rest path)
  (let ((id (cons 'parse:body path)))
    (when (eq 'parse:eval-form (parse:location-sort call id))
      (parse:node-at (w::make-location :node call :id id)))))

(defun subform-count (call)
  (length (located call 'parse:eval-form)))

(defun scope-at (call path)
  (parse:location-env call (cons 'parse:body path) (parse:make-env)))

(defun binds-at-p (call binder &rest path)
  (eq binder (parse:env-lookup binder :variable (scope-at call path))))

(defun scope-kinds (call &rest path)
  (let ((env (scope-at call path))
        (written (located call))
        (kinds (list)))
    (loop for (kind entries) in `((:variable ,(parse::variable-bindings env))
                                  (:function ,(parse::function-bindings env))
                                  (:block ,(parse::blocks env)))
          do (dolist (binder (intersection (mapcar #'ensure-car entries) written))
               (let ((cell (or (assoc (sx binder) kinds :test #'string=)
                               (car (push (list (sx binder)) kinds)))))
                 (pushnew kind (cdr cell)))))
    (sort (mapcar (lambda (cell)
                    (cons (car cell) (sort (cdr cell) #'string< :key #'symbol-name)))
                  kinds)
          #'string< :key #'car)))

(defun scope-names (call &rest path)
  (mapcar #'car (apply #'scope-kinds call path)))

(defmacro var-and-fn (name &body body)
  `(let* ((,name 1)) (flet ((,name () 2)) ,@body)))

(defmacro shadowing-vars (a b &body body)
  `(let* ((,a 1) (,b 2)) ,@body))

(defun select-completion (buffer name)
  (let* ((state (w::completion-state buffer))
         (index (position (string name) (w::candidates state)
                          :key #'symbol-name :test #'string=)))
    (assert index)
    (setf (w::completion-state buffer)
          (w::copy-completion-state state :selection index))))

(defun serialized-content= (expected actual)
  "Compares serialized syntax and anchors independently of formatting."
  (equal (weave-serialization::syntax-key (w::parse-source expected))
         (weave-serialization::syntax-key (w::parse-source actual))))
