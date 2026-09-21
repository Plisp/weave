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
         (root (w::make-location :node 'undefined))
         (ui (make-instance 'w::ui :ast ast :stack (list root))))
    (setf (w::location-node root) ui
          (w::stack ui) (list root))
    (w::descend ui 0)
    ui))

(defun goto (ui &rest path)
  "Focuses `path' from the first root"
  (setf (w::stack ui) (last (w::stack ui)))
  (when (= 1 (length (w::ast ui)))
    (w::descend ui 0))
  (dolist (id path ui)
    (w::descend ui id)))

(defun render-buffer (ui &key (rows 12) (cols 70))
  (let ((buffer (make-array (list rows cols))))
    (dotimes (i (array-total-size buffer))
      (setf (row-major-aref buffer i) (tui::make-cell)))
    (setf (slot-value ui 'tui::%canvas) buffer)
    (let ((tui::*put-buffer* buffer))
      (setf (slot-value ui 'tui::%root-view) (tui:render ui)))
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
    (funcall (w::global-key-handler (w::node-at (w::focus ui)) (w::focus ui) ui)
             nil (event key))))

(defun focus-path (ui)
  (let ((path (reverse (mapcar #'w::location-id (butlast (w::stack ui))))))
    (if (= 1 (length (w::ast ui))) (rest path) path)))

(defun focused (ui)
  (w::node-at (w::focus ui)))

(defun history-length (ui)
  (length (w::history ui)))

(defun binder-names (entry)
  "Sorted names of the binders in a `subforms' entry."
  (let ((names (list)))
    (when (cdr entry)
      (maphash (lambda (binder kind) (declare (ignore kind)) (push (parse:name binder) names))
               (cdr entry)))
    (sort names #'string<)))

(defun binder-kinds (entry)
  "Sorted (name . kinds) of the binders in a `subforms' entry."
  (let ((kinds (list)))
    (when (cdr entry)
      (maphash (lambda (binder ks)
                 (push (cons (parse:name binder) (sort (copy-list ks) #'string< :key #'symbol-name))
                       kinds))
               (cdr entry)))
    (sort kinds #'string< :key #'car)))

(defmacro var-and-fn (name &body body)
  `(let* ((,name 1)) (flet ((,name () 2)) ,@body)))

(defmacro shadowing-vars (a b &body body)
  `(let* ((,a 1) (,b 2)) ,@body))
