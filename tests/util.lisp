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
  (parse:parse-from-string source))

;;; ui

(defun ui-for (source)
  (let* ((ast (if (stringp source) (parse source) source))
         (root (w::make-location :node 'undefined))
         (ui (make-instance 'w::ui :ast ast :stack (list root))))
    (setf (w::location-node root) ui
          (w::stack ui) (list root))
    ui))

(defun goto (ui &rest path)
  "Focuses `path' of ids from the root."
  (setf (w::stack ui) (last (w::stack ui)))
  (dolist (id path ui)
    (w::descend ui id)))

(defun lines (ui &key (rows 12) (cols 70))
  (let ((buffer (make-array (list rows cols))))
    (dotimes (i (array-total-size buffer))
      (setf (row-major-aref buffer i) (tui::make-cell)))
    (let ((tui::*put-buffer* buffer))
      (clrhash (w::node-views ui))
      (clrhash (w::redisplay-cache ui))
      (setf (w::focus-rect ui) nil)
      (w::render-node (w::ast ui) (last (w::stack ui)) ui
                      (tui:make-rect :x 0 :y 0 :rows rows :cols cols)))
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
  (sx (w::ast ui)))

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
  (reverse (mapcar #'w::location-id (butlast (w::stack ui)))))

(defun focused (ui)
  (w::node-at (w::focus ui)))

(defun history-length (ui)
  (length (w::history ui)))
