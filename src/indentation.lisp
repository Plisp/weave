(defpackage #:weave-indentation
  (:use #:cl #:alexandria-2 #:weave-utils #:weave-parser)
  (:export #:loop-indentation
           #:find-loop-keyword
           #:operator-section-start
           #:indent-rule
           #:*indent-rules*))

(in-package #:weave-indentation)

(defparameter *loop-clause-keywords*
  '("NAMED" "WITH" "FOR" "AS" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS" "COLLECT"
    "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING" "COUNT" "COUNTING"
    "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "DO" "DOING" "RETURN" "INITIALLY"
    "FINALLY" "WHEN" "IF" "UNLESS" "ELSE" "END" "AND"))

(defparameter *loop-operand-keywords*
  '("WITH" "FOR" "AS" "FROM" "UPFROM" "DOWNFROM" "TO" "UPTO" "DOWNTO" "BELOW" "ABOVE" "IN"
    "ON" "ACROSS" "=" "THEN" "BY" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS"
    "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING" "COUNT"
    "COUNTING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING" "WHEN" "IF" "UNLESS" "DO"
    "DOING" "RETURN" "INITIALLY" "FINALLY" "INTO" "NAMED"))

(defparameter *loop-prefix-keywords* '("AND" "ELSE"))

(defparameter *loop-body-keywords* '("DO" "DOING" "INITIALLY" "FINALLY"))

(defun find-loop-keyword (element)
  (and (typep element 'symbol-ref)
       (find (name element) *loop-clause-keywords* :test #'string=)))

(defun loop-indentation
  (elements
   &optional (width (lambda (node)
                      (length (name node)))))
  "Indent spec for a loop form. The operator has its own line, then a line indented by one
for every clause keyword, except those following and/else. Forms of a do-like clause after
its first are aligned with the first, and a loop without keywords has a line per form."
  (let* ((indent 1)
         (rows (list (cons 0 1)))
         (column indent)
         (body-column nil)
         (body-started nil)
         (expecting-operand nil)
         (previous (first elements)))
    (loop for element in (rest elements)
          for first-element = t then nil
          for keyword = (unless expecting-operand
                          (find-loop-keyword element))
          do (cond
               (keyword
                (if (member (find-loop-keyword previous) *loop-prefix-keywords*
                            :test #'equal)
                    (progn
                      (incf (cdar rows))
                      (incf column (1+ (funcall width previous))))
                    (progn
                      (push (cons indent 1) rows)
                      (setf column indent)))
                (setf body-column
                      (when (member keyword *loop-body-keywords* :test #'string=)
                        (+ column (funcall width element) 1))
                      body-started nil))
               (first-element
                (push (cons indent 1) rows)
                (setf body-column indent body-started t))
               ((and body-column body-started) (push (cons body-column 1) rows))
               (t
                (incf (cdar rows))
                (setf body-started t)))
             (setf expecting-operand
                   (and (not expecting-operand)
                        (typep element 'symbol-ref)
                        (member (name element) *loop-operand-keywords* :test #'string=))
                   previous element))
    (nreverse rows)))

(defun operator-section-start (symbol keywords)
  "Counts positional args before `keywords' in `symbol's global lambda list."
  (when (and symbol (fboundp symbol))
    (if (and (member '&key keywords) (get symbol 'weave-utils::record-constructor))
        0
        (lambda-list-section-start (slynk-backend:arglist symbol) keywords))))

;; conventional roles informed by cl-indent and sly-cl-indent
;; https://github.com/joaotavora/sly/blob/master/lib/sly-cl-indent.el
;; Line breaks are chosen independently of the rules.
;; :header counts operands before a body, :children controls positional indents,
;; and :rest supplies the role for remaining operands (or all list elements)
(defparameter *indent-rules*
  '((defpackage :header 1 :rest (:break :linear))
    (uiop/package:define-package :like defpackage)
    (defun :header 2 :children (nil (:style :lambda))) (defmacro :like defun)
    (deftype :like defun) (defgeneric :like defun) (define-setf-expander :like defun)
    (lambda :header 1 :children ((:style :lambda))) (defmethod :method t)
    (uncursed:place :header 1 :children ((:style :list)))
    (let :header 1 :children ((:style :list :break :mandatory :rest (:header 1))))
    (if-let :header 1 :children ((:style :list))) (when-let :like if-let)
    (when-let* :like let)
    (with-accessors :header 2 :argument-break 2 :children
     ((:style :list :break :mandatory :rest (:style :list)) nil))
    (with-slots :header 2 :argument-break 2 :children (:data nil)) (let* :like let)
    (symbol-macrolet :like let)
    (flet :header 1 :children
     ((:style :list :break :mandatory :rest (:header 1 :children ((:style :lambda))))))
    (labels :like flet) (macrolet :like flet)
    (destructuring-bind :header 2 :argument-break 2 :children (:data nil))
    (multiple-value-bind :header 2 :argument-break 2 :children (:data nil))
    (multiple-value-setq :argument-break 2 :children (:data nil))
    (progv :header 2 :argument-break 2) (when :header 1) (unless :header 1)
    (block :header 1) (catch :header 1) (progn :header 0) (locally :header 0)
    (eval-when :header 1 :children ((:style :lambda))) (unwind-protect :header 1)
    (dolist :header 1 :children ((:style :list))) (dotimes :like dolist)
    (do :header 2 :children
     ((:style :list :break :mandatory :rest (:style :list)) (:style :list)))
    (do* :like do) (cond :header 0 :rest (:header 0 :compact-body t))
    (case :header 1 :rest (:header 0 :head :data :compact-body t)) (ecase :like case)
    (ccase :like case) (typecase :like case) (etypecase :like case) (ctypecase :like case)
    (handler-case :header 1 :rest (:header 1 :children ((:style :lambda))))
    (restart-case :like handler-case)
    (handler-bind :header 1 :children
     ((:style :list :break :mandatory :rest (:style :list))))
    (restart-bind :like handler-bind)
    (trivia:match :header 1 :rest (:header 0 :head :data :compact-body t))
    (trivia:ematch :like trivia:match) (trivia:cmatch :like trivia:match)
    (if :break :linear) (and :break :linear) (or :break :linear) (setq :pairs t)
    (setf :pairs t) (psetq :pairs t) (psetf :pairs t)
    (defvar :header 2 :body-break :linear) (defparameter :like defvar)
    (defconstant :like defvar)
    (defclass :header 2 :children
     (nil :data (:style :list :break :mandatory :rest (:style :list))))
    (define-condition :like defclass)
    (defrecord :header 2 :children
     (nil :data (:style :list :break :mandatory :rest (:style :list))))
    (defpackage :header 1 :rest :data)))

(defun indent-rule (elements)
  (let ((head (first elements)))
    (when (typep head 'symbol-ref)
      (labels ((lookup (symbol)
                 (let ((rule (cdr (assoc symbol *indent-rules*))))
                   (if (getf rule :like) (lookup (getf rule :like)) rule))))
        (let* ((symbol (resolve head))
               (count (when (and symbol (not (eq symbol 'loop)) (macro-function symbol))
                        (operator-section-start symbol '(&body &rest))))
               (rule (or (lookup symbol)
                         (when count
                           (list :header count)))))
          (if (getf rule :method)
              (let ((position (position-if
                                (lambda (node)
                                  (typep node 'ref-list))
                               elements
                               :start (min 2 (length elements)))))
                (when position
                  (list :header position :children
                        (append (make-list (1- position)) (list '(:style :lambda))))))
              rule))))))

(defun child-rule (rule index)
  "Selects the nested rule for element `index', including the head."
  (cond
    ((eq rule :data) :data)
    (t
     (let ((operand (if (member (getf rule :style) '(:list :lambda)) index (1- index))))
       (cond
         ((minusp operand) (getf rule :head))
         ((< operand (length (getf rule :children))) (nth operand (getf rule :children)))
         ((and (getf rule :header) (< operand (getf rule :header))) nil)
         (t (getf rule :rest)))))))
