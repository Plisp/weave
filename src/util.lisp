;;;;
;;;; misc utilities, :used everywhere
;;;;

(uiop:define-package #:weave-utils
  (:use :cl #:alexandria-2)
  (:export #:disp #:addr-str
           #:list-insert #:list-update #:list-remove
           #:with-lookup #:or-f #:+fail+
           #:string-drop
           #:flex-vector
           ))
(in-package #:weave-utils)

(defmacro disp (form &optional (stream t))
  (once-only ((res form))
    `(progn
       (format ,stream "~%~s~%|> ~s~%" ',form ,res)
       ,res)))

(defun addr-str (obj &optional (len 3))
  (let* ((str (delete-if (lambda (c) (member c '(#\# #\< #\> #\Space #\{ #\})))
                         (with-output-to-string (s)
                           (print-unreadable-object (obj s :identity t)))))
         (length (length str)))
    (if (>= length len)
        (subseq str (- length len))
        str)))

(defun list-insert (list item i)
  "functional insertion, may share structure"
  `(,@(subseq list 0 i) ,item ,@(nthcdr i list)))

(defun list-update (list item i)
  "functional replace, may share structure"
  `(,@(subseq list 0 i) ,item ,@(nthcdr (1+ i) list)))

(defun list-remove (list i)
  `(,@(subseq list 0 i) ,@(nthcdr (1+ i) list)))

(defmacro with-lookup ((name (&rest mvcall) &optional default) &body then)
  (with-gensyms (blockname present-p)
    `(block ,blockname
       (multiple-value-bind (,name ,present-p)
           ,mvcall
         (when ,present-p
           (return-from ,blockname (progn ,@then))))
       ;; don't leak name to the else branch
       ,default)))

(define-modify-macro or-f (&rest forms) or)

(define-constant +fail+ (list t) :test 'equal)

(defun string-drop (s n)
  (subseq s 0 (- (length s) n)))

(defun flex-vector ()
  (make-array 0 :fill-pointer t :adjustable t))
