;;;;
;;;; misc utilities, :used everywhere
;;;;

(uiop:define-package #:weave-utils
  (:use :cl #:alexandria-2)
  (:export #:disp #:addr-str
           #:enumerate
           #:lfind #:list-insert #:findcdr-if
           #:with-lookup #:or-f #:+fail+
           #:flex-vector
           ))
(in-package #:weave-utils)

(defmacro disp (form &optional (stream t))
  (once-only ((res form))
    `(progn
       (format ,stream "~%~s~%|> ~s~%" ',form ,res)
       ,res)))

(defun addr-str (obj)
  (let* ((str (delete-if (lambda (c) (member c '(#\# #\< #\> #\Space #\{ #\})))
                         (with-output-to-string (s)
                           (print-unreadable-object (obj s :identity t)))))
         (length (length str)))
    (if (>= length 3)
        (subseq str (- length 3))
        str)))

(defun enumerate (f list)
  (loop for i from 0
        for x in list
        collect (funcall f x i)))

(defun lfind (item list &key (key 'identity) (test 'eql) (start 0) (end (length list)))
  "NIL-detecting version of find for lists, only searches forwards"
  (declare (optimize speed)
           (type fixnum start end)
           (type list list))
  (assert (<= 0 start end (length list)))
  (loop for elt in (nthcdr start list)
        do (when (funcall (the function test) item (funcall (the function key) elt))
             (return (values elt t)))))

(defun list-insert (list item i)
  "functional insertion, may share structure"
  `(,@(subseq list 0 i) ,item ,@(nthcdr i list)))

(defun findcdr-if (pred list)
  (loop for c on list
        do (when (funcall pred (car c))
             (return c))))

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

(defun flex-vector ()
  (make-array 0 :fill-pointer t :adjustable t))
