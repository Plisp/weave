;;;;
;;;; misc utilities, :used everywhere
;;;;

(defpackage #:weave-utils
  (:use :cl #:alexandria-2)
  (:export #:disp #:addr-str #:lfind #:with-lookup #:or-f #:+fail+))
(in-package #:weave-utils)

(defmacro disp (form)
  (once-only ((res form))
    `(progn
       (format t "~%~s~%|> ~s~%" ',form ,res)
       ,res)))

(defun addr-str (obj)
  (let* ((str (delete-if (lambda (c) (member c '(#\# #\< #\> #\Space #\{ #\})))
                         (with-output-to-string (s)
                           (print-unreadable-object (obj s :identity t)))))
         (length (length str)))
    (if (>= length 3)
        (subseq str (- length 3))
        str)))

(defun lfind (item list &key (key 'identity) (test 'eql) (start 0) (end (length list)))
  "NIL-detecting version of find for lists, only searches forwards"
  (declare (optimize speed)
           (type fixnum start end)
           (type list list))
  (assert (<= 0 start end (length list)))
  (loop for elt in (nthcdr start list)
        do (when (funcall (the function test) item (funcall (the function key) elt))
             (return (values elt t)))))

(defmacro with-lookup ((name (&rest mvcall) &optional default) &body then)
  (with-gensyms (blockname present-p)
    `(block ,blockname
       (multiple-value-bind (,name ,present-p)
           ,mvcall
         (when ,present-p
           (return-from ,blockname (progn ,@then))))
       ,default)))

(define-modify-macro or-f (&rest forms) or)

(define-constant +fail+ (list t) :test 'equal)
