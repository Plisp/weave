;;;;
;;;; misc utilities, :used everywhere
;;;;

(uiop:define-package #:weave-utils
  (:use :cl #:alexandria-2)
  (:export #:disp #:addr-str
           #:list-insert #:list-update #:list-remove
           #:tree-ref #:tree-update
           #:with-lookup #:or-f #:+fail+
           #:string-drop #:split-string
           #:flex-vector
           #:external-symbol-p
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
  "Functional insertion, may share structure"
  `(,@(subseq list 0 i) ,item ,@(nthcdr i list)))

(defun list-update (list item i)
  "Functional replace, may share structure."
  `(,@(subseq list 0 i) ,item ,@(nthcdr (1+ i) list)))

(defun list-remove (list i)
  "Functional deletion, may share structure."
  `(,@(subseq list 0 i) ,@(nthcdr (1+ i) list)))

(defun array-copy (array)
  (let ((copy (make-array (array-dimensions array) :element-type (array-element-type array))))
    (dotimes (i (array-total-size array) copy)
      (setf (row-major-aref copy i) (row-major-aref array i)))))

(defun tree-ref (tree path)
  "Reads the position at `path' (a list of integer indices) within `tree' - an arbitrary
cons/array structure such as quoted data. An empty `path' returns `tree' itself."
  (if (null path)
      tree
      (etypecase tree
        (cons (tree-ref (nth (car path) tree) (cdr path)))
        (array (let ((rank (array-rank tree)))
                 (tree-ref (apply #'aref tree (subseq path 0 rank)) (nthcdr rank path)))))))

(defun tree-update (tree path new-value)
  "Functional replace, may share structure."
  (if (null path)
      new-value
      (etypecase tree
        (cons (list-update tree (tree-update (nth (car path) tree) (cdr path) new-value)
                           (car path)))
        (array (let* ((rank (array-rank tree))
                      (indices (subseq path 0 rank))
                      (copy (array-copy tree)))
                 (apply #'(setf aref)
                        (tree-update (apply #'aref tree indices) (nthcdr rank path) new-value)
                        copy indices)
                 copy)))))

(defmacro with-lookup ((name (&rest mvcall) &optional otherwise) &body then)
  (with-gensyms (blockname present-p)
    `(block ,blockname
       (multiple-value-bind (,name ,present-p)
           ,mvcall
         (when ,present-p
           (return-from ,blockname (progn ,@then))))
       ;; don't leak name to the else branch
       ,otherwise)))

(define-modify-macro or-f (&rest forms) or)

(define-constant +fail+ (list t) :test 'equal)

(defun string-drop (s n)
  (subseq s 0 (- (length s) n)))

(defun split-string (text c)
  (loop for start = 0 then (1+ end)
        for end = (position c text :start start)
        collect (subseq text start end)
        while end))

(defun flex-vector ()
  (make-array 0 :fill-pointer t :adjustable t))

(defun external-symbol-p (s p)
  (eq :external (nth-value 1 (find-symbol s p))))
