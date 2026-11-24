(in-package #:weave-tests)

(define-test rename)

(defun renamed (source path new)
  "The rendering of `source' after renaming the binder at `path', a list of ids, to the
symbol named `new'."
  (sx (parse::rename-binder (parse source) path new)))

(defun rendered (source)
  (sx (parse source)))

(defmacro refuses-rename (source path new)
  `(fail (parse::rename-binder (parse ,source) ',path ,new)
       'parse::rename-error))

(defun path-of (form node)
  "The id path from `form' to `node' by identity, or :NOT-IN-FORM."
  (let ((paths nil))
    (parse::node-search form node nil
                        (lambda (occupant query sort)
                          (declare (ignore sort))
                          (eq occupant query))
                        (lambda (stack) (push (reverse stack) paths)))
    (if paths (first paths) :not-in-form)))

(defun refusal (source path new)
  "The reason for refusing the rename and the path to the blamed node in the parsed source,
or NIL if the rename succeeds."
  (let ((form (parse source)))
    (handler-case (progn (parse::rename-binder form path new) nil)
      (parse::rename-error (e)
        (list (parse::rename-error-reason e) (path-of form (parse::rename-error-node e)))))))

(define-test rename-let-variables :parent rename
  (is equal (rendered "(let ((n 1)) (+ n n))")
      (renamed "(let ((x 1)) (+ x x))" '((parse:vars 0 0)) "N"))
  (is equal (rendered "(let* ((n 1) (y n)) y)")
      (renamed "(let* ((x 1) (y x)) y)" '((parse:vars 0 0)) "N")))

(define-test rename-leaves-shadowing-bindings-alone :parent rename
  (is equal (rendered "(let ((n 1)) (list n (let ((x 2)) x)))")
      (renamed "(let ((x 1)) (list x (let ((x 2)) x)))" '((parse:vars 0 0)) "N"))
  (is equal (rendered "(let ((n 1)) (let ((x 2) (y n)) y))")
      (renamed "(let ((x 1)) (let ((x 2) (y x)) y))" '((parse:vars 0 0)) "N")))

(define-test rename-may-shadow-unreferenced-outer-bindings :parent rename
  (is equal (rendered "(let ((n 1)) (let ((n 2)) n))")
      (renamed "(let ((n 1)) (let ((x 2)) x))" '((parse:body 0) (parse:vars 0 0)) "N")))

(define-test rename-leaves-quoted-occurrences-alone :parent rename
  (is equal (rendered "(let ((n 1)) (list n 'x))")
      (renamed "(let ((x 1)) (list x 'x))" '((parse:vars 0 0)) "N")))

(define-test rename-local-functions :parent rename
  (is equal (rendered "(flet ((n (a) a)) (n (n 1)))")
      (renamed "(flet ((f (a) a)) (f (f 1)))" '((parse:funs 0 0)) "N"))
  (is equal (rendered "(flet ((n () (f))) (n))")
      (renamed "(flet ((f () (f))) (f))" '((parse:funs 0 0)) "N"))
  (is equal (rendered "(labels ((n () (n))) (n))")
      (renamed "(labels ((f () (f))) (f))" '((parse:funs 0 0)) "N"))
  (is equal (rendered "(flet ((n () 1)) #'n)")
      (renamed "(flet ((f () 1)) #'f)" '((parse:funs 0 0)) "N")))

(define-test rename-blocks :parent rename
  (is equal (rendered "(block n (return-from n 1))")
      (renamed "(block b (return-from b 1))" '(parse:name) "N")))

(define-test rename-lambda-list-parameters :parent rename
  (is equal (rendered "(lambda (n) (* n 2))")
      (renamed "(lambda (x) (* x 2))" '(parse:fun-code (parse:lambda-list 0)) "N"))
  (is equal (rendered "(lambda (n &optional (y n)) y)")
      (renamed "(lambda (x &optional (y x)) y)" '(parse:fun-code (parse:lambda-list 0)) "N")))

(define-test rename-through-macro-calls :parent rename
  (is equal (rendered "(let ((n 1)) (when n (print n)))")
      (renamed "(let ((x 1)) (when x (print x)))" '((parse:vars 0 0)) "N"))
  (is equal (rendered "(dolist (n xs) (print n))")
      (renamed "(dolist (x xs) (print x))" '((parse:body 0 0)) "N")))

(define-test rename-refuses-capture-by-inner-bindings :parent rename
  (is equal '(:capture ((parse:body 0) (parse:body 0)))
      (refusal "(let ((x 1)) (let ((n 2)) x))" '((parse:vars 0 0)) "N")))

(define-test rename-refuses-capturing-references :parent rename
  (is equal '(:capture ((parse:body 0) (parse:body 0) (parse:body 1)))
      (refusal "(let ((n 1)) (let ((x 2)) (+ x n)))" '((parse:body 0) (parse:vars 0 0)) "N"))
  (is equal '(:capture ((parse:body 0) (parse:body 1)))
      (refusal "(let ((x 1)) (+ x y))" '((parse:vars 0 0)) "Y"))
  (is equal '(:capture ((parse:body 0) parse:op))
      (refusal "(flet ((f () 1)) (list (f)))" '((parse:funs 0 0)) "LIST")))

(define-test rename-refuses-special-binders :parent rename
  (is equal '(:special ((parse:vars 0 0)))
      (refusal "(let ((x 1)) (declare (special x)) x)" '((parse:vars 0 0)) "N"))
  (is equal '(:special ((parse:vars 0 0)))
      (refusal "(let ((*print-case* :upcase)) (print 1))" '((parse:vars 0 0)) "N"))
  (is equal '(:special ((parse:vars 0 0)))
      (refusal "(let ((*print-case* :upcase)) (print 1))" '((parse:vars 0 0)) "*PRINT-BASE*"))
  (is equal '(:special (parse:fun-code (parse:lambda-list 0)))
      (refusal "(lambda (x) (declare (special x)) x)" '(parse:fun-code (parse:lambda-list 0)) "N")))

(define-test rename-to-global-specials-may-escape :parent rename
  (is equal (rendered "(let ((*print-base* 1)) (lambda () *print-base*))")
      (renamed "(let ((x 1)) (lambda () x))" '((parse:vars 0 0)) "*PRINT-BASE*"))
  (is equal (rendered "(let ((*print-base* 1)) (flet ((f () *print-base*)) (f)))")
      (renamed "(let ((x 1)) (flet ((f () x)) (f)))" '((parse:vars 0 0)) "*PRINT-BASE*")))

(define-test rename-refuses-local-specials-captured-by-closures :parent rename
  (is equal '(:escape ((parse:body 0) parse:fun-code (parse:body 0)))
      (refusal "(let ((x 1)) (declare (special n)) (lambda () x))" '((parse:vars 0 0)) "N")))

(define-test rename-leaves-free-special-declarations-alone :parent rename
  (is equal (rendered "(let ((n 1)) (list n (locally (declare (special x)) x)))")
      (renamed "(let ((x 1)) (list x (locally (declare (special x)) x)))" '((parse:vars 0 0)) "N")))

(define-test rename-introduces-dynamic-bindings :parent rename
  (is equal (rendered "(let ((*print-base* 10)) *print-base*)")
      (renamed "(let ((x 10)) x)" '((parse:vars 0 0)) "*PRINT-BASE*"))
  (is equal (rendered "(let ((*print-base* 10)) (print *print-base* *print-base*))")
      (renamed "(let ((x 10)) (print x *print-base*))" '((parse:vars 0 0)) "*PRINT-BASE*")))

(define-test rename-refuses-duplicate-binders :parent rename
  (is equal '(:conflict ((parse:vars 1 0)))
      (refusal "(let ((x 1) (n 2)) x)" '((parse:vars 0 0)) "N"))
  (is equal '(:conflict ((parse:vars 0 0)))
      (refusal "(let ((n 2) (x 1)) x)" '((parse:vars 1 0)) "N"))
  (is equal '(:conflict (parse:fun-code (parse:lambda-list 1)))
      (refusal "(lambda (x n) x)" '(parse:fun-code (parse:lambda-list 0)) "N"))
  (is equal '(:conflict (parse:fun-code (parse:lambda-list 0)))
      (refusal "(lambda (n x) x)" '(parse:fun-code (parse:lambda-list 1)) "N"))
  (is equal '(:conflict ((parse:funs 1 0)))
      (refusal "(flet ((f () 1) (n () 2)) (f))" '((parse:funs 0 0)) "N"))
  (is equal '(:conflict ((parse:funs 0 0)))
      (refusal "(flet ((n () 2) (f () 1)) (f))" '((parse:funs 1 0)) "N")))

(define-test rename-substitutes-declarations :parent rename
  (is equal (rendered "(let ((n 1)) (declare (ignorable n)) n)")
      (renamed "(let ((x 1)) (declare (ignorable x)) x)" '((parse:vars 0 0)) "N"))
  (is equal (rendered "(let ((n 1)) (let ((y 2)) (declare (type fixnum n)) (+ n y)))")
      (renamed "(let ((x 1)) (let ((y 2)) (declare (type fixnum x)) (+ x y)))"
               '((parse:vars 0 0)) "N"))
  (is equal (rendered "(let ((n 1)) (list n (let ((x 2)) (declare (ignorable x)) x)))")
      (renamed "(let ((x 1)) (list x (let ((x 2)) (declare (ignorable x)) x)))"
               '((parse:vars 0 0)) "N"))
  (is equal (rendered "(lambda (n) (declare (fixnum n)) n)")
      (renamed "(lambda (x) (declare (fixnum x)) x)" '(parse:fun-code (parse:lambda-list 0)) "N"))
  (is equal (rendered "(flet ((n () 1)) (declare (inline n)) (n))")
      (renamed "(flet ((f () 1)) (declare (inline f)) (f))" '((parse:funs 0 0)) "N")))

#+(or)
(define-test rename-refuses-unexpanded-macro-mentions :parent rename
  (refuses-rename "(let ((x 1)) (loop for i from 1 to x (print x)))" ((parse:vars 0 0)) "N"))

(define-test rename-refuses-escaping-macro-binders :parent rename
  (refuses-rename "(with-slots (a) obj a)" ((parse:body 0 0)) "N"))

(define-test rename-to-a-name-not-yet-interned :parent rename
  (let* ((name (loop for candidate = (format nil "FRESH-~d" (random 1000000000))
                     unless (find-symbol candidate '#:weave-tests)
                       return candidate))
         (result (parse::rename-binder (parse "(let ((x 1)) (+ x x))") '((parse:vars 0 0)) name))
         (binder (first (parse:elements (first (parse:elements (parse:vars result))))))
         (references (parse:body (first (parse:body result)))))
    (is equal (format nil "(LET ((~a 1)) (+ ~a ~a))" name name name) (sx result))
    (is eq (find-package '#:weave-tests) (parse:home-package binder))
    (dolist (reference references)
      (is eq (parse:home-package binder) (parse:home-package reference)))))
