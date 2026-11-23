(in-package #:weave-tests)

(define-test matching)

(defun replace-at (node ids new)
  (if (rest ids)
      (parse:update node (first ids)
                    (replace-at (parse:get-location node (first ids)) (rest ids) new))
      (parse:update node (first ids) new)))

(defun holed (source &rest paths)
  "Parses `source' and puts a hole at each path, a list of ids descending through nodes."
  (let ((node (parse source)))
    (dolist (path paths node)
      (setf node (replace-at node path (parse:hole))))))

(defun matches (source query)
  (parse::node-match (parse source) (if (stringp query) (parse query) query) 'parse:eval-form))

(define-test identical-forms-match :parent matching
  (dolist (source '("(if a b)" "(block b 1)" "(let ((x 1)) x)" "(flet ((f (x) x)) (f 1))"
                    "(defun f (x) x)" "(defun f (x) \"doc\" x)" "(when a b)" "`(a ,b)"
                    "#'(lambda (x) x)" "(defmethod m :around ((x y)) y)" "(tagbody a (go a))"
                    "((lambda (x) x) 1)" "(f 1 2)" "'(a (b c))"))
    (is eq t (matches source source))))

(define-test differing-slots-do-not-match :parent matching
  (is eq nil (matches "(let ((x 1)) x)" "(let ((y 1)) x)"))
  (is eq nil (matches "(let ((x 1)) x)" "(let ((x 2)) x)"))
  (is eq nil (matches "(let (x) x)" "(let ((x 1)) x)"))
  (is eq nil (matches "(let ((x 1)) x)" "(let (y) x)"))
  (is eq nil (matches "(defun f (x &optional (y 1)) x)" "(defun f (x &optional (y 2)) x)"))
  (is eq nil (matches "(defun f (x) x)" "(defun f (x) \"doc\" x)"))
  (is eq nil (matches "`(a ,b)" "`(a ,c)")))

(define-test bodies-match-fuzzily :parent matching
  (is eq t (matches "(block b 1 2)" "(block b 2)"))
  (is eq nil (matches "(block b 1)" "(block b 1 1)"))
  (is eq nil (matches "(f 1)" "(f 1 1)")))

(define-test query-only-slots-must-be-present :parent matching
  (is eq nil (matches "(if a b)" "(if a b c)"))
  (is eq t (matches "(if a b c)" (holed "(if a b z)" '(parse:else)))))

(define-test holes-match-anything :parent matching
  (is eq t (matches "(let ((x 1)) x)" (holed "(let ((x z)) x)" '((parse:vars 0 1)))))
  (is eq t (matches "(let ((x 1) (y 2)) x)" (holed "(let ((z 1) (y 2)) x)" '((parse:vars 0 0)))))
  (is eq t (matches "(defun f (x) x)"
                    (holed "(defun f (z) x)" '(parse:fun-code (parse:lambda-list 0)))))
  (is eq t (matches "'(b c)" (holed "'z" '(parse:thing))))
  (is eq t (matches "'(a (b c))" (holed "'(a z)" '((parse:thing 1))))))

(define-test macro-bodies-match-as-syntax :parent matching
  (is eq t (matches "(when a (f 1))" (holed "(when a z)" '((parse:body 1)))))
  (is eq t (matches "(cond ((null x) 1) (t 2))" (holed "(cond ((null x) z))" '((parse:body 0 1))))))

(define-test quoted-lists-match-in-order :parent matching
  (is eq nil (matches "'(b c)" "'(b c b)")))

(define-test comments-are-ignored-when-matching :parent matching
  (is eq t (matches (format nil "(block b ; c~% 1)") "(block b 1)")))

(define-test lambda-list-queries-match-prefixes :parent matching
  (is eq t (matches "(defun f (x y) x)" "(defun f (x) x)"))
  (is eq nil (matches "(defun f (x) x)" "(defun f (x y) x)")))
