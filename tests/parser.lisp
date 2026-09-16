(in-package #:weave-tests)

(define-test parser)

;;; parsing and macro analysis

(define-test holes-parse-as-atoms :parent parser
  (is equal 'parse:function-call
            (type-of (parse:parse-syntax (parse:ref-list (sym "LIST") (parse:hole))))))

(define-test unparseable-macro-argument-is-left-raw :parent parser
  (let ((ast (parse "(loop for x in (1 2) collect x)")))
    (is equal 'parse:macro-call (type-of ast))
    (is equal nil (nth-value 1 (gethash (parse:gen-tree-ref (parse:body ast) '(3))
                                        (parse:subforms ast))))
    (is equal nil (nth-value 1 (gethash nil (parse:subforms ast))))
    (is equal nil (parse:location-sort ast '(parse:body 9))))
  (fail (parse "(print (1 2))")))

(define-test subforms-hold-parsed-forms-and-binders :parent parser
  (let* ((ast (parse "(loop for x in xs do (print x))"))
         (entry (gethash (parse:gen-tree-ref (parse:body ast) '(5)) (parse:subforms ast))))
    (is equal 'parse:function-call (type-of (car entry)))
    (is equal '("X") (mapcar #'parse:name (cdr entry)))))

(define-test hole-argument-keeps-analysis :parent parser
  (let* ((ast (parse "(loop for x in (xs a) do (print x))"))
         (edited (parse:update ast '(parse:body 3 1) (parse:hole))))
    (is equal (hash-table-count (parse:subforms ast))
              (hash-table-count (parse:subforms edited)))))

(define-test hole-binder-is-skipped :parent parser
  (let ((edited (parse:update (parse "(loop for x in xs for y in ys do (print y))")
                              '(parse:body 1) (parse:hole))))
    (is equal '("Y")
              (mapcar #'parse:name
                      (cdr (gethash (parse:gen-tree-ref (parse:body edited) '(9))
                                    (parse:subforms edited)))))))

(define-test macro-argument-edits-survive-copying :parent parser
  (let* ((call (parse "(dolist (x xs) wr)"))
         (edited (parse:update call '(parse:body 1) (sym "WRI")))
         (copy (parse:copy-node edited)))
    (is equal "WRI" (parse:name (car (gethash (nth 1 (parse:body copy)) (parse:subforms copy)))))))

(define-test generated-forms-sort-op :parent parser
  (dolist (source '("(let ((a 1)) a)" "(quote x)" "(if a b c)" "(progn a)" "(function car)"
                    "(when a b)"))
    (is equal 'parse:symbol-ref (parse:location-sort (parse source) 'parse:op))))

(define-test function-designators-sort :parent parser
  (is equal 'parse:fun-designator (parse:location-sort (parse "(function car)") 'parse:fun-designator))
  (is equal 'parse:unevaluated
            (parse:location-sort (parse "(function (lambda (x) x))") 'parse:fun-designator)))

;;; suffix path reachability

(defun syntax-paths (tree &optional prefix)
  (when (parse:gen-list-p tree)
    (loop for element in (parse:elements tree)
          for i from 0
          for path = (append prefix (list i))
          collect path
          append (syntax-paths element path))))

(defun follow (node path)
  (reduce (lambda (value id) (w::node-at (w::make-location :node value :id id)))
          path :initial-value node))

(define-test suffix-paths-reach-their-syntax :parent parser
  (dolist (source '("(let ((a 1)) (f a))" "(lambda (x) (g x))"
                    "(when (f x) (g (h y)))" "(loop for i from 1 to 10 do (print (f i)))"))
    (let* ((ast (parse source))
           (syntax (parse:to-syntax ast)))
      (dolist (path (syntax-paths syntax))
        (is equal (list source path (sx (parse:gen-tree-ref syntax path)))
                  (list source path (sx (follow ast (parse::suffix-path ast path)))))))))

;;; helpers

(define-test value-at-path-reads-presented-forms :parent parser
  (let* ((call (parse "(dolist (x (f xs)) (print x))"))
         (inner (car (gethash (parse:gen-tree-ref (parse:body call) '(0 1)) (parse:subforms call))))
         (edited (parse:update call '(parse:body 0 1) (parse:update inner 'parse:name (sym "G"))))
         (list (w::make-location :node edited :id '(parse:body 0))))
    (is equal "(G XS)" (sx (w::value-at-path list '(1))))
    (is equal "G" (sx (w::value-at-path list '(1 0))))
    (is equal nil (w::value-at-path list '(5)))
    (is equal nil (w::value-at-path list '(0 0)))
    (fail (w::value-at-path (w::make-location :node (parse "(let ((a 1)) a)") :id 'parse:op)
                            '(0)))))

(define-test default-list-contents :parent parser
  (flet ((emptied (source id)
           (sx (w::empty-list-placeholder (w::make-location :node (parse source) :id id)))))
    (is equal "()" (emptied "'(a b)" 'parse::thing))
    (is equal "((_ _))" (emptied "(let ((a 1)) a)" 'parse:vars))
    (is equal "(_)" (emptied "(progn a)" 'parse::forms))
    (fail (emptied "(let ((a 1)) a)" 'parse:op))))

(define-test loop-indentation :parent parser
  (flet ((spec (source)
           (let ((call (parse source)))
             (w::loop-indentation (cons (parse:op call) (parse:body call))))))
    (is equal '((0 . 1) (1 . 1) (1 . 1) (1 . 1)) (spec "(loop (foo) (bar) (quux))"))
    (is equal '((0 . 1) (1 . 4) (1 . 2) (4 . 1) (1 . 2))
              (spec "(loop for x in xs do (print x) (print y) collect x)"))
    (is equal '((0 . 1) (1 . 2) (1 . 2) (1 . 3) (9 . 1)) (spec "(loop when a do (f) else do (g) (h))"))
    (is equal '((0 . 1)) (spec "(loop)"))))

;;; macro analysis

(define-test read-conditionals-in-macro-arguments :parent parser
  (is equal 'parse:macro-call (type-of (parse "(when t #+sbcl (foo) (bar x))"))))

(define-test local-macros-expand :parent parser
  (let* ((ast (parse "(macrolet ((m (x) x)) (m (print y)))"))
         (call (first (parse:body ast))))
    (is equal 'parse:macro-call (type-of call))
    (is equal 'parse:function-call
              (type-of (car (gethash (first (parse:body call)) (parse:subforms call)))))))

(define-test invariants-escape-error-handlers :parent parser
  (fail (parse::invariant nil) 'parse::analysis-invariant-error)
  (is eq :escaped
         (handler-case (handler-case (parse::invariant nil)
                         ((and error (not parse::analysis-invariant-error)) () :caught))
           (parse::analysis-invariant-error () :escaped))))

(define-test copied-macro-calls-own-their-binders :parent parser
  (let* ((call (parse "(dotimes (i 10) (print i))"))
         (copy (parse:copy-node call))
         (binder (cdr (first (parse:location-bindings copy '(parse:body 1))))))
    (is eq (parse:gen-tree-ref (parse:body copy) '(0 0)) binder)
    (is eq nil (eq binder (parse:gen-tree-ref (parse:body call) '(0 0))))))

(define-test hole-binder-binds-nothing :parent parser
  (let ((edited (parse:update (parse "(dolist (x xs) (print x))") '(parse:body 0 0) (parse:hole))))
    (is equal nil (cdr (gethash (nth 1 (parse:body edited)) (parse:subforms edited))))))

(define-test unparseable-calls-drop-binders :parent parser
  (let* ((call (parse:update (parse "(dolist (x xs) (print x))") 'parse:op (parse:hole)))
         (entry (gethash (nth 1 (parse:body call)) (parse:subforms call))))
    (is equal 'parse:function-call (type-of (car entry)))
    (is equal nil (cdr entry))
    (is equal nil (parse:location-bindings call '(parse:body 1)))))

(define-test unparseable-calls-keep-parsed-arguments :parent parser
  (let* ((call (parse:update (parse "(dolist (x (f xs)) (print x))") 'parse:op (parse:hole)))
         (renamed (parse:update call '(parse:body 0 0) (sym "Y"))))
    (is equal 'parse:function-call
              (type-of (car (gethash (parse:gen-tree-ref (parse:body renamed) '(0 1))
                                     (parse:subforms renamed)))))
    (is equal "(_ (Y (F XS)) (PRINT X))" (sx renamed))))

(define-test failed-expansions-keep-parsed-arguments :parent parser
  (let* ((call (parse "(loop for i from 1 to 10 do (print i))"))
         (body (parse:body call))
         (print-form (nth 7 body))
         (deleted (parse:update call 'parse:body (append (subseq body 0 6) (subseq body 7)))))
    (is eq print-form (nth 6 (parse:body deleted)))
    (is equal 'parse:function-call (type-of (car (gethash print-form (parse:subforms deleted)))))
    (is equal nil (parse::expanded deleted))
    (let ((restored (parse:update deleted 'parse:body body)))
      (is equal t (parse::expanded restored))
      (is equal '("I") (mapcar #'parse:name
                               (cdr (gethash (nth 7 (parse:body restored))
                                             (parse:subforms restored)))))))
  (is equal nil (parse::expanded (parse "(loop for i from 1 to 10 (print i))"))))
