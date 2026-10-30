(in-package #:weave-tests)

(define-test editing)

;;; rendering

(define-test layouts-render :parent editing
  (is equal "let (a 1 /      b 2) /  f a" (draw (ui-for "(let ((a 1) (b 2)) (f a))")))
  (is equal "let () /  a" (draw (ui-for "(let () a)")))
  (is equal "flet (f (x) /         x) /  f 1" (draw (ui-for "(flet ((f (x) x)) (f 1))")))
  (is equal "symbol-macrolet (s car y) /  s" (draw (ui-for "(symbol-macrolet ((s (car y))) s)")))
  (is equal "'(a (b c) d)" (draw (ui-for "'(a (b c) d)")))
  (is equal "list () x" (draw (ui-for "(list () x)")))
  (is equal "progn /  ()" (draw (ui-for "(progn ())"))))

(define-test quasiquote-rendering :parent editing
  (dolist (source '("`a" "`()" "`((a b) c)" "`(1 2)"
                    "``(a ,x ,,x ,@(list y))"
                    "'(quasiquote unquote unquote-splicing)"))
    (is equal source (draw (ui-for source))))
  (is equal '("`(when ,x" "   ,y)")
      (lines (ui-for "`(when ,x ,y)")))
  (is equal '("`(loop" "   for x in ,xs" "   collect x)")
      (lines (ui-for "`(loop for x in ,xs collect x)")))
  (let ((ui (ui-for "`(a ,(+ x 1) ,@(list y z))")))
    (is equal '("`(a ,x + 1 ,@list y z)") (lines ui))
    (is equal (lines ui) (lines ui))))

(define-test quasiquote-evaluated-editing :parent editing
  (let ((ui (ui-for "`(a ,(+ x 1) ,@(list y z))")))
    (goto ui '(parse:thing 1 1))
    (is eq 'parse:eval-form (w::locsort (w::focus ui)))
    (press ui :rubout)
    (is equal "(QUASIQUOTE (A (UNQUOTE _) (UNQUOTE-SPLICING (LIST Y Z))))"
        (code ui))
    (press ui #\3)
    (is equal "(QUASIQUOTE (A (UNQUOTE 3) (UNQUOTE-SPLICING (LIST Y Z))))"
        (code ui))
    (goto ui '(parse:thing 2 1))
    (is eq 'parse:eval-form (w::locsort (w::focus ui)))
    (press ui :rubout #\()
    (is equal "(QUASIQUOTE (A (UNQUOTE 3) (UNQUOTE-SPLICING ())))" (code ui))
    (is equal "`(a ,3 ,@())" (draw ui)))
  (let ((ui (ui-for "``(a ,x ,,x)")))
    (goto ui '(parse:thing 1 1 1))
    (is eq 'parse:unevaluated (w::locsort (w::focus ui)))
    (goto ui '(parse:thing 1 2 1 1))
    (is eq 'parse:eval-form (w::locsort (w::focus ui)))
    (press ui :rubout #\7)
    (is equal "(QUASIQUOTE (QUASIQUOTE (A (UNQUOTE X) (UNQUOTE (UNQUOTE 7)))))"
        (code ui))
    (is equal "``(a ,x ,,7)" (draw ui))))

(define-test function-documentation-renders :parent editing
  (let* ((ui (ui-for "(defun g (x) \"first
second\" (declare (type fixnum x)) (+ x 1))"))
         (code (parse:fun-code (first (w::ast ui)))))
    (is = 1 (length (parse:body code)))
    (is equal "\"first
second\"" (parse:str (parse::docstring code)))
    (is equal "defun g /  (x) /  \"first /  second\" /  (declare (type fixnum x)) /  x + 1" (draw ui)))
  (let ((code (make-instance 'parse:function-code
                             :lambda-list-kind :lambda
                             :lambda-list (parse:ref-list)
                             :docstring "raw documentation"
                             :body nil)))
    (is equal "() / \"raw documentation\"" (draw (ui-for code)))))

(define-test function-code-space-after-docstring :parent editing
  (let ((ui (goto (ui-for "(defun f () \"doc\" (print 1))")
                  'parse:fun-code 'parse::docstring)))
    (press ui :space)
    (is equal "(DEFUN F () \"doc\" _ (PRINT 1))" (code ui))
    (is equal '(parse:fun-code (parse:body 0)) (focus-path ui))))

(define-test entry-point-top-level-is-a-list :parent editing
  (is equal t (listp (w::demo-ast)))
  (let ((forms (w::parse-source "")))
    (is = 1 (length forms))
    (is equal 'parse:hole (type-of (first forms)))))

(define-test empty-source-renders-a-hole :parent editing
  (let ((ui (ui-for nil)))
    (is equal '("hole") (lines ui :rows 3 :cols 5))))

(define-test source-file-root-renders-vertically :parent editing
  (let ((ui (ui-for (list (parse "(f 1)") (parse "(g 2)")))))
    (flet ((rendered-line (row)
             (let ((buffer (render-buffer ui)))
               (string-right-trim
                " "
                (with-output-to-string (stream)
                  (dotimes (column (array-dimension buffer 1))
                    (write-string (tui::cell-string (aref buffer row column)) stream)))))))
      (is equal "f 1" (rendered-line 0))
      (is equal "" (rendered-line 1))
      (is equal "g 2" (rendered-line 2))
      (goto ui 1 '(parse:body 0))
      (press ui :rubout #\3)
      (is equal "g 3" (rendered-line 2)))))

(define-test top-level-window-follows-focus :parent editing
  (let* ((forms (loop for i below 8 collect (parse (format nil "(f ~d)" i))))
         (ui (ui-for forms)))
    (is equal '("f 0" "f 1") (lines ui :rows 3 :cols 20))
    (goto ui 5 '(parse:body 0))
    (is equal '("f 5" "f 6") (lines ui :rows 3 :cols 20))
    (is equal '(4 5 6 7)
        (mapcar #'w::segment-index
                (w::scroll-state-segments (w::scroll-state ui))))
    (w::move-down (gethash (w::node-at (w::focus ui)) (w::node-views ui)) ui)
    (is equal '(6 parse:name) (focus-path ui))
    (is equal '("f 5" "f 6") (lines ui :rows 3 :cols 20))))

(define-test completion-clips-to-scrolled-viewport :parent editing
  (let* ((ui (ui-for (loop for i below 8 collect (parse (format nil "(f ~d)" i)))))
         (anchor (progn (goto ui 5 'parse:name) (focused ui))))
    (setf (w::completion-state ui)
          (make-instance 'w::completion-state :anchor anchor
                         :candidates '("FOO" "FOOBAR" "FOOBAZ")))
    (is equal '("f 5" "foo" "foobar") (lines ui :rows 3 :cols 20))
    (setf (w::completion-state ui) nil)
    (goto ui 5 '(parse:body 0))
    (w::swap-node (w::focus ui) (parse "9") ui)
    (is equal '("f 9" "f 6") (lines ui :rows 3 :cols 20))))

(define-test scrolling-down-keeps-focus-at-bottom :parent editing
  (let ((ui (ui-for (loop for i below 8 collect (parse (format nil "(f ~d)" i))))))
    (goto ui 1 'parse:name)
    (lines ui :rows 3 :cols 20)
    (loop for index from 2 to 4
          do (w::move-down (gethash (focused ui) (w::node-views ui)) ui)
             (is equal (list (format nil "f ~d" (1- index))
                             (format nil "f ~d" index))
                 (lines ui :rows 3 :cols 20))
             (is = 2 (- (tui:rect-y (w::focus-rect ui))
                        (w::scroll-state-viewport-row (w::scroll-state ui)))))))

(define-test segment-focus-survives-redisplay :parent editing
  (let ((ui (ui-for (list (parse "(f 0)")
                          (parse (format nil "(defun g () ~s 1)"
                                         (format nil "first~%second")))
                          (parse "(f 2)")))))
    (goto ui 1 'parse:fun-code 'parse::docstring)
    (let ((first (lines ui :rows 3 :cols 20)))
      (is equal first (lines ui :rows 3 :cols 20))
      (is eq (tui:rect (gethash :focus-view (w::redisplay-cache ui)))
          (w::focus-rect ui))
      (is = 2 (tui:rect-rows (w::focus-rect ui))))))

(define-test rendering-clips-to-small-windows :parent editing
  (dolist (case '(("(f a b c)" 1 1)
                  ("(/ a b)" 1 3)
                  ("(* a (+ b c))" 1 4)))
    (destructuring-bind (source rows cols) case
      (is = (* rows cols)
          (array-total-size (render-buffer (ui-for source) :rows rows :cols cols))))))

(define-test file-loads-exact-owning-system :parent editing
  (let ((file (asdf:system-relative-pathname
               :alexandria "alexandria-1/strings.lisp")))
    (multiple-value-bind (forms system component end length)
        (w::parse-file-loading-system file)
      (is equal "alexandria" (asdf:component-name system))
      (is equal "strings" (asdf:component-name component))
      (is = length end)
      (is = 2 (length forms))
      (is equal (find-package :alexandria)
          (parse:home-package (first (parse:body (second forms))))))))

(define-test unexpanded-macro-name-is-highlighted :parent editing
  (let ((bad (cell-bgs (ui-for "(loop for i from 1 to 10 (print i))") 6))
        (good (cell-bgs (ui-for "(loop for i from 1 to 10 do (print i))") 6)))
    ;; only the four cells of the name differ, and they are orange
    (is equal (subseq good 4) (subseq bad 4))
    (is equal t (every (lambda (b) (/= b (first good))) (subseq bad 0 4)))
    (is equal t (apply #'= (subseq bad 0 4)))
    (is equal t (> (ldb (byte 8 16) (first bad)) (ldb (byte 8 0) (first bad)))))
  ;; a call that expands is not highlighted
  (is equal t (let ((bgs (cell-bgs (ui-for "(dolist (x xs) x)") 6)))
                (apply #'= bgs))))

(define-test macro-call-with-hole-op-renders :parent editing
  (let ((ui (ui-for (parse:update (parse "(when a)") '(parse:body 0) (parse:ref-list (parse:hole))))))
    (goto ui 'parse:op)
    (press ui :rubout :rubout :rubout :rubout)
    (is equal "(_ (_))" (code ui))
    (is equal "hole (hole)" (draw ui))))

;;; layout keys

(define-test let-bindings :parent editing
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) (f a) (g b))") 'parse:op)))
    (press ui :space)
    (is equal "(LET ((_ _) (A 1) (B 2)) (F A) (G B))" (code ui))
    (is equal '((parse:vars 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) (f a))") '(parse:vars 0))))
    (press ui :space)
    (is equal "(LET ((A 1) (_ _) (B 2)) (F A))" (code ui))
    (is equal '((parse:vars 1 0)) (focus-path ui))
    (goto ui '(parse:vars 0))
    (press ui :space)
    (is equal '((parse:vars 1 0)) (focus-path ui))
    (is equal 1 (history-length ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) (f a))") '(parse:vars 1 0))))
    (press ui :rubout :rubout)
    (is equal "(LET ((A 1)) (F A))" (code ui))
    (is equal '((parse:vars 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(let ((a 1)) a)") '(parse:vars 0 0))))
    (press ui :rubout :rubout)
    (is equal "(LET ((_ _)) A)" (code ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) (f a))") 'parse:vars :open)))
    (press ui :space)
    (is equal "(LET ((_ _) (A 1) (B 2)) (F A))" (code ui))
    (is equal '((parse:vars 0 0)) (focus-path ui))))

(define-test body-forms :parent editing
  (let ((ui (goto (ui-for "(progn (f 1) (g 2))") '(parse::forms 0))))
    (press ui :rubout)
    (is equal "(PROGN (G 2))" (code ui)))
  (let ((ui (goto (ui-for "(progn (f 1) (g 2))") '(parse::forms 0))))
    (press ui :space)
    (is equal "(PROGN (F 1) _ (G 2))" (code ui))
    (is equal '((parse::forms 1)) (focus-path ui))))

(define-test emptying-a-slot-focuses-its-template :parent editing
  (flet ((empty (source &rest path)
           (let ((ui (apply #'goto (ui-for source) path)))
             (press ui :rubout :rubout)
             (list (code ui) (focus-path ui)))))
    (is equal '("(RETURN-FROM B _)" ((parse:value 0)))
        (empty "(return-from b 1)" '(parse:value 0)))
    (is equal '("(BLOCK B _)" ((parse:body 0))) (empty "(block b 1)" '(parse:body 0)))
    (is equal '("(IF A _)" ((parse::then-else 0))) (empty "(if a b)" '(parse::then-else 0)))
    (is equal '("(PROGN _)" ((parse::forms 0))) (empty "(progn 1)" '(parse::forms 0)))
    (is equal '("(UNWIND-PROTECT A _)" ((parse::cleanup 0)))
        (empty "(unwind-protect a b)" '(parse::cleanup 0)))
    (is equal '("(LET* ((_ _)) 2)" ((parse:vars 0 0)))
        (empty "(let* ((a 1)) 2)" '(parse:vars 0 1)))
    (is equal '("(FLET ((_ () _)) 2)" ((parse::funs 0 0)))
        (empty "(flet ((f () 1)) 2)" '(parse::funs 0 1))))
  ;; one undo step per emptied slot
  (let ((ui (goto (ui-for "(block b 1)") '(parse:body 0))))
    (press ui :rubout :rubout)
    (is equal 2 (history-length ui))
    (press ui '(:ctrl #\u) '(:ctrl #\u))
    (is equal "(BLOCK B 1)" (code ui))))

(define-test quoted-trees :parent editing
  (let ((ui (goto (ui-for "'(a b)") '(parse::thing 0))))
    (press ui :space)
    (is equal "(QUOTE (A _ B))" (code ui))
    (is equal '((parse::thing 1)) (focus-path ui)))
  (let ((ui (goto (ui-for "'(a b)") 'parse::thing)))
    (press ui :rubout)
    (is equal "(QUOTE _)" (code ui)))
  (let ((ui (goto (ui-for "'(a b)") 'parse::thing :open)))
    (press ui :space)
    (is equal "(QUOTE (_ A B))" (code ui))
    (is equal '((parse::thing 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "'(a b)") '(parse::thing 1))))
    (press ui #\()
    (is equal "(QUOTE (A (B _)))" (code ui))
    (is equal '((parse::thing 1 1)) (focus-path ui))))

(define-test lambda-lists :parent editing
  (let ((ui (goto (ui-for "(lambda (x y) x)") 'parse:fun-code '(parse:lambda-list 0))))
    (press ui :space)
    (is equal '(parse:fun-code (parse:lambda-list 1)) (focus-path ui))
    (goto ui 'parse:fun-code '(parse:lambda-list 0))
    (press ui :space)
    (is equal 1 (history-length ui)))
  (let ((ui (goto (ui-for "(lambda (x) x)") 'parse:fun-code '(parse:lambda-list 0))))
    (press ui #\()
    (is equal "(LAMBDA ((X _)) X)" (code ui))
    (is equal '(parse:fun-code (parse:lambda-list 0 1)) (focus-path ui))
    (press ui '(:ctrl #\u))
    (is equal "(LAMBDA (X) X)" (code ui))))

;;; macro calls

(define-test macro-space-jumps-to-holes :parent editing
  (let ((ui (goto (ui-for "(loop for x in xs)") '(parse:body 2))))
    (press ui :space)
    (is equal "(LOOP FOR X IN _ XS)" (code ui))
    (goto ui '(parse:body 2))
    (press ui :space)
    (is equal '((parse:body 3)) (focus-path ui))
    (is equal 1 (history-length ui))))

(define-test macro-list-edits-follow-reparse :parent editing
  (let ((ui (goto (ui-for "(loop for x in xs)") '(parse:body 3))))
    (press ui :rubout :rubout #\( :space #\x)
    (is equal "(LOOP FOR X IN (_ X))" (code ui))
    (goto ui '(parse:body 3 0))
    (press ui :rubout)
    (is equal "(LOOP FOR X IN (X))" (code ui))
    (is equal '((parse:body 3) parse:name) (focus-path ui)))
  (let ((ui (goto (ui-for "(loop for x in xs)") '(parse:body 3))))
    (press ui #\()
    (is equal "(LOOP FOR X IN (XS _))" (code ui))
    (is equal '((parse:body 3) (parse:body 0)) (focus-path ui))))

(define-test typing-a-call-into-a-macro-argument :parent editing
  (let ((ui (goto (ui-for "(loop for x in xs)") '(parse:body 3))))
    (press ui :rubout :rubout #\( #\q #\u #\o #\t #\e :enter #\a)
    (is equal "(LOOP FOR X IN (QUOTE A))" (code ui))))

(define-test completion-survives-macro-arguments :parent editing
  (let ((ui (goto (ui-for "(when xs wr)") '(parse:body 1))))
    (press ui #\i)
    (is equal t (and (w::completion-state ui) t))
    (press ui #\t)
    (is equal t (and (w::completion-state ui) t))))

;;; enter

(define-test enter-makes-calls :parent editing
  (let ((ui (goto (ui-for "(print xs)") '(parse:body 0))))
    (press ui :rubout :rubout #\f :enter)
    (is equal "(PRINT (F _))" (code ui))
    (is equal '((parse:body 0) (parse:body 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(list cons)") '(parse:body 0))))
    (press ui :enter)
    (is equal "(LIST (CONS _ _))" (code ui)))
  (let ((ui (goto (ui-for "(list when)") '(parse:body 0))))
    (press ui :enter)
    (is equal "(LIST (WHEN _))" (code ui))
    (is equal '((parse:body 0) (parse:body 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(let ((a 1)) a)") '(parse:vars 0 0))))
    (press ui :enter)
    (is equal "(LET ((A 1)) A)" (code ui))))

(define-test completion-then-enter :parent editing
  (flet ((complete (word)
           (let ((ui (goto (ui-for "(list xs)") '(parse:body 0))))
             (apply #'press ui :rubout :rubout (coerce word 'list))
             (press ui :enter)
             ui)))
    (is equal "(LIST (CONS _ _))" (code (complete "CONS")))
    (let ((ui (complete "WHEN")))
      (is equal "(LIST (WHEN _))" (code ui))
      (is equal '((parse:body 0) (parse:body 0)) (focus-path ui)))
    (is equal "(LIST MOST-POSITIVE-FIXNUM)" (code (complete "MOST-POSITIVE-FIXNUM")))))

(define-test empty-lists :parent editing
  (let ((ui (goto (ui-for "(print xs)") '(parse:body 0))))
    (press ui :rubout :rubout #\()
    (is equal "(PRINT ())" (code ui))
    (press ui :rubout)
    (is equal "(PRINT _)" (code ui))))

;;; navigation

(define-test parent-skips-plain-bodies :parent editing
  (let ((ui (goto (ui-for "(defun g (a) (if a (f 1) (g 2)))")
                  'parse:fun-code '(parse:body 0) '(parse::then-else 0))))
    (press ui '(:alt #\p))
    (is equal '(parse:fun-code (parse:body 0)) (focus-path ui))
    (press ui '(:alt #\n))
    (is equal '(parse:fun-code (parse:body 0) (parse::then-else 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) (f a))") '(parse:vars 1 0))))
    (press ui '(:alt #\p))
    (is equal '((parse:vars 1)) (focus-path ui))
    (press ui '(:alt #\p))
    (is equal '(parse:vars) (focus-path ui))))

(define-test move-back-follows-layout-order :parent editing
  (flet ((back (source id) (w::move-back (parse source) id)))
    (is equal 'parse:name (back "(f 1)" '(parse:body 0)))
    (is equal '(parse:body 0) (back "(f 1 2)" '(parse:body 1)))
    (is equal 'parse:name (back "(f)" 'parse:name))
    (is equal 'parse::tag (back "(catch tag 1)" '(parse:body 0)))
    (is equal 'parse:name (back "(return-from b 1)" '(parse::value 0)))
    (is equal 'parse:vars (back "(let* ((a 1)) 2)" '(parse:body 0)))
    (is equal 'parse:op (back "(let* ((a 1)) 2)" '(parse:vars 0)))
    (is equal '(parse:vars 0 0) (back "(let* ((a 1)) 2)" '(parse:vars 0 1)))
    (is equal '(parse:vars 0) (back "(let* ((a 1)) 2)" '(parse:vars 0 0)))
    ;; empty slots are skipped, or enter the last element
    (is equal 'parse:op (back "(eval-when () 1)" '(parse:body 0)))
    (is equal '(parse::situations 0) (back "(eval-when (:execute) 1)" '(parse:body 0)))
    (is equal 'parse:name (back "(defmethod f ((x t)) 1)" 'parse:fun-code))
    (is equal '(parse::qualifiers 0) (back "(defmethod f :around ((x t)) 1)" 'parse:fun-code)))
  (flet ((code-back (source &rest path)
           (let ((ui (apply #'goto (ui-for source) path)))
             (press ui :rubout :rubout)
             (list (code ui) (focus-path ui)))))
    (is equal '("(F)" (parse:name)) (code-back "(f 1)" '(parse:body 0)))
    (is equal '("(DEFUN G (X))" (parse:fun-code parse:lambda-list))
        (code-back "(defun g (x) 1)" 'parse:fun-code '(parse:body 0)))
    (is equal '("(DEFUN G (X) \"doc\")" (parse:fun-code parse::docstring))
        (code-back "(defun g (x) \"doc\" 1)" 'parse:fun-code '(parse:body 0)))
    (is equal '("(DEFUN G (X) \"doc\" (DECLARE (TYPE FIXNUM X)))"
                (parse:fun-code (parse:declarations 0)))
        (code-back "(defun g (x) \"doc\" (declare (type fixnum x)) 1)"
                   'parse:fun-code '(parse:body 0)))))

(define-test focusing-a-body-asserts :parent editing
  (fail (press (goto (ui-for "(block b 1)") 'parse:body) #\z) error)
  (fail (press (goto (ui-for "(return-from b 1)") 'parse::value) #\z) error)
  (fail (press (goto (ui-for "(f 1)") 'parse:body) #\z) error)
  ;; syntax lists are focusable
  (is equal "(LET ((A 1)) A)" (code (press (goto (ui-for "(let ((a 1)) a)") 'parse:vars) #\z)))
  (is equal "(QUOTE (A))" (code (press (goto (ui-for "'(a)") 'parse::thing) #\z))))

(define-test first-hole-focus :parent editing
  (flet ((first-hole (syntax)
           (let ((ui (ui-for (parse:parse-syntax syntax))))
             (w::focus-first-hole ui)
             (focus-path ui))))
    (is equal '((parse:body 0)) (first-hole (parse:ref-list (sym "LIST") (parse:hole))))
    (is equal '((parse:body 0)) (first-hole (parse:ref-list (sym "WHEN") (parse:hole))))
    (is equal '((parse:body 0 0))
              (first-hole (parse:ref-list (sym "DOLIST") (parse:ref-list (parse:hole) (parse:hole)))))
    (is equal nil (first-hole (parse:ref-list (sym "WHEN") (parse:ref-list (sym "PRINT") (parse:hole)))))
    (is equal '((parse:vars 0 1))
              (first-hole (parse:ref-list (sym "LET") (parse:ref-list (parse:ref-list (sym "X") (parse:hole)))
                                          (parse:hole))))
    (is equal '(parse::thing) (first-hole (parse:ref-list (sym "QUOTE") (parse:hole))))
    (is equal nil (first-hole (parse:ref-list (sym "FUNCTION")
                                              (parse:ref-list (sym "LAMBDA") (parse:ref-list (parse:hole))
                                                              (parse:hole)))))))

;;; binders

(defun binder-and-bound (source &rest path)
  (let* ((ui (apply #'goto (ui-for source) path))
         (node (focused ui))
         (binder (w::compute-focus-binder node (w::stack ui) ui)))
    (list (when binder (parse:name binder))
          (and (parse:lexical-symbol-ref node (w::focus ui))
               (not (w::symbol-ref-boundp node (w::stack ui) ui))))))

(defun resolution-at (source namespace &rest path)
  (let* ((ui (apply #'goto (ui-for source) path))
         (node (focused ui)))
    (multiple-value-bind (entry kind)
        (parse:env-lookup node namespace (w::environment-at (w::stack ui)))
      (list (when entry (sx (alexandria:ensure-car entry))) kind))))

(define-test environment-transformers-special-scope :parent editing
  (is equal '(nil :special)
      (resolution-at "(let ((x 1)) (locally (declare (special x)) x))"
                     :variable '(parse:body 0) '(parse:body 0)))
  (is equal '("X" :lexical)
      (resolution-at "(locally (declare (special x)) (let ((x 1)) x))"
                     :variable '(parse:body 0) '(parse:body 0)))
  (is equal '("X" :lexical)
      (resolution-at "(let ((x 1)) (let ((y x)) (declare (special x)) y))"
                     :variable '(parse:body 0) '(parse:vars 0 1)))
  (is equal '(nil :special)
      (resolution-at "(let* ((x 1) (y x)) (declare (special x)) y)"
                     :variable '(parse:vars 1 1)))
  (is equal '(nil nil)
      (resolution-at "(let* ((y x) (x 1)) (declare (special x)) y)"
                     :variable '(parse:vars 0 1)))
  (is equal '(nil :special)
      (resolution-at "(lambda (x &optional (y x)) (declare (special x)) y)"
                     :variable 'parse:fun-code '(parse:lambda-list 2 1)))
  (is equal '(nil nil)
      (resolution-at "(lambda (&optional (y x)) (declare (special x)) y)"
                     :variable 'parse:fun-code '(parse:lambda-list 1 1)))
  (is equal '("X" :lexical)
      (resolution-at "(defmacro m ((x &optional (y x))) y)"
                     :variable 'parse:macro-code '(parse:lambda-list 0 2 1)))
  (dolist (source '("(flet ((f () nil)) (declare (special x)) x)"
                    "(labels ((f () nil)) (declare (special x)) x)"
                    "(macrolet ((m () nil)) (declare (special x)) x)"
                    "(symbol-macrolet ((m nil)) (declare (special x)) x)"))
    (is equal '(nil :special) (resolution-at source :variable '(parse:body 0)))))

(define-test environment-transformers-function-boundaries :parent editing
  (is equal '(nil nil)
      (resolution-at "(defun f () (f))" :function
                     'parse:fun-code '(parse:body 0) 'parse:name))
  (is equal '("F" :lexical)
      (resolution-at "(defun f (&optional (x (return-from f 1))) x)"
                     :block 'parse:fun-code '(parse:lambda-list 1 1) 'parse:name))
  (is equal '("F" :lexical)
      (resolution-at "(block f (defun f (&optional (x (return-from f 1))) x))"
                     :block '(parse:body 0) 'parse:fun-code '(parse:lambda-list 1 1) 'parse:name))
  (is equal '("F" :lexical)
      (resolution-at "(defun f () (return-from f 1))"
                     :block 'parse:fun-code '(parse:body 0) 'parse:name))
  (dolist (operator '(flet labels))
    (let ((source (format nil "(~a ((f (&optional (x (return-from f 1))) (return-from f x))) nil)"
                          operator)))
      (is equal '("F" :lexical)
          (resolution-at source :block '(parse:funs 0 1) '(parse:lambda-list 1 1) 'parse:name))
      (is equal '("F" :lexical)
          (resolution-at source :block '(parse:funs 0 1) '(parse:body 0) 'parse:name)))))

(defmacro environment-special-body (name form)
  `(locally (declare (special ,name)) ,form))

(define-test macro-environment-transformers-special-scope :parent editing
  (is equal '(nil :special)
      (resolution-at "(let ((x 1)) (weave-tests::environment-special-body x (print x)))"
                     :variable '(parse:body 0) '(parse:body 1) '(parse:body 0)))
  (is equal '(nil :special)
      (resolution-at "(destructuring-bind (x) '(1) (declare (special x)) (print x))"
                     :variable '(parse:body 3) '(parse:body 0)))
  (let* ((source "(weave-tests::environment-special-body x (print x))")
         (syntax (probe-driver-syntax source))
         (x (parse::gen-nth 1 syntax))
         (old-env (parse::env-with-specials (parse:make-env) (list x)))
         (call (parse::parse syntax old-env
                            (lambda (source ast) (declare (ignore source)) ast))))
    ;; A redundant declaration is still an effect when applied to a new parent.
    (is eq :special
        (nth-value 1 (parse:env-lookup x :variable
                                      (parse:location-env call '(parse:body 1)
                                                          (parse:make-env)))))
    (is eq nil (parse::%specials (parse:make-env))))
  (let* ((call (parse "(weave-tests::copied-body x (print x))"))
         (env (parse::env-with-specials (parse:make-env)
                                        (list (first (parse:body call)))))
         (inside (parse:location-env call '(parse:body 1) env)))
    ;; A new lexical binding overrides an incoming local special declaration.
    (is eq :lexical (nth-value 1 (parse:env-lookup (first (parse:body call)) :variable inside)))
    (is eq :special (nth-value 1 (parse:env-lookup (first (parse:body call)) :variable env)))))

(define-test environment-transformers-survive-copy-and-update :parent editing
  (let* ((original (parse "(defun f () (return-from f))"))
         (copy (parse:copy-node original)))
    (dolist (node (list original copy))
      (let* ((code (parse:fun-code node))
             (env (parse:location-env code '(parse:body 0)
                                      (parse:location-env node 'parse:fun-code (parse:make-env)))))
        (is eq (parse:name node) (parse:env-lookup (parse:name node) :block env))))
    (let* ((renamed (parse:update copy 'parse:name (sym "G" "WEAVE-TESTS")))
           (code (parse:fun-code renamed))
           (env (parse:location-env code '(parse:body 0)
                                    (parse:location-env renamed 'parse:fun-code (parse:make-env)))))
      (is eq (parse:name renamed) (parse:env-lookup (parse:name renamed) :block env))
      (is eq nil (parse:env-lookup (parse:name original) :block env)))))

(define-test binders-and-unbound-references :parent editing
  (is equal '("F" nil) (binder-and-bound "(flet ((f (x) x)) (f 1))" '(parse:body 0) 'parse:name))
  (is equal '("F" nil) (binder-and-bound "(flet ((f () 1)) (function f))" '(parse:body 0) 'parse:fun-designator))
  (is equal '(nil t) (binder-and-bound "(defun g (x) (h x))" 'parse:fun-code '(parse:body 0) 'parse:name))
  (is equal '("X" nil) (binder-and-bound "(defun g (x) (h x))" 'parse:fun-code '(parse:body 0) '(parse:body 0)))
  (is equal '(nil nil) (binder-and-bound "(print *standard-output*)" '(parse:body 0)))
  (is equal '(nil nil) (binder-and-bound "(open f :direction :output)" '(parse:body 1)))
  (is equal '(nil nil) (binder-and-bound "(list nil t)" '(parse:body 0)))
  (is equal '(nil t) (binder-and-bound "(list nosuchvar)" '(parse:body 0)))
  (is equal '("X" nil) (binder-and-bound "(loop for x in xs collect x)" '(parse:body 5)))
  (is equal '(nil t) (binder-and-bound "(loop for x in xs collect x)" '(parse:body 3)))
  (is equal '(nil nil) (binder-and-bound "(loop for x in xs collect x)" '(parse:body 0)))
  (is equal '("Y" nil) (binder-and-bound "(dolist (y ys) (print y))" '(parse:body 1) '(parse:body 0)))
  (is equal '("X" nil) (binder-and-bound "(loop for x in xs do (let ((x 2)) (print x)))"
                                         '(parse:body 5) '(parse:body 0) '(parse:body 0)))
  (is equal '("NEXT" nil) (binder-and-bound "(with-hash-table-iterator (next h) (next))"
                                            '(parse:body 1) 'parse:op))
  (is equal '(nil t) (binder-and-bound "(with-hash-table-iterator (next h) (nosuchfn))"
                                       '(parse:body 1) 'parse:name))
  (is equal '("OUTER" nil) (binder-and-bound "(loop named outer for x in xs do (return-from outer x))"
                                             '(parse:body 7) 'parse:name))
  (is equal '("B" nil) (binder-and-bound "(block b (return-from b 1))" '(parse:body 0) 'parse:name))
    (is equal '(nil t) (binder-and-bound "(block b (return-from nope 1))" '(parse:body 0) 'parse:name)))

;;; selections

(define-test selections-in-lists :parent editing
  (let ((ui (goto (ui-for "'(a b c d)") '(parse::thing 1))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::thing 0))
    (press ui '(:ctrl #\v))
    (is equal "(QUOTE (A B C B C D))" (code ui)))
  (let ((ui (goto (ui-for "'(a b)") '(parse::thing 0))))
    (press ui '(:shift :right) '(:ctrl #\x))
    (is equal "(QUOTE ())" (code ui))
    (is equal '(parse::thing) (focus-path ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2) (c 3)) a)") '(parse:vars 0))))
    (press ui '(:shift :right) '(:ctrl #\x))
    (is equal "(LET ((C 3)) A)" (code ui)))
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) a)") '(parse:vars 0))))
    (press ui '(:shift :right) '(:ctrl #\x))
    (is equal "(LET ((_ _)) A)" (code ui)))
  (let ((ui (goto (ui-for "(progn (f 1) (g 2))") '(parse::forms 0))))
    (press ui '(:shift :right) '(:ctrl #\x))
    (is equal "(PROGN _)" (code ui)))
  (let ((ui (goto (ui-for "(loop for (a b) in xs collect a)") '(parse:body 1 0))))
    (press ui '(:shift :right) '(:ctrl #\x))
    (is equal "(LOOP FOR () IN XS COLLECT A)" (code ui))))

(define-test paste-sorts :parent editing
  (let ((ui (goto (ui-for "(progn (f 1) (g 2) '(x))") '(parse::forms 0))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::forms 2) '(parse::thing 0))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (F 1) (G 2) (QUOTE (X (F 1) (G 2))))" (code ui)))
  (let ((ui (goto (ui-for "(progn (f 1) '(x))") '(parse::forms 0))))
    (press ui '(:ctrl #\c))
    (goto ui '(parse::forms 1) '(parse::thing 0))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (F 1) (QUOTE (X (F 1))))" (code ui)))
  (let ((ui (goto (ui-for "(progn '(a b c) (f 1))") '(parse::forms 0) '(parse::thing 1))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::forms 1))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (QUOTE (A B C)) (F 1))" (code ui))))

(define-test paste-checks-every-form :parent editing
  (let ((ui (goto (ui-for "(progn (loop for x in xs do (print x)) (f 1) '(z))")
                  '(parse::forms 0) '(parse:body 3))))
    (press ui '(:shift :right) '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::forms 1))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (LOOP FOR X IN XS DO (PRINT X)) (F 1) (QUOTE (Z)))" (code ui))
    (goto ui '(parse::forms 2) '(parse::thing 0))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (LOOP FOR X IN XS DO (PRINT X)) (F 1) (QUOTE (Z XS DO (PRINT X))))"
        (code ui))))

(define-test paste-fills-holes :parent editing
  (let ((ui (goto (ui-for "(list a b)") '(parse:body 0))))
    (press ui '(:ctrl #\c))
    (goto ui '(parse:body 1))
    (press ui :rubout '(:ctrl #\v))
    (is equal "(LIST A A)" (code ui))
    (is equal '((parse:body 1)) (focus-path ui))))

(define-test paste-over-selections :parent editing
  (let ((ui (goto (ui-for "(let ((a 1) (b 2)) a)") '(parse:vars 0 0))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse:vars 1 1))
    (press ui '(:shift :left) '(:ctrl #\v))
    (is equal "(LET ((A 1) (A 1)) A)" (code ui)))
  (let ((ui (goto (ui-for "(progn (let ((a 1)) a) (flet ((f (x) x)) (f 1)))")
                  '(parse::forms 0) '(parse:vars 0 0))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::forms 1) '(parse::funs 0 1))
    (press ui '(:shift :left) '(:ctrl #\v))
    (is equal "(PROGN (LET ((A 1)) A) (FLET ((F (X) X)) (F 1)))" (code ui)))
  (let ((ui (goto (ui-for "(progn (let ((a 1) (b 2)) a) (flet ((f (x) x)) (f 1)))")
                  '(parse::forms 0) '(parse:vars 0))))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse::forms 1) '(parse::funs 0))
    (press ui '(:ctrl #\v))
    (is equal "(PROGN (LET ((A 1) (B 2)) A) (FLET ((F (X) X)) (F 1)))" (code ui))))

(define-test paste-duplicates-after-selections :parent editing
  (let ((ui (goto (ui-for "'(a b c)") '(parse::thing 0))))
    (press ui '(:shift :right) '(:ctrl #\c) '(:ctrl #\v))
    (is equal "(QUOTE (A B A B C))" (code ui))))

(define-test paste-focuses-last-form :parent editing
  (let ((ui (goto (ui-for "'(a b c)") '(parse::thing 0))))
    (press ui '(:shift :right) '(:ctrl #\c) '(:ctrl #\v))
    (is equal '((parse::thing 3)) (focus-path ui))))

(define-test loop-layout-renders :parent editing
  (is equal "loop /  for x in xs /  do print x /     print y"
            (draw (ui-for "(loop for x in xs do (print x) (print y))"))))

;;; goal stack and handles

(define-test goal-stack :parent editing
  (let ((ui (goto (ui-for "'(a (b c) d)") '(parse::thing 1 1))))
    (press ui '(:alt #\p) '(:alt #\p))
    (is equal '(parse::thing) (focus-path ui))
    (press ui '(:alt #\n) '(:alt #\n))
    (is equal '((parse::thing 1 1)) (focus-path ui))
    (press ui '(:alt #\n))
    (is equal '((parse::thing 1 1)) (focus-path ui)))
  (let ((ui (goto (ui-for "'(a (b c) d)") '(parse::thing 1 1))))
    (press ui '(:alt #\p) :space '(:alt #\n))
    (is equal '((parse::thing 2)) (focus-path ui))))

(define-test handle-cuts-its-list :parent editing
  (let ((ui (goto (ui-for "'(a (b c))") '(parse::thing 1) :open)))
    (press ui '(:ctrl #\x))
    (is equal "(QUOTE (A _))" (code ui))))

(define-test wrapping-holes :parent editing
  (let ((ui (goto (ui-for "'(a b)") '(parse::thing 1))))
    (press ui :space #\( #\( #\x)
    (is equal "(QUOTE (A B ((X))))" (code ui))
    (is equal '((parse::thing 2 0 0)) (focus-path ui)))
  (let ((ui (goto (ui-for "(loop for x in xs)") '(parse:body 3))))
    (press ui :space #\( #\( #\x)
    (is equal "(LOOP FOR X IN XS ((X)))" (code ui))))

;;; edited macro arguments

(define-test edited-macro-arguments-copy-and-paste :parent editing
  (let ((ui (goto (ui-for "(dolist (x xs) (print a) b)") '(parse:body 1) '(parse:body 0))))
    (press ui #\z)
    (goto ui '(parse:body 1))
    (press ui '(:ctrl #\c))
    (goto ui '(parse:body 2))
    (press ui '(:ctrl #\v))
    (is equal "(DOLIST (X XS) (PRINT AZ) B (PRINT AZ))" (code ui)))
  (let ((ui (goto (ui-for "(dolist (x xs) (print a) (print b))") '(parse:body 1) '(parse:body 0))))
    (press ui #\z)
    (goto ui '(parse:body 2) '(parse:body 0))
    (press ui #\y)
    (goto ui '(parse:body 1))
    (press ui '(:shift :right) '(:ctrl #\c))
    (goto ui '(parse:body 2))
    (press ui '(:ctrl #\v))
    (is equal "(DOLIST (X XS) (PRINT AZ) (PRINT BY) (PRINT AZ) (PRINT BY))" (code ui))))

(define-test macro-rubout-focus :parent editing
  (let ((ui (goto (ui-for "(when x)") '(parse:body 0))))
    (press ui :rubout :rubout)
    (is equal "(WHEN)" (code ui))
    (is equal '(parse:op) (focus-path ui)))
  (let ((ui (goto (ui-for "(loop for (a) in xs)") '(parse:body 1 0))))
    (press ui :rubout :rubout)
    (is equal "(LOOP FOR () IN XS)" (code ui))
    (is equal '((parse:body 1)) (focus-path ui))))

;;; function calls and undo

(define-test call-space :parent editing
  (let ((ui (goto (ui-for "(f a b)") '(parse:body 0))))
    (press ui :space)
    (is equal "(F A _ B)" (code ui))
    (press ui :space)
    (is equal "(F A _ _ B)" (code ui)))
  (let ((ui (goto (ui-for "(f a b)") '(parse:body 1))))
    (press ui :rubout)
    (goto ui '(parse:body 0))
    (press ui :space)
    (is equal '((parse:body 1)) (focus-path ui))
    (is equal 1 (history-length ui)))
  (let ((ui (goto (ui-for "(f a b)") 'parse:name)))
    (press ui :space)
    (is equal "(F _ A B)" (code ui))))

(define-test undo-and-redo :parent editing
  (let ((ui (goto (ui-for "(list a)") '(parse:body 0))))
    (press ui :space)
    (is equal "(LIST A _)" (code ui))
    (press ui '(:ctrl #\u))
    (is equal "(LIST A)" (code ui))
    (press ui '(:ctrl #\r))
    (is equal "(LIST A _)" (code ui))))

(define-test completing-a-macro-name-reparses-arguments :parent editing
  (let ((ui (goto (ui-for "(dolist (x xs) (print x))") 'parse:op)))
    (press ui :rubout :rubout #\s :enter)
    (let ((form (first (w::ast ui))))
      (is equal 'parse:macro-call (type-of form))
      (is equal t (parse::expanded form))
      (is equal 'parse:ref-list (type-of (first (parse:body form)))))))

(define-test binders-through-expansion-lambdas :parent editing
  (is equal '("P" nil)
            (binder-and-bound "(multiple-value-bind (p q) (f) (print p))"
                              '(parse:body 2) '(parse:body 0))))

(define-test multiple-buffer-layout :parent editing
  (let* ((first (ui-for "alpha"))
         (ui (w::buffer-ui first))
         (second (w::add-buffer ui :package *package* :ast (list (parse "beta"))
                                    :source #p"example.lisp" :x 8 :y 2))
         (cells (render-buffer first :rows 6 :cols 20)))
    (is equal #p"example.lisp" (w::buffer-source second))
    (is equal '("alpha" "        beta") (lines first :rows 6 :cols 20))
    (is eq first (w::focused-buffer ui))
    (is eq (w::ast second) (w::location-node (car (last (w::stack second)))))
    (is eq nil (eq (w::node-views first) (w::node-views second)))
    (is eq nil (eq (w::scroll-state first) (w::scroll-state second)))
    ;; Unbound symbols stay red; only the active buffer gets the gold focus blend.
    (is eq nil (= #xcb4b16 (tui-sys:bg (tui::cell-style (aref cells 0 0)))))
    (is = #xcb4b16 (tui-sys:bg (tui::cell-style (aref cells 2 8))))
    (setf (w::focused-buffer ui) second)
    (setf cells (render-buffer first :rows 6 :cols 20))
    (is = #xcb4b16 (tui-sys:bg (tui::cell-style (aref cells 0 0))))
    (is eq nil (= #xcb4b16 (tui-sys:bg (tui::cell-style (aref cells 2 8)))))
    ;; A buffer outside a resized terminal neither draws nor leaves a stale view.
    (render-buffer first :rows 1 :cols 4)
    (is eq nil (w::buffer-view second))
    (is equal '("alpha" "        beta") (lines first :rows 6 :cols 20))))

(define-test multiple-buffer-input-and-history :parent editing
  (let* ((first (ui-for "a"))
         (ui (w::buffer-ui first))
         (second (w::add-buffer ui :package *package* :ast (list (parse "b")) :x 8 :y 2))
         (received nil))
    (render-buffer first :rows 6 :cols 20)
    (setf (slot-value ui 'tui::%screen) (w::make-cell-buffer 6 20))
    (flet ((send (event)
             (let ((*standard-output* (make-broadcast-stream))
                   (*terminal-io* (make-two-way-stream (make-string-input-stream "")
                                                      (make-broadcast-stream))))
               (tui:dispatch-event ui event))))
      ;; Mouse dispatch uses the topmost buffer and translates to local coordinates.
      (setf (tui:mouse-handler (w::buffer-view second))
            (lambda (view event)
              (declare (ignore view))
              (let ((data (tui:event-kind event)))
                (setf received (list (tui:mouse-data-row data)
                                     (tui:mouse-data-col data))))))
      (send (tui-sys:make-event :kind (tui-sys::make-mouse-data
                                     :button :left :state :click :row 3 :col 9)))
      (is eq second (w::focused-buffer ui))
      (is equal '(1 1) received)
      (send (event #\z))
      (is equal "A" (code first))
      (is equal "BZ" (code second))
      (is = 0 (history-length first))
      (is = 1 (history-length second))
      (send (tui-sys:make-event :kind (tui-sys::make-mouse-data
                                     :button :left :state :click :row 1 :col 1)))
      (send (event #\x))
      (is equal "AX" (code first))
      (is equal "BZ" (code second))
      (send (event '(:ctrl #\u)))
      (is equal "A" (code first))
      (is equal "BZ" (code second)))))

(define-test selection-and-clipboard-across-buffers :parent editing
  (let* ((first (goto (ui-for "(f a b)") '(parse:body 0)))
         (ui (w::buffer-ui first))
         (second (w::add-buffer ui :package *package* :ast (list (parse "(g c)")) :y 4)))
    (press first '(:shift :right) '(:ctrl #\c))
    (let ((selection (w::selection ui)))
      (is eq selection (w::selection first))
      (setf (w::focused-buffer ui) second)
      (is eq nil (eq selection (w::selection ui)))
      (is eq nil (w::selection second))
      (is eq t (w::selection-activep selection))
      (is eq nil (w::selection-activep (w::selection ui)))
      (goto second '(parse:body 0))
      (press second '(:ctrl #\v))
      (is equal "(G C A B)" (code second))
      (is equal "(F A B)" (code first)))))

(define-test completion-overlays-all-buffers :parent editing
  (let* ((buffer (ui-for "ca"))
         (ui (w::buffer-ui buffer))
         (other (w::add-buffer ui :package *package* :ast (list (parse "underneath")) :y 3)))
    (setf (w::buffer-x buffer) 3
          (w::buffer-y buffer) 2
          (w::completion-state buffer)
          (w::make-completion-state :anchor (focused buffer) :candidates '(car caar)))
    (let* ((cells (render-buffer buffer :rows 7 :cols 20))
           (overlay (first (tui:children (tui:root-view ui))))
           (rect (tui:rect overlay)))
      (is = 3 (tui:rect-x rect))
      (is = 3 (tui:rect-y rect))
      (is equal "car " (format nil "~{~a~}"
                                (loop for x from 3 below 7 collect
                                      (tui::cell-string (aref cells 3 x)))))
      (is eq other (w::buffer-at-point ui 4 4))
      ;; An overlay click must not focus the buffer beneath it.
      (setf (slot-value ui 'tui::%screen) (w::make-cell-buffer 7 20))
      (let ((*standard-output* (make-broadcast-stream))
            (*terminal-io* (make-two-way-stream (make-string-input-stream "")
                                               (make-broadcast-stream))))
        (tui:dispatch-event ui (tui-sys:make-event
                               :kind (tui-sys::make-mouse-data
                                      :button :left :state :click :row 4 :col 4))))
      (is eq buffer (w::focused-buffer ui)))))

(define-test scrolled-highlight-screen-coordinates :parent editing
  (let* ((bounds (tui:make-rect :x 8 :y 2 :rows 6 :cols 12))
         (view (tui:make-rect :x 1 :y 1 :rows 4 :cols 3))
         (visible (w::transform-rect-to-screen view 3 bounds)))
    (is = 9 (tui:rect-x visible))
    (is = 2 (tui:rect-y visible))
    (is = 2 (tui:rect-rows visible))
    (is = 0 (tui:rect-rows (w::transform-rect-to-screen view 6 bounds)))))

(define-test setf-function-name-rendering-and-editing :parent editing
  (let ((buffer (ui-for "#'(setf aref)")))
    (is equal "#'(setf aref)" (draw buffer))
    (goto buffer '(parse:fun-designator 1))
    (is eq 'parse:unevaluated (w::locsort (w::focus buffer)))
    (is equal "AREF" (parse:name (focused buffer)))
    (press buffer #\x)
    (is equal "(FUNCTION (SETF AREFX))" (code buffer))
    (is equal "#'(setf cl::arefx)" (draw buffer))
    (press buffer '(:ctrl #\u))
    (is equal "#'(setf aref)" (draw buffer))))

(define-test default-special-forms-have-all-slots :parent editing
  (maphash
   (lambda (type layout)
     (let ((node (w::default-node layout)))
       (dolist (slot (c2cl:class-slots (find-class type)))
         (is eq (not (member (c2cl:slot-definition-name slot)
                            (parse:form-optional-slots type)))
             (slot-boundp node (c2cl:slot-definition-name slot))))
       (is eq t (stringp (draw (ui-for node))))
       (is eq t (typep (parse:to-syntax node) 'parse:ref-list))))
   w::*layouts*))

(define-test complete-let-with-enter :parent editing
  (let ((buffer (ui-for nil)))
    (press buffer #\l #\e #\t :enter)
    (is eq 'parse:let-form (type-of (first (w::ast buffer))))
    (is equal "(LET ((_ _)) _)" (code buffer))
    (is equal "let (hole hole) /  hole" (draw buffer))))

(define-test buffer-package-symbol-display :parent editing
  (let ((home (make-package (string (gensym "DISPLAY-HOME-")) :use nil))
        (target (make-package (string (gensym "DISPLAY-TARGET-")) :use nil)))
    (unwind-protect
         (let* ((symbol (intern "VISIBLE" home))
                (node (sym "VISIBLE" home))
                (buffer (w::add-buffer (make-instance 'w::ui)
                                        :ast (list node) :package target)))
           (export symbol home)
           (sb-ext:add-package-local-nickname "LOCAL" home target)
           (let ((*package* home))
             (is equal "local:visible" (draw buffer)))
           (import symbol target)
           (is equal "visible" (draw buffer))
           (unintern symbol target)
           (shadow "VISIBLE" target)
           (is equal "local:visible" (draw buffer))
           (unexport symbol home)
           (is equal "local::visible" (draw buffer))
           (setf (w::buffer-package buffer) home)
           (let ((*package* target))
             (is equal "visible" (draw buffer)))
           (is equal ":visible" (w::symbol-ref-text (sym "VISIBLE" :keyword) target))
           (is equal "#:visible" (w::symbol-ref-text (sym "VISIBLE" nil) target))
           (multiple-value-bind (forms end package)
               (w::parse-source
                (format nil "(in-package ~s) visible" (package-name home)))
             (declare (ignore forms end))
             (is eq home package)))
      (delete-package target)
      (delete-package home))))

(define-test scrolling-up-does-not-stall :parent editing
  (let ((buffer (ui-for (loop for i below 10 collect (parse (format nil "(f ~d)" i))))))
    (goto buffer 7 'parse:name)
    (render-buffer buffer :rows 6 :cols 40)
    (loop for index downfrom 6 to 0
          do (w::move-up (gethash :focus-view (w::redisplay-cache buffer)) buffer)
             (is = index (w::top-level-focus-index buffer))
             (render-buffer buffer :rows 6 :cols 40))))

(define-test scrolling-up-reveals-bottom-of-preceding-form :parent editing
  (let ((buffer (ui-for (list (parse "(f 0)")
                             (parse "(progn (f 1) (f 2) (f 3) (f 4))")
                             (parse "(f 5)")
                             (parse "(f 6)")))))
    (goto buffer 2 'parse:name)
    (render-buffer buffer :rows 3 :cols 40)
    (w::move-up (gethash :focus-view (w::redisplay-cache buffer)) buffer)
    (render-buffer buffer :rows 3 :cols 40)
    (let* ((state (w::scroll-state buffer))
           (position (w::scroll-state-position state))
           (segment (find 1 (w::scroll-state-segments state) :key #'w::segment-index)))
      (is = 1 (w::top-level-focus-index buffer))
      (is = 1 (w::scroll-position-index position))
      (is = (1- (w::segment-rows segment)) (w::scroll-position-row position))
      (is eq t (plusp (w::scroll-position-row position)))
      (is = 0 (w::segment-index (first (w::scroll-state-segments state)))))))

(define-test backward-jump-stages-only-the-new-window :parent editing
  (let ((buffer (ui-for
                 (append (list (parse "(f 0)")
                               (parse "(progn (f 1) (f 2) (f 3) (f 4) (f 5))"))
                         (loop for i from 6 below 16
                               collect (parse (format nil "(f ~d)" i)))))))
    (goto buffer 2 'parse:name)
    (render-buffer buffer :rows 3 :cols 40)
    (goto buffer 1 '(parse:forms 0) 'parse:name)
    (render-buffer buffer :rows 3 :cols 40)
    (let* ((state (w::scroll-state buffer))
           (position (w::scroll-state-position state)))
      (is = 1 (w::scroll-position-index position))
      (is = (tui:rect-y (w::focus-rect buffer)) (w::scroll-state-viewport-row state))
      (is equal '(0 1 2) (mapcar #'w::segment-index (w::scroll-state-segments state))))))

(define-test buffer-state-and-root-location :parent editing
  (let* ((buffer (ui-for "(f 1)"))
         (state (w::buffer-state buffer)))
    (flet ((check-root ()
             (let ((root-location (car (last (w::stack buffer)))))
               (is eq t (integerp (w::location-id root-location)))
               (is eq (nth (w::location-id root-location) (w::ast buffer))
                   (w::node-at root-location))
               (is eq (w::ast buffer) (w::location-node root-location)))))
      (check-root)
      (goto buffer '(parse:body 0))
      (press buffer #\2)
      (check-root)
      (press buffer '(:ctrl #\u))
      (check-root)
      (press buffer '(:ctrl #\r))
      (check-root)
      (let ((old (w::render-state buffer)))
        (setf (w::render-state buffer) (w::make-render-state))
        (is eq state (w::buffer-state buffer))
        (is eq nil (eq (w::node-views old) (w::node-views buffer)))
        (is equal "f 12" (draw buffer))))))

(define-test zipper-updates-preserve-earlier-selections :parent editing
  (let ((buffer (goto (ui-for "(f (g a b) c)") '(parse:body 0) '(parse:body 0))))
    (w::begin-selection buffer)
    (let ((initial (w::selection buffer)))
      (w::expand-selection buffer)
      (let* ((zipper (w::selection buffer))
             (depth (w::zipper-depth zipper)))
        (w::take-selection buffer zipper)
        (w::expand-selection buffer)
        (is eq 'w::selection (type-of initial))
        (is = depth (w::zipper-depth zipper))
        (is eq zipper (w::zipper buffer))
        (is eq t (> (w::zipper-depth (w::selection buffer)) depth))
        (w::shrink-selection buffer)
        (is = depth (w::zipper-depth (w::selection buffer)))
        (w::extend-selection buffer 1)
        (is eq 'w::selection (type-of (w::selection buffer)))
        (is = 0 (w::selection-point zipper))
        (is = 1 (w::selection-point (w::selection buffer)))
        (w::end-selection-mode buffer)
        (is eq t (w::selection-activep zipper))
        (is eq nil (w::selection-activep (w::selection buffer)))))))

(define-test render-state-publishes-only-on-success :parent editing
  (let ((buffer (ui-for "(f 1)")))
    (render-buffer buffer)
    (let* ((old (w::render-state buffer))
           (old-view (w::buffer-view old))
           (count (hash-table-count (w::node-views old)))
           (draw (fdefinition 'w::draw-buffer)))
      (unwind-protect
           (progn
             (setf (fdefinition 'w::draw-buffer)
                   (lambda (working)
                     (funcall draw working)
                     (error "Abort render")))
             (fail (render-buffer buffer)))
        (setf (fdefinition 'w::draw-buffer) draw))
      (is eq old (w::render-state buffer))
      (is eq old-view (w::buffer-view old))
      (is = count (hash-table-count (w::node-views old)))
      (render-buffer buffer)
      (is eq nil (eq old (w::render-state buffer)))
      (is eq nil (eq (w::stage-cache old) (w::stage-cache buffer))))))

(define-test event-copy-isolates-editing-and-global-effects :parent editing
  (let* ((buffer (goto (ui-for "(f a b)") '(parse:body 0)))
         (ui (w::buffer-ui buffer))
         (old (w::buffer-state buffer)))
    (render-buffer buffer)
    (multiple-value-bind (copies effects) (w::handle-ui-event ui (event #\z))
      (is eq old (w::buffer-state buffer))
      (is equal "(F A B)" (code buffer))
      (is equal "(F AZ B)" (code (first copies)))
      (is eq nil (w::selection effects)))
    (multiple-value-bind (copies effects)
        (w::handle-ui-event ui (event '(:shift :right)))
      (declare (ignore copies))
      (is eq nil (w::selection ui))
      (is = 1 (w::selection-point (w::selection effects))))))

(define-test completion-survives-discarded-event :parent editing
  (let* ((buffer (ui-for "ca"))
         (ui (w::buffer-ui buffer))
         (completion (w::make-completion-state :anchor (focused buffer)
                                               :candidates '(car caar))))
    (setf (w::completion-state buffer) completion)
    (render-buffer buffer)
    (let ((copies (w::handle-ui-event ui (event #\r))))
      (is eq completion (w::completion-state buffer))
      (is equal "CA" (parse:name (w::anchor completion)))
      (is equal '(car caar) (w::candidates completion))
      (is equal '(car) (w::candidates (w::completion-state (first copies)))))))

(define-test failed-event-discards-state-and-effects :parent editing
  (let* ((buffer (goto (ui-for "(f a b)") '(parse:body 0)))
         (ui (w::buffer-ui buffer))
         (old (w::buffer-state buffer))
         (event (event '(:alt #\!)))
         (handlers w::*global-key-handlers*))
    (render-buffer buffer)
    (multiple-value-bind (previous present) (gethash event handlers)
      (unwind-protect
           (progn
             (setf (gethash event handlers)
                   (lambda (view working)
                     (declare (ignore view))
                     (w::extend-selection working 1)
                     (setf (w::goal-col working) 99)
                     (error "Abort event")))
             (fail (w::handle-ui-event ui event)))
        (if present (setf (gethash event handlers) previous) (remhash event handlers))))
    (is eq old (w::buffer-state buffer))
    (is = 1 (w::goal-col buffer))
    (is eq nil (w::selection ui))))

(defclass redisplay-escape-ui (tui:tui)
  ((depth :initform 0 :accessor escape-depth)
   (trace :initform nil :accessor escape-trace)))

(defmethod tui:redisplay-internal ((ui redisplay-escape-ui))
  (error "The escape test must not reach the primary method."))

(defmethod tui:redisplay-internal :around ((ui redisplay-escape-ui))
  (if (zerop (escape-depth ui))
      (progn
        (incf (escape-depth ui))
        (push (tui:redisplay ui) (escape-trace ui))
        (push :outer-resumed (escape-trace ui)))
      (progn
        (push :inner (escape-trace ui))
        (throw 'tui::quit-redisplay nil)
        (push :unreachable (escape-trace ui)))))

(define-test redisplay-wrapper-bounds-subclass-methods :parent editing
  (let ((ui (make-instance 'redisplay-escape-ui)))
    (is eq t (tui:redisplay ui))
    (is equal '(:outer-resumed nil :inner) (escape-trace ui))
    (is eq nil (tui:redisplay ui))))

(define-test evaluation-creates-floating-buffer :parent editing
  (let* ((buffer (goto (ui-for "42")))
         (ui (w::buffer-ui buffer)))
    (render-buffer buffer :rows 8 :cols 30)
    (multiple-value-bind (copies effects) (w::handle-ui-event ui (event '(:alt #\e)))
      (declare (ignore copies))
      (is = 1 (length (w::buffers ui)))
      (is = 1 (length (w::new-buffers effects)))
      (is eq t (w::floating (first (w::new-buffers effects)))))
    (setf (slot-value ui 'tui::%screen) (w::make-cell-buffer 8 30))
    (let ((*standard-output* (make-broadcast-stream))
          (*terminal-io* (make-two-way-stream (make-string-input-stream "")
                                             (make-broadcast-stream))))
      (tui:dispatch-event ui (event '(:alt #\e))))
    (is = 2 (length (w::buffers ui)))
    (let* ((result (second (w::buffers ui)))
           (cells (render-buffer buffer :rows 8 :cols 30))
           (x (w::buffer-x result))
           (y (w::buffer-y result)))
      (is eq ui (w::buffer-ui result))
      (is equal "42" (code result))
      (is equal "┌" (tui::cell-string (aref cells y x)))
      (is = #x606060 (tui-sys:fg (tui::cell-style (aref cells y x))))
      (is eq result (w::buffer-at-point ui (1+ y) (1+ x)))
      (setf (w::focused-buffer ui) result)
      (setf cells (render-buffer buffer :rows 8 :cols 30))
      (is = #xb0b0b0 (tui-sys:fg (tui::cell-style (aref cells y x))))
      (render-buffer buffer :rows 1 :cols 1)
      (is eq nil (w::buffer-view result)))))

(define-test floating-buffers-fit-contents :parent editing
  (let* ((base (ui-for "underneath"))
         (ui (w::buffer-ui base))
         (popup (w::add-buffer ui :ast (list (parse "42")) :floating t
                                 :x 18 :y 7 :package *package*)))
    (let* ((cells (render-buffer base :rows 8 :cols 20))
           (rect (w::buffer-outer-rect popup)))
      (is = 4 (tui:rect-cols rect))
      (is = 3 (tui:rect-rows rect))
      (is = 16 (tui:rect-x rect))
      (is = 5 (tui:rect-y rect))
      (is equal "┌" (tui::cell-string (aref cells 5 16)))
      (is equal "4" (tui::cell-string (aref cells 6 17)))
      (is eq popup (w::buffer-at-point ui 6 17)))
    (render-buffer base :rows 3 :cols 3)
    (let ((rect (w::buffer-outer-rect popup)))
      (is <= (tui:rect-x2 rect) 3)
      (is <= (tui:rect-y2 rect) 3))
    (render-buffer base :rows 1 :cols 1)
    (is = 1 (tui:rect-cols (w::buffer-outer-rect popup)))
    (is = 1 (tui:rect-rows (w::buffer-outer-rect popup)))))

(define-test result-pane-screen-anchor :parent editing
  (let* ((buffer (goto (ui-for "42")))
         (ui (w::buffer-ui buffer)))
    (setf (w::buffer-x buffer) 4 (w::buffer-y buffer) 3 (w::floating buffer) t)
    (render-buffer buffer)
    (let ((anchor (w::node-screen-rect buffer (w::node-at (w::focus buffer)))))
      (multiple-value-bind (copies effects) (w::handle-ui-event ui (event '(:alt #\e)))
        (declare (ignore copies))
        (let ((result (first (w::new-buffers effects))))
          (is = (tui:rect-x2 anchor) (w::buffer-x result))
          (is = (tui:rect-y anchor) (w::buffer-y result)))))))

(define-test pane-drag-capture-and-release :parent editing
  (let* ((base (ui-for "base"))
         (ui (w::buffer-ui base))
         (pane (w::add-buffer ui :ast (list (parse "42")) :floating t :x 5 :y 3)))
    (render-buffer base :rows 12 :cols 30)
    (setf (slot-value ui 'tui::%screen) (w::make-cell-buffer 12 30))
    (flet ((send (state row col &optional alt)
             (let ((*standard-output* (make-broadcast-stream))
                   (*terminal-io* (make-two-way-stream (make-string-input-stream "")
                                                      (make-broadcast-stream))))
               (tui:dispatch-event
                ui (tui-sys:make-event
                    :altp alt :kind (tui-sys::make-mouse-data
                                    :button :left :state state :row row :col col))))))
      (send :click 4 6 t)
      (is eq pane (w::focused-buffer ui))
      (is eq (w::buffer-id pane) (w::pane-drag-buffer-id (w::pane-drag ui)))
      ;; Motion outside the original pane continues without requiring Alt again.
      (send :drag 7 12)
      (is = 11 (w::buffer-x pane))
      (is = 6 (w::buffer-y pane))
      (send :release 1 1)
      (is eq nil (w::pane-drag ui))
      (send :drag 8 13)
      (is = 11 (w::buffer-x pane))
      (is = 6 (w::buffer-y pane)))))

(define-test adaptive-ref-list-layout :parent editing
  (let* ((buffer (ui-for "a"))
         (loc (w::focus buffer))
         (rect (tui:make-rect :x 0 :y 0 :rows 20 :cols 70)))
    (flet ((spec (&rest forms)
             (w::ref-list-indentation
              (mapcar #'parse forms) (constantly loc) nil buffer rect nil)))
      (is equal '((0 . 3)) (spec "a" "b" "c"))
      (let ((w::*column-limit* 3))
        (is equal '((0 . 2) (0 . 1)) (spec "a" "b" "c")))
      (is equal '((0 . 1)) (spec "a" "\"first
second\"" "c"))
      (is equal '((0 . nil)) (spec "a" "\"first
second\"")))))

(define-test reader-marker-spacing-only-at-head :parent editing
  (let* ((buffer (ui-for "a"))
         (loc (w::focus buffer))
         (marker (make-instance 'parse:reader-marker :name "READ-QUOTE"
                                                     :home-package (find-package :weave-parser)))
         (rect (tui:make-rect :x 0 :y 0 :rows 4 :cols 20)))
    (flet ((positions (elements)
             (let ((positions nil)
                   (tui::*put-buffer* (w::make-cell-buffer 4 20)))
               (w::render-elements
                elements '((0 . nil)) (constantly loc) nil buffer rect nil
                (lambda (node stack context rect &key)
                  (declare (ignore node stack context))
                  (push (tui:rect-x rect) positions)
                  (make-instance 'tui:view :rect (tui:copy-rect rect :rows 1 :cols 1))))
               (nreverse positions))))
      (is equal '(0 1 3) (positions (list marker 'a 'b)))
      (is equal '(0 2 4) (positions (list 'a marker 'b)))
      (dolist (type '(parse::label-ref parse::dot-marker))
        (let ((node (make-instance type :name (if (eq type 'parse::label-ref) "1" ".")
                                        :home-package nil)))
          (is equal '(0 2 4) (positions (list node 'a 'b))))))))

(define-test unary-reader-prefixes :parent editing
  (dolist (symbol '(cl:quote cl:function eclector.reader:quasiquote
                    eclector.reader:unquote eclector.reader:unquote-splicing
                    parse:read-eval))
    (is eq t (not (null (parse:unary-reader-marker-p (parse::wrap-marker symbol))))))
  (is eq nil (parse:unary-reader-marker-p (sym "QUOTE")))
  (is eq nil (parse:unary-reader-marker-p (parse::wrap-marker 'cl:progn))))
