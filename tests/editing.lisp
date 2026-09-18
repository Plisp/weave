(in-package #:weave-tests)

(define-test editing)

;;; rendering

(define-test layouts-render :parent editing
  (is equal "let (a 1 /      b 2) /  f a" (draw (ui-for "(let ((a 1) (b 2)) (f a))")))
  (is equal "let () /  a" (draw (ui-for "(let () a)")))
  (is equal "flet (f (x) /         x) /  f 1" (draw (ui-for "(flet ((f (x) x)) (f 1))")))
  (is equal "symbol-macrolet (s car y) /  s" (draw (ui-for "(symbol-macrolet ((s (car y))) s)")))
  (is equal "' (a (b c) d)" (draw (ui-for "'(a (b c) d)")))
  (is equal "list () /      x" (draw (ui-for "(list () x)")))
  (is equal "progn /  ()" (draw (ui-for "(progn ())"))))

(define-test function-documentation-renders :parent editing
  (let* ((ui (ui-for "(defun g (x) \"first
second\" (declare (type fixnum x)) (+ x 1))"))
         (code (parse:fun-code (first (w::ast ui)))))
    (is = 1 (length (parse:body code)))
    (is equal "\"first
second\"" (parse:str (parse::docstring code)))
    (is equal "defun g /  (x) /  \"first /  second\" /  x + 1" (draw ui)))
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
    (is equal '("(LOAD-TIME-VALUE X _)" ((parse::read-only-p 0)))
        (empty "(load-time-value x t)" '(parse::read-only-p 0)))
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
  (let ((ui (goto (ui-for "(dolist (x xs) wr)") '(parse:body 1))))
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
                (parse:fun-code parse::docstring))
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
          (and (w::lexical-symbol-ref node (w::focus ui))
               (not (w::symbol-ref-boundp node (w::stack ui) ui))))))

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
