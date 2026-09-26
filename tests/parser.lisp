(in-package #:weave-tests)

(define-test parser)

(define-test quasiquote-reader-markers :parent parser
  (flet ((read-syntax (source)
           (eclector.parse-result:read-from-string
            (parse:make-client source) source)))
    (dolist (case '(("'((eclector.reader:quasiquote ()))" nil)
                    ("'(`())" t)))
      (destructuring-bind (source markerp) case
        (let* ((syntax (read-syntax source))
               (form (parse:gen-tree-ref syntax '(1 0)))
               (operator (first (parse:elements form))))
          (is eq 'eclector.reader:quasiquote (parse:resolve operator))
          (is eq markerp (typep operator 'parse:reader-marker))
          (is = 2 (length (parse:elements form)))
          (is equal nil (parse:elements (second (parse:elements form)))))))
    (let* ((syntax (read-syntax "((eclector.reader:quasiquote a #||#))"))
           (form (first (parse:elements syntax)))
           (operator (first (parse:elements form)))
           (argument (second (parse:elements form))))
      (is = 1 (length (parse:elements syntax)))
      (is = 2 (length (parse:elements form)))
      (is eq 'eclector.reader:quasiquote (parse:resolve operator))
      (is eq nil (typep operator 'parse:reader-marker))
      (is equal "A" (parse:name argument))
      (is equal '("#||#") (mapcar #'parse:str (parse::trailing argument))))))

(define-test reader-prefixes-do-not-consume-matching-operands :parent parser
  (dolist (case '(("'quote" (quote quote) (0) t)
                  ("(quote quote)" (quote quote) (0) nil)
                  ("(#||# quote quote)" (quote quote) (0) nil)
                  ("#'function" (function function) (0) t)
                  ("(function function)" (function function) (0) nil)
                  ("`eclector.reader:quasiquote"
                   (eclector.reader:quasiquote eclector.reader:quasiquote) (0) t)
                  ("(eclector.reader:quasiquote eclector.reader:quasiquote)"
                   (eclector.reader:quasiquote eclector.reader:quasiquote) (0) nil)
                  ("`(,eclector.reader:unquote)"
                   (eclector.reader:quasiquote ((eclector.reader:unquote eclector.reader:unquote)))
                   (1 0 0) t)
                  ("`(,@eclector.reader:unquote-splicing)"
                   (eclector.reader:quasiquote
                    ((eclector.reader:unquote-splicing eclector.reader:unquote-splicing)))
                   (1 0 0) t)
                  ("#+(and) 'quote" (quote quote) (0) t)
                  ("#+(and) (quote quote)" (quote quote) (0) nil)
                  ("#+(and) #'function" (function function) (0) t)))
    (destructuring-bind (source expected operator-path markerp) case
      (let ((syntax (eclector.parse-result:read-from-string (parse:make-client source) source)))
        (is equal expected
            (parse::with-temporary-interning (interned)
              (parse::strip-wrappers syntax interned)))
        (is eq markerp (typep (parse:gen-tree-ref syntax operator-path) 'parse:reader-marker)))))
  (let ((ast (parse:parse-from-string (parse:make-client "nil 'quote") :start 4)))
    (is equal "(QUOTE QUOTE)" (sx ast))))

(define-test trivia-pattern-can-match-the-symbol-quote :parent parser
  (dolist (source '("(trivia:match v ((list 'quote x) x))"
                    "(trivia:match v ((list (quote quote) x) x))"))
    (let ((ast (parse source)))
      (is eq 'parse:macro-call (type-of ast))
      (is eq t (parse:expanded ast)))))

;;; parsing and macro analysis

(define-test holes-parse-as-atoms :parent parser
  (is equal 'parse:function-call
            (type-of (parse:parse-syntax (parse:ref-list (sym "LIST") (parse:hole))))))

(define-test parse-from-string-starts-at-index :parent parser
  (let* ((source (format nil "(f)~%; retained~%(defun g () \"documentation\" 1)"))
         (client (parse:make-client source)))
    (multiple-value-bind (first next)
        (parse:parse-from-string client)
      (is equal "(F)" (sx first))
      (multiple-value-bind (second end)
          (parse:parse-from-string client :start next)
        (let ((code (parse:fun-code second)))
          (is equal "\"documentation\"" (parse:str (parse::docstring code)))
          (is equal "1" (parse:str (first (parse:body code)))))
        (is equal "; retained" (parse:str (first (parse::leading second))))
        (is = (length source) end)))))

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
    (is equal '("X") (binder-names entry))))

(defmacro copied-body (name &body body)
  `(let ((,name nil)) ,@(copy-tree body)))

(defmacro copied-evaluated-and-quoted (evaluated quoted)
  `(progn ,(copy-tree evaluated) ',(copy-tree quoted)))

(defmacro copied-separate-scopes (a first-form b second-form)
  `(progn (let ((,a nil)) ,(copy-tree first-form))
          (let ((,b nil)) ,(copy-tree second-form))))

(defmacro duplicated-evaluated (name form reference)
  `(let ((,name nil)) ,form ,(copy-tree form) ,reference))

(defmacro reconstructed-call ((operator value) after)
  (declare (ignore operator))
  `(progn (list ,value) ,after))

(defmacro probe-driver-scopes (name form)
  `(progn
     (let ((,name nil) (probe-driver-hidden nil))
       (probe-driver-helper probe-driver-hidden probe-driver-free)
       ,form)
     (let ((,name nil)) ,(copy-tree form))))

(defun probe-driver-syntax (source)
  (eclector.parse-result:read-from-string (parse:make-client source) source))

(define-test probe-driver-exposes-introduced-code-and-environments :parent parser
  (let* ((call (probe-driver-syntax
                "(weave-tests::probe-driver-scopes x (print x))"))
         (source (parse:gen-tree-ref call '(2)))
         (binder (parse:gen-tree-ref call '(1)))
         (env (parse::env-with-variables parse::+nullenv+
                                        '(probe-driver-outer probe-driver-free)))
         (scope-sizes nil)
         (helper-seen nil)
         (free-seen nil))
    (multiple-value-bind (result expanded)
        (parse::call-with-macro-probes
         call env (constantly t)
         (lambda (raw base-env probes)
           (is = 2 (hash-table-count probes))
           (parse::walk-form
            (parse::env-macroexpand raw base-env) base-env
            (lambda (form subenv)
              (let ((info (and (consp form) (null (cdr form))
                               (gethash (car form) probes))))
                (cond
                  ((and info (eq :exp (parse::probe-info-sort info)))
                   (is eq source (parse::probe-info-source info))
                   (let* ((prefix (ldiff (parse::variable-bindings subenv)
                                         (parse::variable-bindings base-env)))
                          (source-binder (find-if (lambda (entry) (gethash entry probes)) prefix)))
                     (is eq binder (parse::probe-info-source (gethash source-binder probes)))
                     (push (length prefix) scope-sizes)
                     (is eq t (not (null (member 'probe-driver-outer
                                                (parse::variable-bindings subenv))))))
                   nil)
                  (t
                   (when (and (consp form) (eq (car form) 'probe-driver-helper))
                     (setf helper-seen t)
                     (is = 2 (length (ldiff (parse::variable-bindings subenv)
                                            (parse::variable-bindings base-env)))))
                   (when (eq form 'probe-driver-hidden)
                     (is eq form (parse::env-variable-info form subenv)))
                   (when (eq form 'probe-driver-free)
                     (setf free-seen t)
                     (is eq form (parse::env-variable-info form subenv))
                     (is eq nil (member form (ldiff (parse::variable-bindings subenv)
                                                   (parse::variable-bindings base-env)))))
                   t))))
            (constantly nil) (constantly nil))
           :consumed))
      (is eq :consumed result)
      (is eq t expanded))
    (is equal '(1 2) (sort scope-sizes #'<))
    (is eq t helper-seen)
    (is eq t free-seen)
    (is eq 'parse:ref-list (type-of source))
    (is eq 'parse:symbol-ref (type-of binder))))

(define-test probe-driver-rejection-restores-and-descends :parent parser
  (let* ((call (probe-driver-syntax
                "(weave-tests::copied-body x (list (print x)))"))
         (outer (parse:gen-tree-ref call '(2)))
         (inner (parse:gen-tree-ref call '(2 1)))
         (observed nil))
    (multiple-value-bind (result expanded)
        (parse::call-with-macro-probes
         call parse::+nullenv+
         (lambda (info env)
           (declare (ignore env))
           (push (parse::probe-info-source info) observed)
           (not (eq outer (parse::probe-info-source info))))
         (lambda (raw env probes)
           (let ((occurrences 0))
             (is = 3 (hash-table-count probes))
             (parse::walk-form
              (parse::env-macroexpand raw env) env
              (lambda (form subenv)
                (declare (ignore subenv))
                (let ((info (and (consp form) (null (cdr form))
                                 (gethash (car form) probes))))
                  (if info
                      (progn
                        (is eq inner (parse::probe-info-source info))
                        (incf occurrences)
                        nil)
                      t)))
              (constantly nil) (constantly nil))
             occurrences)))
      (is = 1 result)
      (is eq t expanded))
    (is eq t (not (null (member outer observed))))
    (is eq t (not (null (member inner observed))))))

(define-test probe-driver-initial-expansion-failure :parent parser
  (let ((called nil))
    (multiple-value-bind (result expanded)
        (parse::call-with-macro-probes
         (probe-driver-syntax "(weave-tests::copied-body)") parse::+nullenv+
         (lambda (&rest args) (declare (ignore args)) (setf called t))
         (lambda (&rest args) (declare (ignore args)) (setf called t)))
      (is eq nil result)
      (is eq nil expanded)
      (is eq nil called))))

(defmacro hygiene-introduced-inner ()
  `(let ((hygiene-local nil))
     (hygiene-helper hygiene-local hygiene-free)))

(defmacro hygiene-introduced (form reference)
  `(progn ,form ,reference (hygiene-introduced-inner) 'hygiene-quoted))

(defmacro hygiene-binding (form)
  `(let (hygiene-captured) ,form))

(defmacro hygiene-introduced-copy (form)
  `(progn ,form (print hygiene-free)))

(defmacro hygiene-source-binder (name form)
  `(let ((,name nil)) ,name ,form))

(defun test-introduced-forms (call env &key on-introduced)
  (parse::call-with-macro-probes
   call env (constantly t)
   (lambda (raw base-env probes)
     (parse::walk-probed-expansion
      raw base-env probes
      (lambda (info namespace subenv)
        (declare (ignore subenv))
        (not (or (and (eq namespace :exp)
                      (eq (parse::probe-info-sort info) :exp))
                 (and (eq (parse::probe-info-sort info) :ref)
                      (eq namespace (parse::probe-info-namespace info))))))
      (lambda (form subenv) (funcall on-introduced form subenv base-env))))))

(define-test hygiene-hook-walks-introduced-code :parent parser
  (let* ((call (probe-driver-syntax
                "(weave-tests::hygiene-introduced (print source-only) source-ref)"))
         (env (parse::env-with-variables parse::+nullenv+ '(hygiene-free)))
         (seen nil)
         (helper-seen nil)
         (capturable nil))
    (is equal ""
        (with-output-to-string (*standard-output*)
          (multiple-value-bind (complete expanded)
              (test-introduced-forms
               call env
               :on-introduced
               (lambda (form subenv base-env)
                 (push form seen)
                 (when (and (consp form) (eq (car form) 'hygiene-helper))
                   (setf helper-seen t)
                   (is equal '(hygiene-local)
                       (ldiff (parse::variable-bindings subenv)
                              (parse::variable-bindings base-env))))
                 (when (eq form 'hygiene-free)
                   (is eq form (parse::env-variable-info form subenv))
                   (setf capturable
                         (not (member form (ldiff (parse::variable-bindings subenv)
                                                 (parse::variable-bindings base-env))))))
                 t))
            (is eq t complete)
            (is eq t expanded))))
    (is eq t helper-seen)
    (is eq t capturable)
    (is eq t (not (null (member 'hygiene-local seen))))
    (is eq nil (member 'hygiene-quoted seen))
    (is eq nil (find-if (lambda (form)
                         (and (symbolp form)
                              (or (null (symbol-package form))
                                  (member (symbol-name form) '("SOURCE-ONLY" "SOURCE-REF")
                                          :test #'string=))))
                       seen))))

(define-test hygiene-hook-can-prune-introduced-code :parent parser
  (let ((helper-seen nil)
        (free-seen nil))
    (test-introduced-forms
     (probe-driver-syntax "(weave-tests::hygiene-introduced (print source-only) source-ref)")
     (parse::make-env)
     :on-introduced
     (lambda (form subenv base-env)
       (declare (ignore subenv base-env))
       (when (eq form 'hygiene-free) (setf free-seen t))
       (if (and (consp form) (eq (car form) 'hygiene-helper))
           (progn (setf helper-seen t) nil)
           t)))
    (is eq t helper-seen)
    (is eq nil free-seen)))

(define-test hygiene-hook-keeps-introduced-equal-lookalikes :parent parser
  (let ((seen 0))
    (test-introduced-forms
     (probe-driver-syntax
      "(weave-tests::hygiene-introduced-copy (print weave-tests::hygiene-free))")
     (parse::make-env)
     :on-introduced
     (lambda (form subenv base-env)
       (declare (ignore subenv base-env))
       (when (eq form 'hygiene-free) (incf seen))
       t))
    (is = 1 seen)))

(define-test hygiene-binding-check-retains-source-boundaries :parent parser
  (multiple-value-bind (bindings references complete)
      (parse::hygiene-check
       (probe-driver-syntax
        "(weave-tests::hygiene-binding (print weave-tests::hygiene-captured))")
       (parse::make-env))
    (is eq t complete)
    (is equal '((:variable (hygiene-captured))) (mapcar #'cdr bindings))
    (is eq nil references))
  (is equal '(nil nil t)
      (multiple-value-list
       (parse::hygiene-check
        (probe-driver-syntax "(weave-tests::hygiene-source-binder x (print x))")
        (parse::make-env)))))

(define-test probed-expansion-keeps-binder-reference-provenance :parent parser
  (let ((roles nil)
        (introduced-symbols nil))
    (parse::call-with-macro-probes
     (probe-driver-syntax "(weave-tests::hygiene-source-binder x (print x))")
     (parse::make-env) (constantly t)
     (lambda (raw env probes)
       (is eq t
           (parse::walk-probed-expansion
            raw env probes
            (lambda (info namespace subenv)
              (declare (ignore subenv))
              (push (list (parse::probe-info-sort info)
                          (parse::probe-info-namespace info) namespace) roles)
              nil)
            (lambda (form subenv)
              (declare (ignore subenv))
              (when (symbolp form) (push form introduced-symbols))
              t)))))
    (is equal '((:exp nil :exp) (:binder :variable :variable)) roles)
    (is eq nil (find-if (lambda (sym) (null (symbol-package sym))) introduced-symbols))))

(defvar hygiene-unbound-special)
(defparameter hygiene-bound-special :global)
(defvar *hygiene-fixture-expansion*)
(defvar *hygiene-expansion-count*)

(defmacro hygiene-fixture ()
  (copy-tree *hygiene-fixture-expansion*))

(defmacro hygiene-env-marker () nil)

(define-test tagbody-walker-scopes :parent parser
  (let* ((outer-tags (list 'outer))
         (env (parse::make-env :%tags outer-tags))
         (seen nil)
         (binders nil)
         (scopes nil))
    (parse::walk-form
     '(tagbody (go later) later 17 nil
        (tagbody (go later) later 23)
        (go outer))
     env
     (lambda (form subenv)
       (push form seen)
       (when (and (consp form) (eq (car form) 'go))
         (push (parse::tags subenv) scopes)
         (is eq outer-tags (member 'outer (parse::tags subenv))))
       t)
     (lambda (tag sort) (push (list tag sort) binders))
     (constantly nil))
    (is eq nil (some #'atom seen))
    (is equal '((later :tag) (17 :tag) (nil :tag) (later :tag) (23 :tag))
        (nreverse binders))
    (is equal '((later 17 nil outer) (later 23 later 17 nil outer)
                (later 17 nil outer))
        (nreverse scopes))
    (is eq outer-tags (parse::tags env))))

(define-test tagbody-parser-sorts-and-scopes :parent parser
  (let* ((source "(tagbody (weave-tests::hygiene-env-marker) later #x10 () :end t
                   (tagbody (weave-tests::hygiene-env-marker) later 23)
                   (weave-tests::hygiene-env-marker))")
         (syntax (probe-driver-syntax source))
         (labels (subseq (parse:elements syntax) 2 7))
         (scopes nil)
         (ast (parse::parse
               syntax (parse::make-env)
               (lambda (source ast)
                 (declare (ignore source))
                 (when (typep ast 'parse:macro-call)
                   (parse::with-temporary-interning (interned)
                     (push (parse::tags (parse::strip-env (parse::call-env ast) interned))
                           scopes)))
                 ast))))
    (is equal '((later 16 nil :end t) (later 23 later 16 nil :end t)
                (later 16 nil :end t))
        (nreverse scopes))
    (is equal '(parse:eval-form parse:binder parse:binder parse:binder
                parse:binder parse:binder parse:eval-form parse:eval-form)
        (loop for i below (length (parse:body ast))
              collect (parse:location-sort ast (list 'parse:body i))))
    (is eq t (every #'eq labels (subseq (parse:body ast) 1 6)))
    (is eq 'parse:binder (type-of (second (parse:body ast))))
    (is equal (reverse labels)
        (parse::tags (parse:location-env ast '(parse:body 0) (parse:make-env))))
    (is eq nil (parse:location-sort ast '(parse:body -1)))
    (is eq nil (parse:location-sort ast '(parse:body 8)))
    (is eq nil (parse::tagbody-tag-p (make-instance 'parse:literal :str "1.5")))
    (is eq nil (parse::tagbody-tag-p (make-instance 'parse:ref-list :kind :vector)))))

(define-test tagbody-shadowed-tags-in-compiler-environment :parent parser
  (let* ((env (parse::make-env :%tags '(label 1 label 1)
                              :%variable-bindings '((alias 42))))
         (source "(tagbody (symbol-macrolet ((alias 42))
                           (tagbody label 1 (list alias))) label 1)"))
    (is eql 42 (parse::macroexpand-with-env 'alias env))
    (is eq 'parse:tagbody-form (type-of (parse source)))))

(define-test loop-labels-are-not-reference-captures :parent parser
  (multiple-value-bind (bindings references complete)
      (parse::hygiene-check (probe-driver-syntax "(loop repeat 1 do (print t))")
                            (parse::make-env))
    (is eq t complete)
    (is eq nil (remove-if (lambda (entry) (and (consp entry) (eq :function (car entry))))
                         references))
    (is equal '((:block (nil))) (mapcar #'cdr bindings))))

(define-test walker-function-reference-namespaces :parent parser
  (let ((forms nil)
        (references nil))
    (parse::walk-form
     '(symbol-macrolet ((hygiene-helper hidden-variable))
        (progn (hygiene-helper) (function hygiene-helper)
               (function (setf hygiene-helper))
               (quote (hygiene-quoted))
               ((lambda () (hygiene-inner)))))
     (parse::make-env)
     (lambda (form env) (declare (ignore env)) (push form forms) t)
     (constantly nil)
     (lambda (name namespace env)
       (declare (ignore env))
       (push (list namespace name) references)))
    (is equal '((:function hygiene-helper) (:function hygiene-helper)
                (:function (setf hygiene-helper)) (:function hygiene-inner))
        (nreverse references))
    (is eq nil (member 'hygiene-helper forms))
    (is eq nil (member 'hidden-variable forms))
    (is eq nil (member 'hygiene-quoted forms)))
  (let ((references nil))
    (parse::walk-form '(progn (hygiene-helper)) (parse::make-env)
                     (constantly nil) (constantly nil)
                     (lambda (&rest args) (push args references)))
    (is eq nil references)))

(define-test hygiene-function-reference-scopes :parent parser
  (dolist (case '(((hygiene-helper) ((:function hygiene-helper)))
                  ((function hygiene-helper) ((:function hygiene-helper)))
                  ((function (setf hygiene-helper)) ((:function (setf hygiene-helper))))
                  ((quote hygiene-helper) nil)
                  ((let ((hygiene-helper nil)) (hygiene-helper))
                   ((:function hygiene-helper)))
                  ((flet ((hygiene-helper () nil))
                     (hygiene-helper) (function hygiene-helper)) nil)
                  ((flet ((hygiene-helper () (hygiene-helper))) (hygiene-helper))
                   ((:function hygiene-helper)))
                  ((labels ((hygiene-helper () (hygiene-helper))) (hygiene-helper)) nil)
                  ((symbol-macrolet ((hygiene-helper hidden-variable))
                     (function hygiene-helper)) ((:function hygiene-helper)))
                  (((lambda () (hygiene-helper))) ((:function hygiene-helper)))
                  ((function (lambda () (hygiene-helper))) ((:function hygiene-helper)))
                  ((macrolet ((hygiene-macro () '(hygiene-helper))) (hygiene-macro))
                   ((:function hygiene-helper)))))
    (destructuring-bind (*hygiene-fixture-expansion* expected) case
      (multiple-value-bind (bindings references complete)
          (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                               (parse::make-env))
        (is eq t complete)
        (is eq nil bindings)
        (is equal expected references))))
  (let ((*hygiene-fixture-expansion* '(hygiene-helper)))
    (is equal '((:function hygiene-helper))
        (nth-value 1
                   (parse::hygiene-check
                    (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                    (parse::env-with-functions (parse::make-env) '(hygiene-helper))))))
  (let* ((fresh (gensym "HYGIENE-FUNCTION"))
         (*hygiene-fixture-expansion* `(progn (,fresh) (function ,fresh)
                                            (function (setf ,fresh)))))
    (is equal '(nil nil t)
        (multiple-value-list
         (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                              (parse::make-env))))))

(defmacro hygiene-call-function (name) `(,name))
(defmacro hygiene-function-name (name) `(function ,name))
(defmacro hygiene-function-copy (name) `(progn (,name) (hygiene-helper)))
(defmacro hygiene-local-function (name) `(flet ((,name () nil)) (,name)))
(defmacro hygiene-capture-function (name) `(flet ((hygiene-local () nil)) (,name)))

(define-test hygiene-catches-unsubstituted-function-gensym :parent parser
  (let ((fresh (gensym "HYGIENE-LOCAL")))
    (dolist (reference '((hygiene-helper) (function hygiene-helper)))
      (let ((*hygiene-fixture-expansion* `(flet ((,fresh () nil)) ,reference)))
        (is equal '(nil ((:function hygiene-helper)) t)
            (multiple-value-list
             (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                                  (parse::make-env))))))))

(define-test function-reference-hook-does-not-expose-singleton-probes :parent parser
  (let ((references nil))
    (parse::call-with-macro-probes
     (probe-driver-syntax "(weave-tests::copied-body x (print x))")
     (parse::make-env) (constantly t)
     (lambda (raw env probes)
       (parse::walk-probed-expansion
        raw env probes (constantly t) (constantly t)
        (lambda (name namespace subenv)
          (declare (ignore namespace subenv))
          (push name references)))))
    (is eq nil references)))

(define-test hygiene-function-source-provenance :parent parser
  (dolist (source '("(weave-tests::hygiene-call-function weave-tests::hygiene-helper)"
                    "(weave-tests::hygiene-function-name weave-tests::hygiene-helper)"
                    "(weave-tests::hygiene-function-name (setf weave-tests::hygiene-helper))"
                    "(weave-tests::hygiene-local-function weave-tests::hygiene-helper)"))
    (is equal '(nil nil t)
        (multiple-value-list
         (parse::hygiene-check (probe-driver-syntax source) (parse::make-env)))))
  (is equal '(nil ((:function hygiene-helper)) t)
      (multiple-value-list
       (parse::hygiene-check
        (probe-driver-syntax "(weave-tests::hygiene-function-copy weave-tests::hygiene-helper)")
        (parse::make-env))))
  (multiple-value-bind (bindings references complete)
      (parse::hygiene-check
       (probe-driver-syntax "(weave-tests::hygiene-capture-function weave-tests::hygiene-helper)")
       (parse::make-env))
    (is eq t complete)
    (is eq nil references)
    (is equal '((:function (hygiene-local))) (mapcar #'cdr bindings)))
  (let* ((ast (parse "(weave-tests::hygiene-call-function weave-tests::hygiene-helper)"))
         (name (first (parse:body ast))))
    (is eq t (parse:expanded ast))
    (is eq nil (gethash name (parse:subforms ast)))
    (is eq 'parse:unevaluated (parse:location-sort ast '(parse:body 0)))))

(defmacro hygiene-environment-specialness (&environment env)
  (if (eq :special (cl-environments:variable-information 'hygiene-free env))
      nil
      'hygiene-free))

(define-test walker-block-reference-namespaces :parent parser
  (let ((references nil)
        (forms nil))
    (parse::walk-form
     '(block hygiene-outer
        (block nil (return-from nil (return-from hygiene-outer hygiene-value))))
     (parse::make-env)
     (lambda (form env) (declare (ignore env)) (push form forms) t)
     (constantly nil)
     (lambda (name namespace env)
       (push (list namespace name (parse::blocks env)) references)))
    (is equal '((:block nil (nil hygiene-outer))
                (:block hygiene-outer (nil hygiene-outer)))
        (nreverse references))
    (is eq nil (member 'hygiene-outer forms))
    (is eq nil (member nil forms))
    (is eq t (not (null (member 'hygiene-value forms))))))

(define-test walker-defun-implicit-block-reference :parent parser
  (let ((references nil))
    ;; Walk DEFUN directly: a fixture macro returning DEFUN would let SBCL
    ;; fully expand its implementation before reaching our hardwired walker.
    (parse::walk-form
     '(defun hygiene-outer () (return-from hygiene-outer))
     (parse::make-env) (constantly t) (constantly nil)
     (lambda (name namespace env)
       (push (list namespace name (parse::blocks env)) references)))
    (is equal '((:block hygiene-outer (hygiene-outer))) references)))

(define-test hygiene-block-reference-scopes :parent parser
  (dolist (case '(((return-from hygiene-outer) ((:block hygiene-outer)))
                  ((return-from nil) ((:block nil)))
                  ((return-from :exit) ((:block :exit)))
                  ((return-from t) ((:block t)))
                  ((block nil (return-from nil)) nil)
                  ((block hygiene-outer (return-from hygiene-outer)) nil)
                  ((block hygiene-outer
                     (block hygiene-outer (return-from hygiene-outer))) nil)
                  ((progn (block hygiene-outer nil) (return-from hygiene-outer))
                   ((:block hygiene-outer)))
                  ((block hygiene-outer (return-from nil)) ((:block nil)))
                  ((let ((hygiene-outer nil)) (return-from hygiene-outer))
                   ((:block hygiene-outer)))
                  ((symbol-macrolet ((hygiene-outer hidden-reference))
                     (return-from hygiene-outer)) ((:block hygiene-outer)))
                  ((flet ((hygiene-outer () (return-from hygiene-outer))) nil) nil)
                  ((block hygiene-outer (return-from hygiene-outer hygiene-free))
                   (hygiene-free))))
    (destructuring-bind (*hygiene-fixture-expansion* expected) case
      (multiple-value-bind (bindings references complete)
          (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                               (parse::make-env))
        (is eq t complete)
        (is eq nil bindings)
        (is equal expected references))))
  (dolist (name '(hygiene-outer nil))
    (let ((*hygiene-fixture-expansion* `(return-from ,name)))
      (is equal (list nil (list (list :block name)) t)
          (multiple-value-list
           (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                                (parse::env-with-blocks (parse::make-env) (list name)))))))
  (let ((*hygiene-fixture-expansion* `(return-from ,(gensym "HYGIENE-BLOCK"))))
    (is equal '(nil nil t)
        (multiple-value-list
         (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                              (parse::make-env))))))

(defmacro hygiene-return-name (name) `(return-from ,name))
(defmacro hygiene-block-copy (name) `(progn (return-from ,name) (return-from hygiene-outer)))
(defmacro hygiene-source-block (name) `(block ,name (return-from ,name)))
(defmacro hygiene-capture-block (name)
  `(flet ((hygiene-local () nil))
     (let ((hygiene-local nil))
       (block hygiene-outer (return-from ,name)))))

(define-test hygiene-block-source-provenance :parent parser
  (dolist (source '("(weave-tests::hygiene-return-name weave-tests::hygiene-outer)"
                    "(weave-tests::hygiene-source-block weave-tests::hygiene-outer)"))
    (is equal '(nil nil t)
        (multiple-value-list
         (parse::hygiene-check (probe-driver-syntax source) (parse::make-env)))))
  (is equal '(nil ((:block hygiene-outer)) t)
      (multiple-value-list
       (parse::hygiene-check
        (probe-driver-syntax "(weave-tests::hygiene-block-copy weave-tests::hygiene-outer)")
        (parse::make-env))))
  (multiple-value-bind (bindings references complete)
      (parse::hygiene-check
       (probe-driver-syntax "(weave-tests::hygiene-capture-block weave-tests::hygiene-outer)")
       (parse::make-env))
    (is eq t complete)
    (is eq nil references)
    (is equal '((:block (hygiene-outer))) (mapcar #'cdr bindings)))
  (let* ((ast (parse "(weave-tests::hygiene-return-name weave-tests::hygiene-outer)"))
         (name (first (parse:body ast))))
    (is eq t (parse:expanded ast))
    (is eq nil (gethash name (parse:subforms ast)))
    (is eq 'parse:unevaluated (parse:location-sort ast '(parse:body 0)))))

(define-test constant-reference-names-are-not-probed :parent parser
  (dolist (namespace '(:function :block))
    (dolist (name '("nil" "()" "t" ":exit" "cl:pi"))
      (let* ((source (format nil "(weave-tests::~a ~a)"
                             (if (eq namespace :function)
                                 "hygiene-function-name" "hygiene-return-name")
                             name))
             (call (probe-driver-syntax source)))
        (multiple-value-bind (result expanded)
            (parse::call-with-macro-probes
             call (parse::make-env)
             (lambda (&rest args)
               (declare (ignore args))
               (error "Constant name should not have been probed"))
             (lambda (raw env probes)
               (declare (ignore raw env))
               (hash-table-count probes)))
          (is eql 0 result)
          (is eq t expanded))
        ;; Constants remain unattributed: a conservative capture report is expected.
        (is equal (list nil (list (list namespace (read-from-string name))) t)
            (multiple-value-list (parse::hygiene-check call (parse::make-env))))))))

(defmacro hygiene-expansion-error ()
  (error "Deliberate hygiene expansion failure"))

(defmacro hygiene-late-error ()
  (if (= 1 (incf *hygiene-expansion-count*))
      nil
      '(progn hygiene-free (hygiene-expansion-error))))

(define-test hygiene-detects-actual-global-specialness :parent parser
  (let ((symbol '*hygiene-ordinary-global*))
    (unwind-protect
         (progn
           (setf (symbol-value symbol) :global)
           (dolist (case `((,symbol (,symbol))
                           (hygiene-unbound-special nil)
                           (hygiene-bound-special nil)))
             (destructuring-bind (*hygiene-fixture-expansion* expected) case
               (multiple-value-bind (bindings references complete)
                   (parse::hygiene-check
                    (probe-driver-syntax "(weave-tests::hygiene-fixture)") (parse::make-env))
                 (is eq t complete)
                 (is eq nil bindings)
                 (is equal expected references)))))
      (makunbound symbol))))

(define-test local-special-declaration-scopes :parent parser
  (dolist (case
           '(((locally (declare (special hygiene-free)) (hygiene-env-marker)) (t))
             ((locally (declare (special hygiene-free))
                (let ((hygiene-free nil)) (hygiene-env-marker))) (nil))
             ((let ((hygiene-free (hygiene-env-marker)))
                (declare (special hygiene-free)) (hygiene-env-marker)) (nil t))
             ((let* ((hygiene-free nil) (other (hygiene-env-marker)))
                (declare (special hygiene-free)) (hygiene-env-marker)) (t t))
             ((let* ((other (hygiene-env-marker)) (hygiene-free nil))
                (declare (special hygiene-free)) (hygiene-env-marker)) (nil t))
             ((lambda (&optional (other (hygiene-env-marker)))
                (declare (special hygiene-free)) (hygiene-env-marker)) (nil t))
             ((lambda (hygiene-free &optional (other (hygiene-env-marker)))
                (declare (special hygiene-free)) (hygiene-env-marker)) (t t))
             ((lambda (&optional (hygiene-free (hygiene-env-marker)))
                (declare (special hygiene-free)) (hygiene-env-marker)) (nil t))
             ((flet ((local () (declare (special hygiene-free)) (hygiene-env-marker)))
                nil) (t))
             ((flet ((local () (declare (special hygiene-free)) nil))
                (hygiene-env-marker)) (nil))
             ((symbol-macrolet ((hygiene-free 1))
                (locally (declare (special hygiene-free)) (hygiene-env-marker))) (t))))
    (destructuring-bind (form expected) case
      (let ((walked nil)
            (parsed nil))
        (parse::walk-form
         form (parse::make-env)
         (lambda (form env)
           (when (equal form '(hygiene-env-marker))
             (push (parse::env-special-p 'hygiene-free env) walked))
           t)
         (constantly nil) (constantly nil))
        (parse::parse
         (probe-driver-syntax (prin1-to-string form)) (parse::make-env)
         (lambda (source ast)
           (declare (ignore source))
           (when (and (typep ast 'parse:macro-call)
                      (eq (parse:resolve (parse:op ast)) 'hygiene-env-marker))
             (push (parse::env-special-p 'hygiene-free (parse::call-env ast)) parsed))
           ast))
        (is equal expected (nreverse walked))
        (is equal expected (nreverse parsed))))))

(define-test hygiene-uses-local-special-declarations :parent parser
  (dolist (case '(((locally (declare (special hygiene-free)) hygiene-free) nil)
                  ((let ((local hygiene-free))
                     (declare (special hygiene-free)) hygiene-free) (hygiene-free))
                  ((lambda (&optional (local hygiene-free))
                     (declare (special hygiene-free)) hygiene-free) (hygiene-free))
                  ((symbol-macrolet ((hygiene-free hidden-reference))
                     (locally (declare (special hygiene-free)) hygiene-free)) nil)
                  ((locally (declare (special hygiene-free))
                     (hygiene-environment-specialness)) nil)))
    (destructuring-bind (*hygiene-fixture-expansion* expected) case
      (multiple-value-bind (bindings references complete)
          (parse::hygiene-check
           (probe-driver-syntax "(weave-tests::hygiene-fixture)") (parse::make-env))
        (is eq t complete)
        (is eq nil bindings)
        (is equal expected references)))))

(define-test local-special-shadowing-preserves-uninterned-source-names :parent parser
  (let* ((name (symbol-name (gensym "HYGIENE-LOCAL-")))
         (source (format nil "(locally (declare (special weave-tests::~a))
                               (let ((weave-tests::~a nil))
                                 (weave-tests::hygiene-env-marker)))" name name))
         (syntax (probe-driver-syntax source))
         (variable (parse:gen-tree-ref syntax '(1 1 1)))
         (seen nil))
    (unintern (find-symbol name :weave-tests) :weave-tests)
    (is eq nil (find-symbol name :weave-tests))
    (parse::parse
     syntax (parse::make-env)
     (lambda (source ast)
       (declare (ignore source))
       (when (typep ast 'parse:macro-call)
         (push (parse::env-special-p variable (parse::call-env ast)) seen))
       ast))
    (is equal '(nil) seen)
    (is eq nil (find-symbol name :weave-tests))))

(define-test hygiene-failures-are-not-clean-results :parent parser
  (let ((*hygiene-fixture-expansion* nil))
    (is equal '(nil nil t)
        (multiple-value-list
         (parse::hygiene-check
          (probe-driver-syntax "(weave-tests::hygiene-fixture)") (parse::make-env)))))
  (is equal '(nil nil nil)
      (multiple-value-list
       (parse::hygiene-check
        (probe-driver-syntax "(weave-tests::hygiene-expansion-error)") (parse::make-env))))
  (let ((*hygiene-expansion-count* 0))
    (is equal '(nil (hygiene-free) nil)
        (multiple-value-list
         (parse::hygiene-check
          (probe-driver-syntax "(weave-tests::hygiene-late-error)") (parse::make-env)))))
  (let ((*hygiene-fixture-expansion* '(let 123 x)))
    (is equal '(nil nil nil)
        (multiple-value-list
         (parse::hygiene-check
          (probe-driver-syntax "(weave-tests::hygiene-fixture)") (parse::make-env))))))

(define-test repeated-evaluated-probes-keep-binding-maps :parent parser
  (let* ((ast (parse "(weave-tests::duplicated-evaluated x (print x) x)"))
         (binder (first (parse:body ast))))
    (is eq t (parse:expanded ast))
    (is = 2 (hash-table-count (parse:subforms ast)))
    (dolist (source (rest (parse:body ast)))
      (let ((entry (gethash source (parse:subforms ast))))
        (is eq t (hash-table-p (cdr entry)))
        (is equal '("X") (binder-names entry))
        (is equal '(:variable) (gethash binder (cdr entry)))))))

(define-test rejected-compound-probes-restore-descendants :parent parser
  (let* ((ast (parse "(weave-tests::reconstructed-call (list x) (print y))"))
         (source (first (parse:body ast)))
         (reference (parse:gen-tree-ref source '(1)))
         (after (second (parse:body ast))))
    (is eq t (parse:expanded ast))
    (is eq nil (gethash source (parse:subforms ast)))
    (is eq reference (car (gethash reference (parse:subforms ast))))
    (is eq 'parse:function-call (type-of (car (gethash after (parse:subforms ast)))))
    (is = 2 (hash-table-count (parse:subforms ast)))))

(define-test reconstructed-compound-forms :parent parser
  (let* ((ast (parse "(weave-tests::copied-body x (print (list x)))"))
         (form (second (parse:body ast)))
         (entry (gethash form (parse:subforms ast))))
    (is eq t (parse:expanded ast))
    (is eq 'parse:eval-form (parse:location-sort ast '(parse:body 1)))
    (is eq 'parse:function-call (type-of (car entry)))
    (is equal '("X") (binder-names entry))
    (is equal "(PRINT (LIST X))" (sx (car entry)))
    (is equal 1 (hash-table-count (parse:subforms ast)))))

(define-test structural-matches-need-evaluated-probes :parent parser
  (dolist (source '("(weave-tests::copied-evaluated-and-quoted (print x) (print x))"
                    "(weave-tests::copied-evaluated-and-quoted (print 1) (print 1))"))
    (let ((ast (parse source)))
      (is eq t (parse:expanded ast))
      (is eq 'parse:eval-form (parse:location-sort ast '(parse:body 0)))
      (is eq 'parse:unevaluated (parse:location-sort ast '(parse:body 1)))
      (is equal 1 (hash-table-count (parse:subforms ast))))))

(define-test equal-compounds-retain-separate-scopes :parent parser
  (let* ((ast (parse "(weave-tests::copied-separate-scopes x (print x) x (print x))"))
         (first-entry (gethash (second (parse:body ast)) (parse:subforms ast)))
         (second-entry (gethash (fourth (parse:body ast)) (parse:subforms ast))))
    (is eq t (parse:expanded ast))
    (is equal '("X") (binder-names first-entry))
    (is equal '("X") (binder-names second-entry))
    (is equal '(:variable) (gethash (first (parse:body ast)) (cdr first-entry)))
    (is equal '(:variable) (gethash (third (parse:body ast)) (cdr second-entry)))
    (is eq nil (gethash (third (parse:body ast)) (cdr first-entry)))
    (is eq nil (gethash (first (parse:body ast)) (cdr second-entry)))))

(define-test reconstructed-forms-keep-local-macro-environments :parent parser
  (let* ((ast (parse "(macrolet ((m (arg) (list 'print arg)))
                       (weave-tests::copied-body x (m x)))"))
         (call (first (parse:body ast)))
         (entry (gethash (second (parse:body call)) (parse:subforms call))))
    (is eq t (parse:expanded call))
    (is eq 'parse:macro-call (type-of (car entry)))
    (is eq t (parse:expanded (car entry)))
    (is equal '("X") (binder-names entry))))

(defsetf walker-setq-accessor walker-setq-writer)
(define-symbol-macro walker-setq-alias (walker-setq-accessor walker-setq-object))

(defun setq-walk-observations (form)
  (let ((forms nil) (references nil))
    (parse::walk-form
     form (parse::make-env)
     (lambda (form env) (declare (ignore env)) (push form forms) t)
     (constantly nil)
     (lambda (name namespace env)
       (declare (ignore env))
       (push (list namespace name) references)))
    (values (nreverse forms) (nreverse references))))

(define-test setq-targets-are-references-not-evaluated-forms :parent parser
  (multiple-value-bind (forms references)
      (setq-walk-observations '(setq first-target (first-value) second-target (second-value)))
    (is equal '((:variable first-target) (:function first-value)
                (:variable second-target) (:function second-value)) references)
    (is eq nil (member 'first-target forms))
    (is eq nil (member 'second-target forms)))
  (is equal nil (nth-value 1 (setq-walk-observations '(setq))))
  (dolist (form '((setq x) (setq nil 1) (setq (car x) 1)))
    (fail (setq-walk-observations form))))

(define-test setq-symbol-macro-targets-use-setters :parent parser
  (dolist (form '((symbol-macrolet ((alias (walker-setq-accessor walker-setq-object)))
                   (setq alias walker-setq-value))
                  (setq walker-setq-alias walker-setq-value)))
    (multiple-value-bind (forms references) (setq-walk-observations form)
      (is equal '((:function walker-setq-writer)) references)
      (is eq t (not (null (member 'walker-setq-object forms))))
      (is eq t (not (null (member 'walker-setq-value forms))))
      (is eq nil (member 'alias forms))
      (is eq nil (member 'walker-setq-alias forms))))
  (let ((references
          (nth-value 1
                     (setq-walk-observations
                      '(symbol-macrolet ((alias (walker-ordinary-accessor object)))
                         (setq alias 1))))))
    (is eq t (not (null (member '(:function (setf walker-ordinary-accessor)) references
                                :test #'equal))))
    (is eq nil (member '(:function walker-ordinary-accessor) references :test #'equal)))
  (dolist (form '((let ((walker-setq-alias nil)) (setq walker-setq-alias 1))
                  (locally (declare (special walker-setq-alias)) (setq walker-setq-alias 1))))
    (is equal '((:variable walker-setq-alias))
        (nth-value 1 (setq-walk-observations form))))
  (is equal '((:variable cell))
      (nth-value 1
                 (setq-walk-observations
                  '(symbol-macrolet ((alias cell)) (setq alias 1))))))

(defmacro hygiene-setq-target (name) `(setq ,name 1))

(define-test setq-write-references-retain-hygiene-analysis :parent parser
  (dolist (case '(((setq hygiene-free 1) (hygiene-free))
                  ((let ((hygiene-free nil)) (setq hygiene-free 1)) nil)
                  ((locally (declare (special hygiene-free)) (setq hygiene-free 1)) nil)
                  ((symbol-macrolet ((alias (walker-setq-accessor walker-setq-object)))
                     (setq alias 1)) (walker-setq-object (:function walker-setq-writer)))))
    (destructuring-bind (*hygiene-fixture-expansion* expected) case
      (multiple-value-bind (bindings references complete)
          (parse::hygiene-check (probe-driver-syntax "(weave-tests::hygiene-fixture)")
                               (parse::make-env))
        (is eq t complete)
        (is eq nil bindings)
        (is equal expected references))))
  (is equal '(nil nil t)
      (multiple-value-list
       (parse::hygiene-check
        (probe-driver-syntax "(weave-tests::hygiene-setq-target weave-tests::hygiene-free)")
        (parse::make-env))))
  (let* ((ast (parse "(weave-tests::hygiene-setq-target weave-tests::hygiene-free)"))
         (target (first (parse:body ast))))
    (is eq t (parse:expanded ast))
    (is eq target (car (gethash target (parse:subforms ast))))))

(defvar *walker-binder-object*)
(defmacro walker-returns-binder () *walker-binder-object*)

(define-test walker-rejects-binder-objects-before-on-form :parent parser
  (let ((*walker-binder-object* (make-instance 'parse:binder :name "BINDER")))
    (dolist (form (list *walker-binder-object* '(walker-returns-binder)))
      (let ((binder-seen nil))
        (is eq :caught
            (handler-case
                (parse::with-suppressed-parse-errors
                  (parse::walk-form
                   form (parse::make-env)
                   (lambda (form env)
                     (declare (ignore env))
                     (when (typep form 'parse:binder) (setf binder-seen t))
                     t)
                   (constantly nil) (constantly nil)))
              (parse::analysis-invariant-error () :caught)))
        (is eq nil binder-seen))))
  ;; A symbol used at both a binding and a reference occurrence remains valid.
  (is eq t (not (null (member 'same (setq-walk-observations '(let ((same nil)) same)))))))

(define-symbol-macro walker-global-alias (global-payload global-value))

(define-test walker-follows-symbol-macros :parent parser
  (flet ((seen (form &optional stop)
           (let (forms)
             (parse::walk-form form parse::+nullenv+
                               (lambda (form env)
                                 (declare (ignore env))
                                 (push form forms)
                                 (not (equal form stop)))
                               (constantly nil) (constantly nil))
             forms)))
    (let ((forms (seen '(symbol-macrolet ((alias (payload value))) alias))))
      (is equal t (not (null (member '(payload value) forms :test #'equal))))
      (is equal t (not (null (member 'value forms)))))
    (let ((forms (seen '(symbol-macrolet ((a b) (b (payload value))) a))))
      (is equal t (not (null (member '(payload value) forms :test #'equal)))))
    (let ((forms (seen '(symbol-macrolet ((alias (payload value))) 'alias))))
      (is equal nil (member '(payload value) forms :test #'equal)))
    (let ((forms (seen '(symbol-macrolet ((alias (payload value))) nil))))
      (is equal nil (member '(payload value) forms :test #'equal)))
    (let ((forms (seen '(symbol-macrolet ((alias (payload value)))
                         (let ((alias nil)) alias)))))
      (is equal nil (member '(payload value) forms :test #'equal)))
    (let ((forms (seen '(symbol-macrolet ((alias (payload value))) alias) 'alias)))
      (is equal nil (member '(payload value) forms :test #'equal)))
    (let ((forms (seen 'walker-global-alias)))
      (is equal t (not (null (member '(global-payload global-value) forms :test #'equal)))))
    (let ((forms (seen '(let ((walker-global-alias nil)) walker-global-alias))))
      (is equal nil (member '(global-payload global-value) forms :test #'equal)))
    (let ((forms (seen '(symbol-macrolet ((walker-global-alias nil)) walker-global-alias))))
      (is equal t (not (null (member nil forms))))
      (is equal nil (member '(global-payload global-value) forms :test #'equal)))))

(defmacro symbol-macro-body (name form)
  (let ((alias (gensym "ALIAS")))
    `(let ((,name nil))
       (symbol-macrolet ((,alias ,form)) ,alias))))

(define-test symbol-macro-expansions-keep-binding-environments :parent parser
  (let* ((ast (parse "(weave-tests::symbol-macro-body x (print x))"))
         (entry (gethash (second (parse:body ast)) (parse:subforms ast))))
    (is eq t (parse:expanded ast))
    (is eq 'parse:eval-form (parse:location-sort ast '(parse:body 1)))
    (is eq 'parse:function-call (type-of (car entry)))
    (is equal '("X") (binder-names entry))
    (is equal '(:variable) (gethash (first (parse:body ast)) (cdr entry)))))

(define-test trivia-matched-inputs-are-evaluated :parent parser
  (dolist (source '("(trivia:match (source value) ((list x) x) (_ nil))"
                    "(trivia:ematch (source value) ((list x) x))"
                    "(trivia:cmatch (source value) ((list x) x))"))
    (let* ((ast (parse source))
           (entry (gethash (first (parse:body ast)) (parse:subforms ast))))
      (is eq t (parse:expanded ast))
      (is eq 'parse:eval-form (parse:location-sort ast '(parse:body 0)))
      (is eq 'parse:function-call (type-of (car entry)))
      (is equal "(SOURCE VALUE)" (sx (car entry)))))
  (let* ((ast (parse "(trivia:match* ((source value) other) (((list x) _) x))"))
         (entry (gethash (parse:gen-tree-ref (parse:body ast) '(0 0)) (parse:subforms ast))))
    (is eq t (parse:expanded ast))
    (is eq 'parse:eval-form (parse:location-sort ast '(parse:body 0 0)))
    (is eq 'parse:function-call (type-of (car entry)))
    (is equal "(SOURCE VALUE)" (sx (car entry)))))

(define-test hole-argument-keeps-analysis :parent parser
  (let* ((ast (parse "(loop for x in (xs a) do (print x))"))
         (edited (parse:update ast '(parse:body 3 1) (parse:hole))))
    (is equal (hash-table-count (parse:subforms ast))
              (hash-table-count (parse:subforms edited)))))

(define-test hole-binder-is-skipped :parent parser
  (let ((edited (parse:update (parse "(loop for x in xs for y in ys do (print y))")
                              '(parse:body 1) (parse:hole))))
    (is equal '("Y")
              (binder-names (gethash (parse:gen-tree-ref (parse:body edited) '(9))
                                     (parse:subforms edited))))))

(define-test macro-argument-edits-survive-copying :parent parser
  (let* ((call (parse "(when xs wr)"))
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

(defun call-with-unresolved-function-source (source check)
  (let* ((package (make-package (symbol-name (gensym "FUNCTION-SOURCE-")) :use '(:cl)))
         (*package* package))
    (unwind-protect
         (let ((syntax (probe-driver-syntax source)))
           (dolist (name '("M" "N" "OTHER"))
             (when (find-symbol name package)
               (unintern (find-symbol name package) package)))
           (funcall check (parse::parse syntax (parse::make-env)
                                       (lambda (source ast) (declare (ignore source)) ast)))
           (dolist (name '("M" "N" "OTHER"))
             (is eq nil (find-symbol name package))))
      (delete-package package))))

(define-test unresolved-local-macros-have-distinct-names :parent parser
  (call-with-unresolved-function-source
   "(macrolet ((m (x) x) (n (x) (list 'quote x)))
      (m (print 1)) (n (print 2)) (other (print 3)))"
   (lambda (ast)
     (destructuring-bind (m n other) (parse:body ast)
       (is eq 'parse:macro-call (type-of m))
       (is eq 'parse:macro-call (type-of n))
       (is eq t (parse:expanded m))
       (is eq t (parse:expanded n))
       (is = 1 (hash-table-count (parse:subforms m)))
       (is = 0 (hash-table-count (parse:subforms n)))
       (is eq 'parse:function-call (type-of other))))))

(define-test unresolved-local-functions-shadow-only-their-own-name :parent parser
  (dolist (operator '(flet labels))
    (call-with-unresolved-function-source
     (format nil "(macrolet ((m (x) x) (n (x) x))
                    (~a ((m () (m (print 1))))
                      (m) (n (print 2)) (other)
                      (weave-tests::hygiene-env-marker)))" operator)
     (lambda (ast)
       (let* ((local (first (parse:body ast)))
              (definition (first (parse:elements (parse:funs local))))
              (code (second (parse:elements definition)))
              (recursive-call (first (parse:body code))))
         (is eq (if (eq operator 'labels) 'parse:function-call 'parse:macro-call)
             (type-of recursive-call))
         (destructuring-bind (m n other marker) (parse:body local)
           (is eq 'parse:function-call (type-of m))
           (is eq 'parse:macro-call (type-of n))
           (is eq t (parse:expanded n))
           (is = 1 (hash-table-count (parse:subforms n)))
           (is eq 'parse:function-call (type-of other))
           (let* ((env (parse::call-env marker))
                  (entry (parse::env-function-info (parse:name m) env)))
             (is eq t (typep entry 'parse:symbol-ref))
             (is string= "M" (parse:name entry))
             (is eq nil (parse::env-function-info (parse:name other) env)))))))))

(define-test implicit-function-blocks-exclude-lambda-list-initializers :parent parser
  (dolist (operator '(defun defmacro defmethod))
    (let* ((form `(block hygiene-outer
                    (,operator hygiene-helper
                        (&optional (x (hygiene-env-marker)))
                      (hygiene-env-marker))))
           (expected '(("HYGIENE-OUTER") ("HYGIENE-HELPER" "HYGIENE-OUTER")))
           (walked nil)
           (parsed nil))
      (parse::walk-form
       form (parse::make-env)
       (lambda (form env)
         (when (equal form '(hygiene-env-marker))
           (push (mapcar #'sx (parse::blocks env)) walked))
         t)
       (constantly nil) (constantly nil))
      (parse::parse
       (probe-driver-syntax (prin1-to-string form)) (parse::make-env)
       (lambda (source ast)
         (declare (ignore source))
         (when (and (typep ast 'parse:macro-call)
                    (eq 'hygiene-env-marker (parse:resolve (parse:op ast))))
           (push (mapcar #'sx (parse::blocks (parse::call-env ast))) parsed))
         ast))
      (is equal expected (nreverse walked))
      (is equal expected (nreverse parsed)))))

(define-test function-environment-matches-wrappers-and-symbols :parent parser
  (let* ((wrapper (sym "HYGIENE-HELPER" "WEAVE-TESTS"))
         (env (parse::env-with-functions (parse::make-env) (list wrapper))))
    (is eq wrapper (parse::env-function-info 'hygiene-helper env))
    (is eq wrapper (parse::env-function-info (sym "HYGIENE-HELPER" "WEAVE-TESTS") env))
    (is eq nil (parse::env-function-info (sym "HYGIENE-HELPER" "CL-USER") env))
    (is eq 'hygiene-helper
        (parse::env-function-info wrapper
                                 (parse::env-with-functions (parse::make-env)
                                                           '(hygiene-helper))))))

(define-test macro-call-envmap-failure-return-values :parent parser
  (dolist (source '("(weave-tests::hygiene-expansion-error)"
                    nil))
    (let ((results
            (multiple-value-list
             (parse::macro-call-envmap
              (if source (probe-driver-syntax source)
                  (make-instance 'parse:literal :str "("))
              (parse:make-env)
              (lambda (form env)
                (declare (ignore env))
                form)))))
      (is = 3 (length results))
      (is eq t (hash-table-p (first results)))
      (is eq t (hash-table-p (second results)))
      (is eq nil (third results)))))

(define-test invariants-escape-error-handlers :parent parser
  (fail (parse::invariant nil) 'parse::analysis-invariant-error)
  (is eq :escaped
         (handler-case (handler-case (parse::invariant nil)
                         ((and error (not parse::analysis-invariant-error)) () :caught))
           (parse::analysis-invariant-error () :escaped))))

(define-test copied-macro-calls-own-their-binders :parent parser
  (let* ((call (parse "(dotimes (i 10) (print i))"))
         (copy (parse:copy-node call))
         (binder (parse:env-lookup (parse:gen-tree-ref (parse:body copy) '(0 0)) :variable
                                  (parse:location-env copy '(parse:body 1) (parse:make-env)))))
    (is eq (parse:gen-tree-ref (parse:body copy) '(0 0)) binder)
    (is eq nil (eq binder (parse:gen-tree-ref (parse:body call) '(0 0))))))

(define-test hole-binder-binds-nothing :parent parser
  (let ((edited (parse:update (parse "(dolist (x xs) (print x))") '(parse:body 0 0) (parse:hole))))
    (is equal nil (binder-names (gethash (nth 1 (parse:body edited))
                                         (parse:subforms edited))))))

(define-test unparseable-calls-drop-binders :parent parser
  (let* ((call (parse:update (parse "(dolist (x xs) (print x))") 'parse:op (parse:hole)))
         (entry (gethash (nth 1 (parse:body call)) (parse:subforms call))))
    (is equal 'parse:function-call (type-of (car entry)))
    (is equal nil (cdr entry))
    (is equal nil (parse::variable-bindings
                   (parse:location-env call '(parse:body 1) (parse:make-env))))))

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
      (is equal '("I") (binder-names (gethash (nth 7 (parse:body restored))
                                              (parse:subforms restored))))))
  (is equal nil (parse::expanded (parse "(loop for i from 1 to 10 (print i))"))))

(define-test function-call-renames-stay-calls :parent parser
  (is equal 'parse:function-call
            (type-of (parse:update (parse "(dolis (x xs) (print x))") 'parse:name (sym "DOLIST")))))

(define-test lambda-expressions-are-walked :parent parser
  (flet ((binders (source)
           (let ((ast (parse source)))
             (binder-names (gethash (car (last (parse:body ast))) (parse:subforms ast))))))
    (is equal '("P" "Q") (binders "(multiple-value-bind (p q) (f x) (list p q))"))
    (is equal '("A" "B" "BP")
              (binders "(destructuring-bind (a &optional (b 1 bp)) l (list a b bp))")))
  (let ((ast (parse "(destructuring-bind (a &optional (b 1 bp)) l (list a b bp))")))
    (dolist (path '((0 0) (0 2 0) (0 2 2)))
      (is equal 'parse:binder (parse:location-sort ast (cons 'parse:body path))))))

(define-test function-designator-lambdas-parse :parent parser
  (let ((ast (parse "#'(lambda (x) x)")))
    (is equal 'parse::lambda-form (type-of (parse:fun-designator ast)))
    (is equal "(FUNCTION (LAMBDA (X) X))" (sx ast)))
  (is equal 'parse:symbol-ref (type-of (parse:fun-designator (parse "(function car)")))))

(define-test dotted-tails-map-to-paths :parent parser
  (flet ((binders (source)
           (let ((ast (parse source)))
             (binder-names (gethash (car (last (parse:body ast))) (parse:subforms ast))))))
    (is equal '("A" "B" "C") (binders "(destructuring-bind (a . (b c)) l (list a b c))"))
    (is equal '("A" "B") (binders "(destructuring-bind (a . b) l (list a b))"))
    (is equal '("A" "B") (binders "(loop for (a . b) in xs collect a)"))))

(define-test binding-kinds :parent parser
  (flet ((kinds (source)
           (let ((ast (parse source)))
             (binder-kinds (gethash (car (last (parse:body ast))) (parse:subforms ast))))))
    (is equal '(("X" :variable)) (kinds "(dolist (x xs) (print x))"))
    (is equal '(("NEXT" :function)) (kinds "(with-hash-table-iterator (next h) (next))"))
    (is equal '(("IT" :function)) (kinds "(with-package-iterator (it p :internal) (it))"))
    (is equal '(("OUTER" :block) ("X" :variable))
              (kinds "(loop named outer for x in xs do (return-from outer x))"))
    (is equal '(("P" :variable) ("Q" :variable))
              (kinds "(multiple-value-bind (p q) (f) (list p q))"))
    (is equal '(("F" :function :variable)) (kinds "(var-and-fn f (f))"))
    (is equal '(("X" :variable)) (kinds "(shadowing-vars x x (print x))")))
  ;; the first visible binder of a name is saved
  (let* ((ast (parse "(shadowing-vars x x (print x))"))
         (binders (cdr (gethash (car (last (parse:body ast))) (parse:subforms ast)))))
    (is equal nil (gethash (parse:gen-tree-ref (parse:body ast) '(0)) binders))
    (is equal '(:variable) (gethash (parse:gen-tree-ref (parse:body ast) '(1)) binders))))
