;;;;
;;;; incremental lisp parsing and type inference
;;;; TODO test anaphoric macros, cffi, quasiquote: with-gensyms, once-only
;;;;

(defpackage :infrared
  (:use :cl :alexandria-2)
  (:local-nicknames (#:read :eclector.parse-result)
                    (#:env  :cl-environments) ; function-information
                    (#:sly  :slynk-backend)) ; arglist
  (:export))
(in-package :infrared)

(defmacro disp (form)
  (once-only ((res form))
    `(progn
       (format t "~s~%|> ~a~%" ',form ,res)
       ,res)))

(defun addr-str (obj)
  (let* ((str (delete-if (lambda (c) (member c '(#\# #\< #\> #\Space #\{ #\})))
                         (with-output-to-string (s)
                           (print-unreadable-object (obj s :identity t)))))
         (length (length str)))
    (if (>= length 3)
        (subseq str (- length 3))
        str)))

;;
;;; eclector reader
;;

(defvar *literal-magic* (gensym)) ; unique

(defclass my-client (eclector.parse-result:parse-result-client)
  ((source :initarg :source
           :initform (error "no source"))))

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result t) (children t) (source t))
  (cond ((and (atom result) (constantp result))
         (list *literal-magic* (subseq (slot-value client 'source)
                                       (car source) (cdr source))))
        ((null children) result)
        (t
         (if (consp result)
             (case (car result)
               ;; readtable should stop read-macro representations from being used?
               ;; otherwise I could use gensyms like *literal-magic*
               (function (if (eq 'function (first children))
                             children
                             `(read-function ,@children)))
               (quote
                (if (eq 'quote (first children))
                    children
                    `(read-quote ,@children)))
               (read-eval `(read-eval ,@children))
               (eclector.reader:quasiquote `(read-quasiquote ,@children))
               (eclector.reader:unquote `(read-unquote ,@children))
               (t children))
             children))))

;; TODO handle comments (including formatting) and reader conditionals properly
(defmethod eclector.parse-result:make-skipped-input-result
    ((client my-client) (stream t) (reason t) (children t) (source t))
  (list :reason reason :source source :children children))

(defmethod eclector.reader:evaluate-expression ((client my-client) (expression t))
  (list 'read-eval expression))

(defmethod eclector.reader:fixup ((client my-client) obj state)
  (error "TODO handle circular lists using fixup"))

;;
;;; class defs: mainly we want a structure that's
;;; - close enough to s-expressions for macroexpansion and evaluation
;;; - gives identity to semantic units like comments and identifiers
;;;   which may need bespoke display methods
;;;   - binders derive their identity from their enclosing irregular form
;;;     XXX can duplicate bindings in let*, though this is bad practice anyways
;;; - caches information on binders and evaluation contexts (needed for
;;;   completion and basic analysis) so macroexpansions can be lexically
;;;   limited during interactive editing
;;;   - make sure editing an AST node does not affect lexical bindings outside
;;; note: a form's 'parent' isn't meaningful in a macroexpansion, and prevents
;;; using the datatype immutably e.g. for slow analysis on a different thread
;;

(defclass comment ()
  ((str :initarg :str
        :accessor str)
   (kind :initarg :kind
         :initform :line :type (or :line :block)
         :accessor kind))
  (:documentation ""))

(defclass eval-form ()
  ()
  (:documentation "Form in an evaluation context, perhaps quoted."))

(defclass literal-form (eval-form)
  ((form :initarg :form
         :type string :initform (error "literal not provided")
         :accessor form))
  (:documentation "Atomic literal"))

(defclass symbol-ref (eval-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :accessor name))
  (:documentation "Represents a symbol or symbol macro?"))

(defclass function-call (eval-form)
  ((name :initarg :name
         :initform (error "must provide function name")
         :accessor name)
   (args :initarg :args
         :initform (error "must provide function argument list")
         :accessor args))
  (:documentation "args is a list of eval-forms"))

(defclass irregular-form (eval-form)
  ((name :initarg :name
         :accessor name))
  (:documentation "macro invocation or special operator"))

(defclass function-code ()
  ((lambda-list :initarg :lambda-list
                :accessor lambda-list)
   (docstring :initarg :docstring
              :accessor docstring
              :type string)
   (declarations :initarg :declarations
                 :accessor declarations)
   (body :initarg :body
         :accessor body))
  (:documentation "(macro) lambda list and body list of eval-forms"))

(defmethod print-object ((object literal-form) stream)
  (princ (form object) stream))

(defmethod print-object ((object symbol-ref) stream)
  (pprint-logical-block (stream (list))
    (format stream "<~a@~a>" (name object) (addr-str object))))

(defmethod print-object ((object function-call) stream)
  (pprint-logical-block (stream (args object) :suffix ")")
    (write-char #\( stream)
    (write (name object) :stream stream)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (print-object (pprint-pop) stream))))

(defmethod print-object ((object function-call) stream)
  (pprint-logical-block (stream (args object))
    (write-char #\( stream)
    (write (name object) :stream stream)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (print-object (pprint-pop) stream)))
  (write-string ")" stream))

;;
;;; interface
;;

(defgeneric to-text (ast) (:documentation "Serialize ast to text"))
(defgeneric to-sexp (ast) (:documentation "convert to sexp for macroexpansion")
  (:method ((ast t)) nil))

;;
;;; code walking
;;

(defun macrolet-code-wrap (name rest form)
  `(macrolet ((,name ,@rest))
     ,form))

(defun symbol-macrolet-wrap (name expansion form)
  `(symbol-macrolet ((,name ,expansion))
     ,form))

(defun flet-wrap (name form)
  `(flet ((,name (&rest args) (declare (ignorable args))))
     ,form))

(defun let-wrap (name form)
  "some globals don't like to ever be bound to nil, so try a global value first"
  `(let ((,name ,(if (boundp name) name nil)))
     (declare (ignorable ,name))
     ,form))

(defstruct (env (:conc-name nil))
  "variable-bindings: (v &optional macroexpansion)
function-bindings: (f &optional rest-of-macrolet)
copy-env can exploit structure sharing, remember to PUSH!"
  (%function-bindings (list))
  (%variable-bindings (list))
  (%blocks (list))
  (%tags (list)))

(defmethod function-bindings ((env env)) (%function-bindings env))
(defmethod variable-bindings ((env env)) (%variable-bindings env))
(defmethod blocks ((env env)) (%blocks env))
(defmethod tags ((env env)) (%tags env))

(defun env-variable-info (name env)
  (find name (variable-bindings env) :key #'first))
(defun env-function-info (name env)
  (find name (function-bindings env) :key #'first))

(defun env-with-variables (env bindings)
  (let ((new-env (copy-env env)))
    (dolist (v bindings)
      (push (list v) (variable-bindings new-env)))
    new-env))

(defun env-with-functions (env bindings)
  (let ((new-env (copy-env env)))
    (dolist (v bindings)
      (push (list v) (function-bindings new-env)))
    new-env))

(defun wrap-with-wrapper (form entries wrapper)
  (if (null entries)
      form
      (wrap-with-wrapper (funcall wrapper form (first entries))
                         (cdr entries) wrapper)))

(defun wrap-function-like-env (form entries)
  (wrap-with-wrapper form entries
                     (lambda (form entry)
                       (if (null (cdr entry))
                           (flet-wrap (first entry) form)
                           (macrolet-code-wrap (first entry) (cdr entry) form)))))

(defun wrap-variable-like-env (form entries)
  (wrap-with-wrapper form entries
                     (lambda (form entry)
                       (if (null (second entry))
                           (let-wrap (first entry) form)
                           (symbol-macrolet-wrap (first entry) (second entry) form)))))

(defun wrap-block-env (form entries)
  (wrap-with-wrapper form entries
                     (lambda (form entry)
                       `(block ,entry ,form))))

(defun wrap-tag-env (form tags)
  (if (null tags)
      form
      (let ((s (gensym)))
        `(catch ',s
           (tagbody
              ,@tags
              (throw ',s ,form))))))

(defun go-tag-p (x) (or (integerp x) (symbolp x)))

(defmacro macroexpand-in-lispenv (form &environment env)
  `(macroexpand ',form ,env))

(defun macroexpand-with-env (form env)
  (eval (line-up-first `(macroexpand-in-lispenv ,form)
                       (wrap-function-like-env (function-bindings env))
                       (wrap-variable-like-env (variable-bindings env))
                       (wrap-block-env (blocks env))
                       (wrap-tag-env (tags env)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type simple-vector *hardwired-operators*))
  (defparameter *hardwired-operators* #(*literal-magic* lambda defun defmethod defmacro)
    "The list of nonportable hardwired macros, not macroexpanded"))

(defun hardwired-p (macro-name)
  (find macro-name *hardwired-operators*))

(defmethod env-macroexpand-1 (x env)
  "Tries macroexpanding a non-hardwired form x in evaluation position, once"
  (cond ((symbolp x)
         (if-let (local-expansion (second (env-variable-info x env)))
           (values local-expansion t)
           (macroexpand-1 x))) ; possible global macro, or function call
        ((consp x)
         (assert (not (hardwired-p (first x))))
         (let* ((lexical-info (env-function-info (first x) env))
                (local-expansion (cdr lexical-info)))
           (cond ((null lexical-info) ; global
                  (cond ((consp (first x)) x) ; lambda in head position
                        ((macro-function (first x)) (macroexpand-with-env x env))
                        (t nil)))
                 ((null local-expansion) x) ; flet/labels bound
                 (t (macroexpand-with-env x env))))) ; local macro
        (t x)))

(defun env-macroexpand (form env)
  "Perform full macroexpansion of the head of form"
  (loop with any-expanded-p
        for (newform expanded-p) := (multiple-value-list (env-macroexpand-1 form env))
        until (and (not expanded-p) (eq newform form))
        do (setf form newform
                 any-expanded-p (or any-expanded-p expanded-p))
        finally (return (values form any-expanded-p))))

;;
;;; ast conversion
;;

(define-condition form-parse-error (simple-error)
  ())

(defun form-parse-error (control &rest args)
  (error 'form-parse-error
         :format-control (concatenate 'string "parse failed: " control)
         :format-arguments args))

;; structure is that of a macro call, with interspersed forms
;; variables are bound like a let*
(defun lambda-list-variable-names (list)
  "Requires a list. Doesn't require well-formedness but may throw form-parse-error,
we need lambda lists to have some symbols for scoping."
  ;; parse one at a time
  (loop for name in list
        unless (find name lambda-list-keywords)
          collect (handler-case
                      (trivia:ematch name
                        ((type symbol) name)
                        ((list* (and (type symbol) x) _) x) ; methods and defaults
                        ((list* (list _ (and (type symbol) x)) _) x))
                    (trivia:match-error ()
                      (form-parse-error "lambda list match")))
        when (trivia:match name
               ((list _ _ supplied-p)
                (if (symbolp supplied-p)
                    supplied-p
                    (form-parse-error "lambda list supplied-p match"))))
          collect it))
;; not checked here, we use it for method lambda lists too
(assert (equal (lambda-list-variable-names '((a t))) '(A)))
(assert (equal (lambda-list-variable-names '(a a)) '(A A))) ; XXX are duplicates fine?
(assert (equal (lambda-list-variable-names '(a &key ((:weave name) default supplied-p)))
               '(A NAME SUPPLIED-P)))
(assert (not (handler-case
                 (lambda-list-variable-names '(a &key (name default (supplied-p))))
               (form-parse-error ()))))
(assert (not (handler-case
                 (lambda-list-variable-names '(a &key ((:weave (name)))))
               (form-parse-error ()))))

(defun parse-body-declarations (entries enable-docstring)
  "Requires a list. Doesn't require well-formedness.
Splits entries into a prefix of (optional docstring) declarations and forms for parsing."
  (let ((maybe-docstring nil))
    (when (and enable-docstring (stringp (first entries)))
      (setf maybe-docstring (pop entries)))
    ;; scan for declarations, then check if the docstring was the only thing
    (loop with declarations := ()
          for entry = (pop entries)
          for operator := (and (consp entry) (car entry))
          while entry
          do (if (eq operator 'declare)
                 (push entry declarations)
                 (progn (push entry entries) (loop-finish)))
          finally (if (and (null entries) (null declarations) maybe-docstring)
                      (return (list nil (list maybe-docstring)))
                      (return (if maybe-docstring
                                  (list (cons maybe-docstring (nreverse declarations))
                                        entries)
                                  (list (nreverse declarations) entries)))))))
;; crucial exception
(assert (equal (parse-body-declarations '("doc") t) '(NIL ("doc"))))
(assert (equal (parse-body-declarations '("doc" (declare (ignore a))) t)
               '(("doc" (DECLARE (IGNORE A))) NIL)))
(assert (equal (parse-body-declarations '("doc" (declare (ignore a)) 3) t)
               '(("doc" (DECLARE (IGNORE A))) (3))))
(assert (equal (parse-body-declarations '("doc" (declare (ignore a))) nil)
               '(NIL ("doc" (DECLARE (IGNORE A)))))) ; programmer error

(defun canonicalize-bindings (bindings)
  (loop for b in bindings
        collect (if (symbolp b) `(,b nil) b)))

(assert (equal (canonicalize-bindings '(x (y 0))) '((X NIL) (Y 0))))

;;
;;; special operators
;;

;; hardcoded printers
(defclass let*-form (irregular-form)
  ((op :initform 'let*)
   (lexenv :initarg :lexenv
           :initform (error "must provide let lexenv")
           :accessor lexenv)
   (bindings :initarg :bindings
             :initform (list)
             :accessor bindings
             :documentation "(binder . nil-or-eval-form)")
   (body :initarg :body
         :accessor body
         :type list))
  (:documentation ""))

(defclass let-form (let*-form)
  ((op :initform 'let)))

(defmethod print-object ((object let*-form) stream)
  (pprint-logical-block (stream (list))
    (pprint-logical-block (stream (bindings object))
      (write (op object) :stream stream)
      (pprint-indent :current -2 stream)
      (loop
        (pprint-exit-if-list-exhausted)
        (pprint-newline :mandatory stream)
        (write-char #\space stream)
        (destructuring-bind (binding . form)
            (pprint-pop)
          (print-object binding stream)
          (write-string " = " stream)
          (print-object form stream))))
    (pprint-newline :fill stream)
    (write-string "in " stream)
    (print-object (body object) stream)))

;; return-from
;; catch load-time-value setq
;; eval-when locally
;; tagbody
;; function multiple-value-call the
;; go multiple-value-prog1 throw
;; if progn unwind-protect
;; progv
;; quote

(defclass let*-form (irregular-form)
  ((op :initform 'let*)
   (lexenv :initarg :lexenv
           :initform (error "must provide let lexenv")
           :accessor lexenv)
   (bindings :initarg :bindings
             :initform (list)
             :accessor bindings
             :documentation "(binder . nil-or-eval-form)")
   (body :initarg :body
         :accessor body
         :type list))
  (:documentation ""))

(defparameter *special-parsers* (make-hash-table :test #'eq))

(defun spec-names (spec)
  "Validates the spec and returns a list of bound names."
  (flet ((spec-keyword-p (sym)
           (and (symbolp sym) (char= #\& (schar (symbol-name sym) 0)))))
    (cond ((null spec) nil)
          ((consp (car spec)) ; list pattern
           (when (or (eq (caar spec) '&rest) (eq (caar spec) '&body))
             (assert (= 2 (length (car spec)))))
           (append (spec-names (car spec)) (spec-names (cdr spec))))
          ((spec-keyword-p (car spec))
           (assert (and (second spec) (symbolp (second spec))
                        (not (spec-keyword-p (second spec)))))
           (cons (second spec) (spec-names (cddr spec))))
          (t
           (assert (symbolp (car spec)))
           (cons (car spec) (spec-names (cdr spec)))))))

;; if lisp is mud then parsing is the experience of it slipping through your fingers
(defmacro deform ((name &rest spec) &key var fun block rest-patterns)
  "Generates an AST type and scope parser associated with a macro or
special operator NAME. The generated parser performs checking and signals form-parse-error when runtime matching fails.
Only one &body may occur per scope, designating an implicit progn containing
arbitrary evaluated forms. Rest patterns destructure but do not create fields.
Caveats:
- recursive &rest patterns are not allowed
- &or SYMBOL... only matches a symbol
- If a variable is not bound as a VAR, then it is considered to be an evaluation context."
  `(progn
     (defclass ,(symbolicate name "-FORM") (irregular-form)
       ((op :initform ',name)
        ,@(loop for name in (spec-names spec)
                collect `(,name :initarg ,(make-keyword name)
                                :accessor ,name))
        ,@(when (or var fun block)
            `((envmap :initarg :envmap :initform (error "no env!"))))))

     ;; spec names validated above ^
     (defun ,(symbolicate name "-WALKER")
         (form env &optional (mapper (constantly form)) (collect-p nil))
       "(mapper form ctx..) -> new form. Returns parsed result as sexpr"
       (declare (optimize debug))
       ,(labels
            ((parse-body (form env mapper collect-p)
               (let ((result (loop for form in body
                                   collect (funcall mapper form :eval env))))
                 (when collect-p (list result))))
             (cons-parser (spec)
               (cond
                 ((null spec) (lambda (form env mapper envmap)
                                (declare (ignore env mapper envmap))
                                (if (null form) nil
                                    (form-parse-error "expected null, got ~a" form))))
                 ((atom (car spec))
                  (case (car spec)
                    (&declarations
                     (lambda (form env mapper collect-p)
                       (destructuring-bind (decls rest)
                           (parse-body-declarations form nil)
                         ;; call mapper for side effect
                         (let ((decls (funcall mapper decls :declaration env))
                               (rest (funcall (cons-parser (cddr spec)) rest
                                              env mapper collect-p)))
                           (when collect-p (cons decls rest))))))
                    (&body #'parse-body)
                    (&or
                     (lambda (body env mapper collect-p)
                       ;; try to match
                       ()
                       (when collect-p
                         )))
                    (&rest
                     (lambda (body env mapper collect-p)
                       (if-let (pattern (cdr (assoc (second spec) rest-patterns)))
                         (let ((result (loop for form in body
                                             collect (funcall (cons-parser pattern) form
                                                              env mapper collect-p))))
                           (when collect-p (list result)))
                         (funcall #'parse-body body env mapper collect-p))))
                    ((nil)
                     (form-parse-error "constant NIL in spec"))
                    (t ; TODO
                     (constantly nil))))
                 (t ; (consp (car spec))
                  (lambda (form env mapper collect-p)
                    (if (listp (car form))
                        (let ((list-results (funcall (cons-parser (car spec)) (car form)
                                                     env mapper collect-p))
                              (rest (funcall (cons-parser (cdr spec)) (cdr form)
                                             env mapper collect-p)))
                          (when collect-p (cons list-results rest)))
                        (form-parse-error "list expected, got ~a, context ~a"
                                          form spec)))))))
          `(funcall ,(cons-parser spec) (cdr form) env mapper collect-p)))

     (setf (gethash ',name *special-parsers*) ',(symbolicate name "-WALKER"))))

;; (deform (let (&rest vars) &declarations decls &body body)
;;   :var ((name . body))
;;   :rest-patterns ((vars . (&or name (name init)))))
;; (deform (flet (&rest funs)
;;           &declarations decls
;;           &body body)
;;   :fun ((name . body))
;;   :block ((name . fcode))
;;   :rest-patterns ((funs . (name &lambda fcode))))
;; (deform (let (&rest (&or name (name init)))
;;               &declarations decls
;;               &body body)
;;   :var ((name . body))
;;   :restnames '(vars))
;; (deform (let* (&rest (&or name (name init)))
;;               &declarations
;;               &body body)
;;   :var ((name . body) ((< name) . init)))
;; (deform (labels (&rest (name &lambda fcode))
;;               &declarations
;;               &body body)
;;   :fun (((* name) . fcode) (name . body))
;;   :block ((name . fcode))
;;   :restnames '(funs))
;; (deform (macrolet (&rest (name &macro-lambda macro-code))
;;               &declarations
;;               &body body)
;;   :var ((name . body))
;;   :block ((name . macro-code)))
;; (deform (symbol-macrolet (&rest (name expansion))
;;               &declarations
;;               &body body)
;;   :var ((name . body)))
;; (deform (block name &body body)
;;   :block ((name . body)))
;; (deform (defun name &lambda fun-code)
;;   :fun ((name . fun-code))
;;   :block ((name . fun-code)))
;; (deform (lambda &lambda fun-code))
;; (deform (defmethod name &rest-syms qualifiers &method-lambda fun-code)
;;   :fun ((name . fun-code))
;;   :block ((name . fun-code)))
;; (deform (defmacro name &macro-lambda macro-code)
;;   :block ((name . macro-code)))

;; This makes no attempt to guess general types, as (impl) source may not be available
;; need per-symbol expansion for symbols, catch errors for stuff like loop keywords
(defun find-macroexpansion-bindings (call env)
  "Form is a single macro call, which is macroexpanded to produce a scope description
for that particular instance."
  (let ((info (make-hash-table)))
    (labels
        ((search-tree (o tree) ; TODO cache all trees in a hash table
           (if (atom tree)
               (eq o tree)
               (or (eq o tree)
                   (some (curry #'search-tree o) (cdr tree)))))
         ;; look breadth-first through an expression, stopping at evaluated
         ;; forms from the call. Not necessarily a plain macro call
         (collect-bindings (form env)
           (disp (list 'looking-at form))
           (if (or (atom form) (search-tree form call))
               (progn
                 (disp (list "stopped at" form))
                 nil)
               (let ((op (car form)))
                 (if (or (special-operator-p op) (hardwired-p op))
                     (case op
                       (let
                           (destructuring-bind
                               (let bindings &rest decls-body) form
                             (declare (ignore let))
                             (let ((names (mapcar #'first
                                                  (canonicalize-bindings bindings))))
                               (dolist (name names)
                                 (when (search-tree name call)
                                   (pushnew name (gethash :variable info))))
                               ;; iterate body
                               (mapc (rcurry #'collect-bindings
                                             (env-with-variables env names))
                                     (second (parse-body-declarations decls-body nil))))))
                       (block
                           (destructuring-bind
                               (block name &rest body) form
                             (declare (ignore block))
                             (pushnew name (gethash :block info))
                             ;; don't need blocks in env for macroexpansions
                             (mapc (rcurry #'collect-bindings env) body)))
                       (tagbody
                          (destructuring-bind
                              (tagbody &rest body) form
                            (declare (ignore tagbody)) ; don't care about tags
                            (dolist (subform body)
                              (when (not (go-tag-p subform))
                                (collect-bindings subform env)))))
                       (go nil)
                       (t
                        (disp 'skipped)
                        nil))
                     ;; macro call
                     (multiple-value-bind (newform expanded-p)
                         (env-macroexpand form env)
                       (if expanded-p
                           ;; TODO check for special forms before proceeding with macro
                           (cerror "continue" "stopped at macro call: ~a~%expansion: ~a"
                                   form newform)
                           ;; function call
                           nil)))))))
      ;; must strictly be a regular macro call, otherwise macroexpanding is bad
      (assert (and (consp call) (not (hardwired-p (car call)))))
      ;; approach this breadth first
      (let ((expansion (disp (env-macroexpand call env))))
        (collect-bindings expansion env)
        (print (hash-table-plist info))))))

;; test:
;; (loop for it from 1 to 10
;;       if (evenp it)
;;         collect it)

;; test:
;; (loop for it from from
;;       do (print it))

;; special operators
(when nil
  (defun handle-let (bindings body-forms env)
    (let* ((body-env
             (env-with-variables env (mapcar #'first (canonicalize-bindings bindings))))
           (body-asts (mapcar (rcurry 'form-ast body-env) body-forms))
           (ast (make-instance 'let-form :body body-asts :lexenv env)))
      ;; ensure all bindings exist in the ast, we rely on this below
      (loop for binding in bindings
            do (if (symbolp binding)
                   (push (cons (make-instance 'binder :name binding) nil) (bindings ast))
                   (push (cons (make-instance 'binder :name (car binding))
                               (form-ast (second binding) env))
                         (bindings ast))))
      ast))

  (defun handle-special (form env)
    (let ((op (car form)))
      (ccase op
        (let (destructuring-bind (_ bindings &rest body) form ; TODO declarations
               (declare (ignore _))
               (handle-let bindings body env)))
        (*literal-magic* form)
        )))

  (defun form-ast (form env)
    "Convert form to an ast in the lexical environment env. Only makes sense when applied
to forms intended for evaluation.
Returns values: ast, variable references"
    (if (atom form)
        (let ((record (env-variable-info form env)))
          (assert (symbolp form))
          (cond ((and record (eq (second record) nil))
                 (let ((ref (make-instance 'symbol-ref :name form)))
                   (values ref (list (make-ref-info :ref ref)))))
                ((and record (eq (second record) :macro))
                 (error "local symbol macro ~s" form))
                ((nth-value 1 (macroexpand-1 form))
                 (error "global symbol macro ~s" form))
                (t form))) ; keyword or nil or t
        ;;
        (let ((record (env-function-info form env)))
          (cond ((and record (eq (second record) nil))
                 (make-instance 'symbol-ref :name form))
                ((and record (eq (second record) :macro))
                 (error "local macro ~s" form))
                ((special-operator-p (car form))
                 (handle-special form env))
                ((macroexpand-1 form)
                 (error "global macro ~s" form))
                (t
                 (error "unknown ~s?" form)))))))
