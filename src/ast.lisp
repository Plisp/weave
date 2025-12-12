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
       (format t "~s|> ~a~%" ',form ,res)
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
function-bindings: (f &optional macro-params-body)
copy-env can exploit structure sharing, remember to PUSH!"
  (%function-bindings (list))
  (%variable-bindings (list))
  (%blocks (list))
  (%tags (list)))

(defmethod function-bindings ((env env)) (%function-bindings env))
(defmethod (setf function-bindings) (new (env env)) (setf (%function-bindings env) new))
(defmethod variable-bindings ((env env)) (%variable-bindings env))
(defmethod (setf variable-bindings) (new (env env)) (setf (%variable-bindings env) new))
(defmethod blocks ((env env)) (%blocks env))
(defmethod (setf blocks) (new (env env)) (setf (%blocks env) new))
(defmethod tags ((env env)) (%tags env))
(defmethod (setf tags) (new (env env)) (setf (%tags env) new))

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

(declaim (notinline form-parse-error))
(defun form-parse-error (control &rest args)
  (error 'form-parse-error
         :format-control (concatenate 'string "parse failed: " control)
         :format-arguments args))

;; we need to be more permissive for lambda lists. It makes little sense to preserve
;; well-formedness when it often breaks with edits due to positional &keyword context
;; e.g. (&key (a _) (b _)) -?> (a b)  or  (&optional (b _) c) -?> (b &optional c)
(defun lambda-list-variable-names (list)
  "Requires a list but otherwise doesn't force well-formedness and tries to be very
tolerant. Returns an ordered list of symbols suitable for constructing cumulative scopes."
  (loop for name in list
        when (and (not (find name lambda-list-keywords))
                  (trivia:match name
                    ((type symbol) name)
                    ((list* (and (type symbol) x) _) x) ; methods and defaults
                    ((list* (list _ (and (type symbol) x)) _) x)))
          collect it
        when (trivia:match name
               ((list _ _ (and (type symbol) supplied-p-var)) supplied-p-var))
          collect it))
;; could be a method lambda list, or an intermediate &key editing state. allow it
(assert (equal (lambda-list-variable-names '((a t))) '(A)))
(assert (equal (lambda-list-variable-names '(a a)) '(A A))) ; XXX are duplicates fine?
(assert (equal (lambda-list-variable-names '(a &key ((:weave name) default supplied-p)))
               '(A NAME SUPPLIED-P)))

(defun macro-lambda-variable-names (list)
  (loop with names := (list t) ; sentinel cons
        with lastcons = names
        for rest on list
        for this = (car rest)
        do (cond ((member this '(&body &rest &key &optional &aux) :test #'eq)
                  (rplacd lastcons (lambda-list-variable-names rest))
                  (return (cdr names)))
                 ((find this lambda-list-keywords) nil)
                 ((consp this)
                  (when-let (rec (macro-lambda-variable-names this))
                    (rplacd lastcons rec)
                    (setf lastcons (last rec))))
                 (t ; nil being a list sucks
                  (when (and this (symbolp this))
                    (rplacd lastcons (list this))
                    (setf lastcons (cdr lastcons)))))
        finally (return (cdr names))))

(assert (equal (macro-lambda-variable-names '(() &body body)) '(BODY)))
(assert (equal (macro-lambda-variable-names '((stream filespec &rest options) &body body))
               '(STREAM FILESPEC OPTIONS BODY)))

(defun parse-body-declarations (body documentation)
  "wraps alexandria but throws a form-parse-error"
  (declare (optimize speed))
  (handler-case (parse-body body :documentation documentation)
    (error ()
      (form-parse-error "docstring after declarations"))))

(defun canonicalize-bindings (bindings)
  (loop for b in bindings
        collect (if (symbolp b) `(,b nil) b)))

(assert (equal (canonicalize-bindings '(x (y 0))) '((X NIL) (Y 0))))

;;
;;; parsing
;;; if lisp is mud then parsing is the experience of it slipping through your fingers
;;

;; return-from
;; catch load-time-value setq
;; eval-when locally
;; tagbody
;; function multiple-value-call the
;; go multiple-value-prog1 throw
;; if progn unwind-protect
;; progv
;; quote

(defparameter *special-walkers* (make-hash-table :test #'eq))

(eval-when (:compile-toplevel)
  (defparameter *parser-keywords* '(&rest &body &or &declarations &rest-qualifiers
                                    &lambda &method-lambda &macro-lambda))
  (defparameter *arity-1-parser-keywords* '(&rest &body
                                            &lambda &method-lambda &macro-lambda))
  (defun spec-names (spec)
    "Validates the spec and returns an alist of tag names and their arity-1 specifier
if applicable."
    (flet ((spec-keyword-p (sym)
             (member sym *parser-keywords* :test #'eq)))
      (cond ((null spec) nil)
            ((consp (car spec)) ; list pattern
             (append (spec-names (car spec)) (spec-names (cdr spec))))
            ;; spec keyword, expect a symbol immediately after, except for &or
            ((spec-keyword-p (car spec))
             (assert (or (eq (car spec) '&or)
                         (and (second spec)
                              (symbolp (second spec))
                              (not (spec-keyword-p (second spec))))))
             (when (member (car spec) *arity-1-parser-keywords* :test #'eq)
               (assert (= 2 (length spec)))
               (return-from spec-names (list (cons (second spec) (car spec)))))
             (spec-names (cdr spec)))
            (t ; lone patterns must be symbols
             (assert (symbolp (car spec)))
             (cons (list (car spec)) (spec-names (cdr spec)))))))

  (defun search-tree (o tree)
    (declare (optimize speed))
    (if (atom tree)
        (eq o tree)
        (or (search-tree o (car tree))
            (some (lambda (m) (search-tree o m)) (cdr tree)))))

  (defstruct function-info
    (kind '&lambda :type (or (eql &lambda) (eql &macro-lambda) (eql &method-lambda)))
    (arglist nil)
    (documentation nil :type (or null string))
    (decls nil)
    (body nil)))

(defmacro deform ((name &rest spec) &key var fun block rest-patterns)
  "Generates an AST type and scope parser associated with a macro or
special operator NAME. The generated parser performs checking and signals form-parse-error
when runtime matching fails.

Keywords starting with & have special meaning and have arity 1, except for &or.
Only one &body/&rest may occur per scope, designating an implicit progn similar to lambda
lists. Rest-patterns destructure and parse subforms but do not create fields.
Any binding (see below) forces a symbol match.

VAR, FUN and BLOCK are alists with entries (binding-tag . body-tag), where each BINDING-TAG
establishes the corresponding kind of lexical binding in the evaluation contexts named
by BODY-TAG. BINDING-TAG can be NIL to indicate BODY-TAG refers to evaluated forms.
Binding tag names may not be (member < > *)"
  (let ((parser-name (symbolicate name "-CONS-PARSER")))
    (flet ((validate-custom-binds (entries)
             (loop
               for (bind-tag . form-tag) in entries
               do (trivia:match bind-tag
                    ((list (or (eql '<) (eql '*)) bind-name)
                     (assert (symbolp bind-name))
                     (loop
                       for (rest-tag . pattern) in rest-patterns
                       do (when (search-tree bind-name pattern)
                            (assert (search-tree form-tag pattern))
                            (assert
                             (trivia:match pattern
                               ((list (eql bind-name) (eql form-tag)) t)
                               ((list (eql bind-name) keyword (eql form-tag))
                                (member keyword *arity-1-parser-keywords*))
                               ((list* (eql '&or) rest)
                                (loop
                                  for pattern in rest
                                  thereis
                                  (trivia:match pattern
                                    ((list (eql bind-name) (eql form-tag)) t)
                                    ((list (eql bind-name) keyword (eql form-tag))
                                     (member keyword *arity-1-parser-keywords*))))))))))))))
      (validate-custom-binds var)
      (validate-custom-binds fun)
      (validate-custom-binds block))

    `(progn
       (defclass ,(symbolicate name "-FORM") (irregular-form)
         ((op :initform ',name)
          ,@(loop for name in (delete-duplicates (mapcar #'car (spec-names spec)))
                  collect `(,name :initarg ,(make-keyword name)
                                  :accessor ,name))
          ,@(when (or var fun block)
              `((envmap :initarg :envmap :initform (error "no env!"))))))
       ;; spec validated above by spec-names ^
       (defmacro ,parser-name (spec)
         (cond
           ((null spec) `(lambda (form tagmap)
                           (declare (ignore tagmap))
                           (if (null form) nil
                               (form-parse-error "expected null, got ~a" form))))
           ((atom spec)
            `(lambda (form tagmap)
               (when (and (or (assoc ',spec ',',var)
                              (assoc ',spec ',',fun)
                              (assoc ',spec ',',block))
                          (not (symbolp form)))
                 (form-parse-error "expected bound symbol match: ~a" form))
               (push form (gethash ',spec tagmap))
               form))
           ((consp (car spec))
            `(lambda (form tagmap)
               (if (listp (car form))
                   (cons
                    (funcall (,',parser-name ,(car spec)) (car form) tagmap)
                    (funcall (,',parser-name ,(cdr spec)) (cdr form) tagmap))
                   (form-parse-error "list expected, got ~a, context ~a" form
                                     ',spec))))
           (t ; (atom (car spec))
            (case (car spec)
              (&declarations
               `(lambda (form tagmap)
                  (multiple-value-bind (body decls)
                      (parse-body-declarations form nil)
                    (push decls (gethash ',(second spec) tagmap))
                    (cons decls (funcall (,',parser-name ,(cddr spec)) body tagmap)))))
              (&or
               `(lambda (form tagmap)
                  (block nil
                    ,@(loop
                        for pattern in (cdr spec)
                        collect
                        `(handler-case
                             (return (funcall (,',parser-name ,pattern) form tagmap))
                           (form-parse-error ())))
                    (form-parse-error "expected one of ~a got ~a" ',(cdr spec) form))))
              ((&method-lambda &lambda &macro-lambda)
               `(lambda (forms tagmap)
                  (when (null forms)
                    (form-parse-error "missing lambda list"))
                  ;; special treatment is needed after parsing the body forms
                  ;; the walker only needs the source unlike the parser
                  (push forms (gethash ',(second spec) tagmap))
                  forms))
              (&body
               `(lambda (body tagmap)
                  (symbol-macrolet ((res (gethash ',(second spec) tagmap)))
                    (push body res)
                    body)))
              (&rest
               `(lambda (body tagmap)
                  (symbol-macrolet ((res (gethash ',(second spec) tagmap)))
                    ,(if-let (pattern (cdr (assoc (second spec) ',rest-patterns)))
                       `(let ((body-parsed
                                (loop for form in body
                                      collect (funcall (,',parser-name ,pattern)
                                                       form tagmap))))
                          (push body-parsed res)
                          body-parsed)
                       `(progn (push body res) body)))))
              (&rest-qualifiers
               `(lambda (body tagmap)
                  (symbol-macrolet ((res (gethash ',(second spec) tagmap)))
                    (let ((qualifiers
                            (loop for form := (car body)
                                  while (symbolp form)
                                  collect form
                                  do (pop body))))
                      (push qualifiers res)
                      (cons qualifiers
                            (funcall (,',parser-name ,(cddr spec)) body tagmap))))))
              (t ; symbol match car... against e....
               `(lambda (form tagmap)
                  (if (consp form)
                      (cons (funcall (,',parser-name ,(car spec)) (car form) tagmap)
                            (funcall (,',parser-name ,(cdr spec)) (cdr form) tagmap))
                      (form-parse-error "expected non-nil car: ~a" form))))))))

       (defun ,(symbolicate name "-TAGGER") (form)
         "(tagmap form ctx..) -> new form. Returns parsed result as s expr"
         (let ((tagmap (make-hash-table :test #'eq)))
           (funcall (,parser-name ,spec) (cdr form) tagmap)
           tagmap))

       (defun ,(symbolicate name "-WALKER") (tagmap walker env)
         `(progn
            ,@(loop
                with res-code := nil
                for (bind-tag . form-tag) in var
                collect
                (trivia:match bind-tag
                  ((list (or (eql '<) (eql '*)) bind-name)
                   (let ((custom (find-custom-binds var)))
                     (error "")))
                  ((list* name expansion) ; symbol macro
                   (error ""))
                  (_ ; variable TODO handle symbol macro and non-rest bindings
                   `(loop
                      for body-form in (apply #'append (gethash ',form-tag tagmap))
                      do (funcall walker
                                  body-form
                                  (env-with-variables env (gethash ',bind-tag tagmap))))
                   )))))

       (setf (gethash ',name *special-walkers*) ',(symbolicate name "-WALKER"))
       (values))))

(deform (defmethod name &rest-qualifiers qualifiers &method-lambda fun-code)
  :fun ((name . fun-code))
  :block ((name . fun-code)))

(deform (alexandria:when-let* (&or (name init) (&rest vars))
          &body body)
  :var ((name . body) ((< name) . init))
  :rest-patterns ((vars . (name init))))

(deform (let (&rest vars)
          &declarations decls
          &body body)
  :var ((name . body) (nil . init))
  :rest-patterns ((vars . (&or name (name init)))))

(deform (let* (&rest vars)
          &declarations decls
          &body body)
  :var ((name . body) ((< name) . init)) ; XXX extremely hacky handling
  :rest-patterns ((vars . (&or name (name init)))))

(deform (flet (&rest funs)
          &declarations decls
          &body body)
  :fun ((name . body))
  :block ((name . fcode))
  :rest-patterns ((funs . (name &lambda fcode))))

(deform (labels (&rest funs)
          &declarations decls
          &body body)
  :fun (((* name) . fcode) (name . body))
  :block ((name . fcode))
  :rest-patterns ((funs . (name &lambda fcode))))

(deform (macrolet (&rest macro-defs)
          &declarations decls
          &body body)
  :fun (((name macro-code) . body))
  :block ((name . macro-code))
  :rest-patterns ((macro-defs . (name &macro-lambda macro-code))))

(deform (symbol-macrolet (&rest macro-code)
          &declarations decls
          &body body)
  :var (((name expansion) . body))
  :rest-patterns ((macro-code . (name expansion))))

(deform (block name &body body)
        :block ((name . body)))

(deform (defun name &lambda fun-code)
  :fun ((name . fun-code))
  :block ((name . fun-code)))

(deform (lambda &lambda fun-code))

(deform (defmacro name &macro-lambda macro-code)
  :block ((name . macro-code)))

;; test:
;; (loop for it from 1 to 10
;;       if (evenp it)
;;         collect it)

;; test:
;; (loop for it from from
;;       do (print it))

;; special operators
;; (when nil
;;   (defun handle-let (bindings body-forms env)
;;     (let* ((body-env
;;              (env-with-variables env (mapcar #'first (canonicalize-bindings bindings))))
;;            (body-asts (mapcar (rcurry 'form-ast body-env) body-forms))
;;            (ast (make-instance 'let-form :body body-asts :lexenv env)))
;;       (loop for binding in bindings
;;             do (if (symbolp binding)
;;                    (push (cons (make-instance 'binder :name binding) nil)
;;                                (bindings ast))
;;                    (push (cons (make-instance 'binder :name (car binding))
;;                                (form-ast (second binding) env))
;;                          (bindings ast))))
;;       ast)))
