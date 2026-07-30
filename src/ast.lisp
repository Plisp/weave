;;;;
;;;; incremental lisp parsing
;;;;
;;
;; note: can use cl-environments:function-information
;; note: slynk-backend:arglist

(uiop:define-package #:weave-parser
  (:use :cl #:alexandria-2 #:weave-utils)
  (:export #:make-env
           #:parse
           #:is-atom))
(in-package #:weave-parser)

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
               ((eclector.reader:quasiquote
                 eclector.reader:unquote eclector.reader:unquote-splicing)
                `(,(car result) ,@children))
               (t children))
             children))))

;; TODO handle comments (including formatting) and reader conditionals properly
(defmethod eclector.parse-result:make-skipped-input-result
    ((client my-client) (stream t) (reason t) (children t) (source t))
  (list :reason reason :source source :children children))

(defmethod eclector.reader:evaluate-expression ((client my-client) (expression t))
  (list 'read-eval expression))

(defmethod eclector.reader:fixup ((client my-client) obj state)
  (declare (ignore obj state))
  (error "TODO handle circular lists, ask how eclector does it"))

;; TODO reimplement this but with structure sharing to handle quasiquote
;; (defun transform (form &optional (wrap-in-list t))
;;   (flet ((maybe-wrap (thing)
;;            (if wrap-in-list
;;                `(list ,thing)
;;                thing)))
;;     (typecase form
;;       ((cons (eql unquote))
;;        (maybe-wrap (second form)))
;;       ((cons (eql unquote-splicing))
;;        (second form))
;;       (t
;;        (maybe-wrap (transform-quasiquote-argument form))))))

;; (defun transform-compound (compound)
;;   (labels ((rec (object)
;;              (typecase object
;;                ((cons t (or (not cons) (cons (eql unquote))))
;;                 (list (transform (car object)) (transform (cdr object) nil)))
;;                ((cons t (cons (eql unquote-splicing)))
;;                 (error 'unquote-splicing-in-dotted-list
;;                        :argument (second object)))
;;                (t
;;                 (list* (transform (car object)) (rec (cdr object)))))))
;;     (rec compound)))

;; (defun transform-quasiquote-argument (argument)
;;   (typecase argument
;;     ((cons (eql unquote))
;;      (second argument))
;;     ((cons (eql unquote-splicing))
;;      (error "unquote splice at top"))
;;     (cons
;;      `(append ,@(transform-compound argument)))
;;     ((and vector (not string))
;;      `(apply #'vector
;;              ,(transform-quasiquote-argument
;;                (coerce argument 'list))))
;;     (t
;;      `(quote ,argument))))

;; (defun expand (form)
;;   (if (atom form)
;;       form
;;       (let ((expanded (cons (expand (car form)) (expand (cdr form)))))
;;         (if (eq (first expanded) 'quasiquote)
;;             (transform-quasiquote-argument (second expanded))
;;             expanded))))

;; (defmacro quasiquote (&whole form argument)
;;   (declare (ignore argument))
;;   (expand form))

;; (defmacro unquote ()
;;   (error "comma outside backquote"))
;; (defmacro unquote-splicing ()
;;   (error "comma-at outside backquote"))

;; (defun read-translate (form)
;;   (labels ((translate-no-quasiquote (form)
;;              (if (atom form)
;;                  form
;;                  (case (car form)
;;                    (read-function `(function ,(translate-no-quasiquote (second form))))
;;                    (read-quote `(quote ,(translate-no-quasiquote (second form))))
;;                    (t (mapcar #'translate-no-quasiquote form))))))
;;     (translate-no-quasiquote (eclector.reader::expand form))))

;;
;;; class defs: mainly we want a structure that's
;;; - close enough to s-expressions for macroexpansion and evaluation
;;; - gives identity to semantic units which may need identity under editing
;;;   since we should avoid sequence cursors
;;;   - let binders need identity through insertion, type info is associated with
;;;     binders rather than the references
;;;   - variable refs in all evaluation contexts have identity like other
;;;     evaluated forms for uniformity
;;; - caches information on binders and evaluation contexts (needed for
;;;   completion and basic analysis) so macroexpansions can be lexically
;;;   limited during interactive editing
;;;   - make sure editing an AST node does not affect lexical bindings outside
;;; - structural operations should not be allowed on named nodes anyways
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

(defclass atom-form ()
  ())

(defclass eval-form ()
  ()
  (:documentation "Form in an evaluation context, perhaps quoted."))

(defclass literal (eval-form atom-form)
  ((str :initarg :str
        :type string :initform (error "literal not provided")
        :reader str))
  (:documentation "Atomic literal"))

(defclass symbol-ref (eval-form atom-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :reader name))
  (:documentation "Represents a symbol, possibly referring to a symbol macro."))

(defclass binder (atom-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :reader name))
  (:documentation "Represents a binder, NOT a reference in evaluation position."))

(defclass function-call (eval-form)
  ((name :initarg :name
         :initform (error "must provide function name")
         :reader name)
   (body :initarg :body
         :initform (error "must provide function argument list")
         :reader body))
  (:documentation "body is a list of eval-forms"))

(defclass irregular-form (eval-form)
  ()
  (:documentation "macro invocation or special operator"))

(defclass macro-call (irregular-form)
  ((op :initarg :op
       :reader op)
   (body :initarg :body
         :initform (error "must provide unknown call body")
         :reader body)
   (gensym-names :initarg :gensym-names
                 :reader gensym-names)
   (eval-binders :initarg :eval-binders
                 :reader eval-binders)
   (subform-asts :initarg :subform-asts
                 :reader subform-asts)))

(defclass function-code ()
  ((lambda-list :initarg :lambda-list
                :reader lambda-list)
   (docstring :initarg :docstring
              :reader docstring
              :type string)
   (declarations :initarg :declarations
                 :reader declarations)
   (body :initarg :body
         :reader body))
  (:documentation "(macro) lambda list and body list of eval-forms"))

(defgeneric is-atom (node)
  (:method (node) nil))
(defmethod is-atom ((node binder)) t)
(defmethod is-atom ((node literal)) t)
(defmethod is-atom ((node symbol-ref)) t)

(defstruct location
  "`id's usually contain a symbol (slot), possibly list index and should respect `cl:equal'.
These are specific to the `node' type."
  (node (error "must provide parent node"))
  (id nil))

(defgeneric get-location (node location)
  (:documentation "Returns the current value at `location'."))
(defun getloc (location)
  (get-location (location-node location) location))

(defgeneric is-body (node id)
  (:documentation "A body form is suitable for structural editing operations.")
  (:method (node id) nil)
  (:method (node (id (eql 'body))) t))
(defgeneric update (node id new-value)
  (:documentation "Functionally updates the location corresponding to `id',
returns a new node with all child nodes identical except the hole indicated.
List structure may share conses with the old node."))

(defun location= (n1 n2)
  (and (eq (location-node n1) (location-node n2))
       (equal (location-id n1) (location-id n2))))

(defmethod get-location ((node function-call) location)
  (trivia:cmatch (location-id location)
    ((eql 'name) (name node))
    ((eql 'body) (body node))
    ((type integer) (nth (location-id location) (body node)))))

(defmethod update ((node function-call) id new-value)
  (trivia:cmatch id
    ((eql 'name)
     (make-instance 'function-call :name new-value :body (body node)))
    ((eql 'body)
     (make-instance 'function-call :name (name node) :body new-value))
    ((type integer)
     (let ((old-body (body node)))
       (make-instance 'function-call :name (name node)
                                     :body `(,@(subseq old-body 0 id)
                                             ,new-value
                                             ,@(nthcdr (1+ id) old-body)))))))

(defmethod print-object ((object literal) stream)
  (format stream "<lit: ~a>" (str object)))

(defmethod print-object ((object symbol-ref) stream)
  (pprint-logical-block (stream (list))
    (format stream "<~a@~a>" (name object) (addr-str object))))

(defmethod print-object ((object binder) stream)
  (pprint-logical-block (stream (list))
    (format stream "<~a@~a>" (name object) (addr-str object))))

(defmethod print-object ((object function-call) stream)
  (pprint-logical-block (stream (body object) :suffix ")")
    (write-char #\( stream)
    (write (name object) :stream stream)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (print-object (pprint-pop) stream))))

(defmethod print-object ((object function-call) stream)
  (pprint-logical-block (stream (body object))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (env (:conc-name nil))
    "variable-bindings: (v &optional macroexpansion)
function-bindings: (f &optional macro-params-body)
copy-env can exploit structure sharing, remember to PUSH!"
    (%function-bindings (list))
    (%variable-bindings (list))
    (%blocks (list))
    (%tags (list)))

  (defmethod make-load-form ((o env) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots o))

  (define-constant +nullenv+ (make-env) :test 'equalp))

(defun function-bindings (env) (%function-bindings env))
(defun variable-bindings (env) (%variable-bindings env))
(defun blocks (env) (%blocks env))
(defun tags (env) (%tags env))

(defun env-variable-info (name env)
  (loop for entry in (variable-bindings env)
        do (when (and (symbolp name) (eq name entry))
             (return (values name nil)))
           (when (and (consp entry) (eq name (car entry)))
             (return (values (car entry) (cdr entry))))))
(defun env-function-info (name env)
  (loop for entry in (function-bindings env)
        do (when (and (symbolp name) (eq name entry))
             (return (values name nil)))
           (when (and (consp entry) (eq name (car entry)))
             (return (values (car entry) (cdr entry))))))

(defun env-with-variables (env bindings)
  (let ((new-env (copy-env env)))
    (setf (%variable-bindings new-env) (append bindings (%variable-bindings new-env)))
    new-env))

(defun env-with-functions (env bindings)
  (let ((new-env (copy-env env)))
    (setf (%function-bindings new-env) (append bindings (%function-bindings new-env)))
    new-env))

(defun env-with-blocks (env bindings)
  (let ((new-env (copy-env env)))
    (setf (%blocks new-env) (append bindings (%blocks new-env)))
    new-env))

(defun wrap-with-wrapper (form entries wrapper)
  (if (null entries)
      form
      (wrap-with-wrapper (funcall wrapper form (first entries))
                         (cdr entries) wrapper)))

(defun wrap-function-like-env (form entries)
  (wrap-with-wrapper form entries
                     (lambda (form entry)
                       (if (symbolp entry)
                           (flet-wrap entry form)
                           (macrolet-code-wrap (first entry) (cdr entry) form)))))

(defun wrap-variable-like-env (form entries)
  (wrap-with-wrapper form entries
                     (lambda (form entry)
                       (if (symbolp entry)
                           (let-wrap entry form)
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

(declaim (type simple-vector *hardwired-operators*))
(defparameter *hardwired-operators* #(*literal-magic* lambda defun defmethod defmacro)
  "The list of nonportable hardwired macros, not macroexpanded")

(defun hardwired-p (macro-name)
  (position macro-name *hardwired-operators*))

(defun env-macroexpand-1 (x env)
  "Tries macroexpanding a form x in evaluation position once,
does not touch hardwired operators."
  (cond ((symbolp x)
         (multiple-value-bind (result local-expansion)
             (env-variable-info x env)
           (declare (ignore result))
           (if local-expansion
               (values local-expansion t)
               (macroexpand-1 x)))) ; possible global
        ((consp x)
         (let ((name (car x)))
           (multiple-value-bind (result local-expansion)
               (env-function-info name env)
             (cond ((null result) ; global
                    (if (and (symbolp name) ; don't expand direct lambda call
                             (macro-function name) (not (hardwired-p name)))
                        (macroexpand-with-env x env)
                        x))
                   ((null local-expansion) x) ; flet/labels bound
                   (t (macroexpand-with-env x env)))))) ; local macro
        (t x)))

(defun env-macroexpand (form env)
  "Perform full macroexpansion of the head of form"
  (loop with any-expanded-p
        for (newform expanded-p) := (multiple-value-list (env-macroexpand-1 form env))
        until (and (not expanded-p) (eq newform form))
        do (setf form newform)
           (or-f any-expanded-p expanded-p)
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
(defun map-lambda-list (list on-binder value-mapper specializer-list-p)
  "Requires a list but otherwise doesn't force well-formedness and tries to be very
tolerant. Previous calls to ON-BINDER give exactly the environment of VALUE-MAPPER calls.
Reconstructs the list structure from the return values of ON-BINDER and VALUE-MAPPER."
  (loop with seen-opt-key-aux := nil
        with res := (list)
        for elt in list
        do (when (member elt '(&optional &rest &key &aux))
             (setf seen-opt-key-aux t))
           (labels ((maybe-default (val)
                      (if (and specializer-list-p (not seen-opt-key-aux))
                          val
                          (funcall value-mapper val)))
                    (map-param (elt)
                      (trivia:match elt
                        ((or (and (type symbol) x)
                             (list (and (type symbol) x))) ; keyword or method-like
                         (funcall on-binder x))
                        ;; default value or specializer
                        ((list (and (type symbol) x) val)
                         (let ((value (maybe-default val)))
                           (list (funcall on-binder x) value)))
                        ;; supplied-p
                        ((list (and (type symbol) x) val
                               (and (type symbol) supplied-p))
                         (let ((value (maybe-default val)))
                           (list (funcall on-binder x) value
                                 (funcall on-binder supplied-p))))
                        ;; keyword name
                        ((list (list call-name (and (type symbol) x)) val)
                         (let ((value (maybe-default val)))
                           (list (list call-name (funcall on-binder x)) value)))
                        ;; everything
                        ((list (list call-name (and (type symbol) x)) val
                               (and (type symbol) supplied-p))
                         (let ((value (maybe-default val)))
                           (list (list call-name (funcall on-binder x)) value
                                 (funcall on-binder supplied-p))))
                        (_ (funcall value-mapper elt)))))
             (if (find elt lambda-list-keywords)
                 (push elt res)
                 (push (map-param elt) res))) ; invalid
        finally (return (nreverse res))))

(defun map-macro-lambda (list on-binder value-mapper)
  (loop with res := (list)
        for rest on list
        for this = (car rest)
        do (cond ((member this '(&body &rest &key &optional &aux) :test 'eq)
                  (return (nreconc res (map-lambda-list rest on-binder value-mapper nil))))
                 ((find this lambda-list-keywords) (push this res))
                 ((consp this) (push (map-macro-lambda this on-binder value-mapper) res))
                 ((null this) (push nil res))
                 ((symbolp this) (push (funcall on-binder this) res))
                 (t (push (funcall value-mapper this) res)))
        finally (return (nreverse res))))

(defun parse-body-declarations (body documentation)
  "Wraps alexandria but throws a form-parse-error"
  (declare (optimize speed))
  (handler-case (parse-body body :documentation documentation)
    (error ()
      (form-parse-error "docstring after declarations"))))

;;
;;; parsing
;;
;; if lisp is mud then parsing is the experience of it slipping through your fingers
;;

(defparameter *special-walkers* (make-hash-table :test 'eq))
(defparameter *special-parsers* (make-hash-table :test 'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parser-keywords* '(&rest &body &or &declarations &rest-qualifiers
                                    &lambda &method-lambda &macro-lambda))
  (defparameter *arity-1-parser-keywords* '(&rest &body
                                            &lambda &method-lambda &macro-lambda))
  (defun spec-names (spec)
    "Validates the spec and returns an alist of tag names and their arity-1 specifier
if applicable."
    (flet ((spec-keyword-p (sym)
             (member sym *parser-keywords* :test 'eq)))
      (cond ((null spec) nil)
            ((consp (car spec)) ; list pattern
             (append (spec-names (car spec)) (spec-names (cdr spec))))
            ;; spec keyword, expect a non-NIL symbol immediately after, except for &or
            ((spec-keyword-p (car spec))
             (assert (or (not (eq (car spec) '&or))
                         (notany #'spec-keyword-p (cdr spec))))
             (cond ((member (car spec) *arity-1-parser-keywords* :test 'eq)
                    (assert (= 2 (length spec)))
                    (list (cons (second spec) (car spec))))
                   ((member (car spec) '(&declarations &rest-qualifiers) :test 'eq)
                    (cons (cons (second spec) (car spec)) (spec-names (cddr spec))))
                   (t
                    (spec-names (cdr spec)))))
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
    (arglist nil)
    (documentation nil :type (or null string))
    (decls nil)
    (body nil)))

(defmacro defform ((name &rest spec) &key binds rest-patterns)
  "Generates an AST type and scope parser associated with a macro or
special operator NAME. The generated parser performs checking and signals form-parse-error
when runtime matching fails.

Keywords starting with & have special meaning and have arity 1, except for &or, which should
be followed by an arbitrary number of non-keywords patterns to be matched in sequence.
Only one &body/&rest may occur per scope, &body must indicate evaluated forms and &rest
indicates specially interpreted bindings or such, destructuring via REST-PATTERNS.

Every entry (ctx . entries) in BINDS denotes an evaluation context 'ctx' in which the
corresponding lexical entries are bound. ctx is a symbol and entries is a plist (see below)
Binding tag names **must not** be (member NIL < =). For each ctx, may have either:
`<` for sequential variable binding
`=` to indicate a rest entry in which parallel block bindings occur
Any binding forces a symbol match."
  (let ((parser-name (symbolicate name "-SPEC-PARSER"))
        (custom-bind-rest-labels nil)
        (tag-kinds (nconc (spec-names spec)
                          (mapcan (lambda (pair) (spec-names (cdr pair)))
                                  rest-patterns)))
        (toplevel-parts (mapcar 'car (spec-names spec))))
    ;; &body must be evaluated
    (loop for (sym . kind) in tag-kinds
          do (when (eq kind '&body)
               (assert (loop for (ctx . entries) in binds
                             thereis (eq ctx sym)))))
    ;; check consistency of binds
    (loop for (ctx . entries) in binds
          do (assert (symbolp ctx))
             ;; rest forms must not be evaluated
             (assert (not (eq '&rest (cdr (assoc ctx tag-kinds)))))
             (loop
               with special-seen := nil
               for (kind bind-tag) on entries by #'cddr
               do (trivia:ematch bind-tag
                    ((type symbol))
                    ((list (or (eql '<) (eql '=)) bind-name)
                     (assert (symbolp bind-name))
                     ;; only one allowed for now, change when pattern matching
                     (assert (not special-seen))
                     (setf special-seen t)
                     ;; for now = only used for blocks
                     (when (eq (car bind-tag) '=) (assert (or (eq kind :block))))
                     (when (eq (car bind-tag) '<) (assert (or (eq kind :variable))))
                     (loop
                       for (rest-tag . pattern) in rest-patterns
                       do (when (search-tree bind-name pattern)
                            (assert (search-tree ctx pattern))
                            (assert
                             (trivia:match pattern
                               ((list (eql bind-name) (eql ctx))
                                (push (cons bind-name rest-tag) custom-bind-rest-labels))
                               ((list (eql bind-name) keyword (eql ctx))
                                (when (member keyword *arity-1-parser-keywords*)
                                  (push (cons bind-name rest-tag) custom-bind-rest-labels)))
                               ((list* (eql '&or) rest)
                                (loop
                                  for pattern in rest
                                  thereis
                                  (trivia:match pattern
                                    ((list (eql bind-name) (eql ctx))
                                     (push (cons bind-name rest-tag)
                                           custom-bind-rest-labels))
                                    ((list (eql bind-name) keyword (eql ctx))
                                     (when (member keyword *arity-1-parser-keywords*)
                                       (push (cons bind-name rest-tag)
                                             custom-bind-rest-labels)))))))))))
                    ((list (type symbol) (type symbol))))))
    ;; code
    `(progn
       (defclass ,(symbolicate name "-FORM") (irregular-form)
         (,@(loop for name in (remove-duplicates toplevel-parts :test 'equal)
                  collect `(,name :initarg ,(make-keyword name)
                                  ;; XXX could generate initargs instead
                                  :accessor ,name))))
       (export ',(symbolicate name "-FORM"))
       ,@(loop for name in (remove-duplicates toplevel-parts :test 'equal)
               collect `(export ',name))
       ;; spec validated above by spec-names ^
       ;; TODO turn this into a macrolet within lexical scope of tagnames
       ;;      and write a with-tags macro, no need for hash table
       (defmacro ,parser-name (spec)
         (cond
           ((null spec) `(lambda (form tagmap)
                           (declare (ignore tagmap))
                           (if (null form) nil
                               (form-parse-error "expected null, got ~a" form))))
           ((atom spec)
            `(lambda (form tagmap)
               (when (and ,(loop for (ctx . entries) in ',binds
                                 thereis (loop for (kind tag) on entries by #'cddr
                                               thereis (eq spec tag)))
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
                   (form-parse-error "list expected, got ~a, context ~a" form ',spec))))
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
                             (let ((res (funcall (,',parser-name ,pattern) form tagmap)))
                               ;;(push (cons res ',pattern) (gethash ',(cdr spec) tagmap))
                               (return res))
                           (form-parse-error ())))
                    (form-parse-error "expected one of ~a got ~a" ',(cdr spec) form))))
              ((&method-lambda &lambda &macro-lambda)
               `(lambda (forms tagmap)
                  (when (null forms)
                    (form-parse-error "missing lambda list"))
                  (multiple-value-bind (body decls doc)
                      (parse-body-declarations (cdr forms) t)
                    (let ((res (make-function-info :arglist (car forms)
                                                   :documentation doc
                                                   :decls decls
                                                   :body body)))
                      (push res (gethash ',(second spec) tagmap))
                      res))))
              (&body
               `(lambda (body tagmap)
                  (symbol-macrolet ((res (gethash ',(second spec) tagmap)))
                    (push body res)
                    body)))
              (&rest
               `(lambda (body tagmap)
                  (symbol-macrolet ((res (gethash ',(second spec) tagmap)))
                    ,(if-let (pattern (cdr (assoc (second spec) ',rest-patterns)))
                       `(let (body-parsed)
                          (loop for form in body
                                do (push (funcall (,',parser-name ,pattern) form tagmap)
                                         body-parsed))
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
         (let ((tagmap (make-hash-table :test 'eq)))
           (funcall (,parser-name ,spec) (cdr form) tagmap)
           tagmap))

       ,(labels ((sequential-p (entry) (and (listp entry) (eq (first entry) '<)))
                 (parallel-p (entry)   (and (listp entry) (eq (first entry) '=)))
                 (augment-env (env-exp entries)
                   (if entries
                       (destructuring-bind ((kind . record) &rest rest)
                           entries
                         (augment-env
                          (if (or (sequential-p record) (parallel-p record))
                              env-exp ; handled separately
                              (case kind
                                (:variable
                                 `(env-with-variables
                                   ,env-exp
                                   ,(trivia:ematch record
                                      ((list (type symbol) whole-tag)
                                       `(apply 'append (gethash ',whole-tag tagmap)))
                                      ((type symbol) ; binding records may SHARE STRUCTURE
                                       `(gethash ',record tagmap)))))
                                (:function
                                 `(env-with-functions
                                   ,env-exp
                                   ,(trivia:ematch record
                                      ((list (type symbol) whole-tag)
                                       `(loop
                                          for (name . info)
                                            in (apply 'append (gethash ',whole-tag tagmap))
                                          collect
                                          (list* name (function-info-arglist info)
                                                 (function-info-body info))))
                                      ((type symbol)
                                       `(gethash ',record tagmap)))))
                                (:block `(env-with-blocks
                                          ,env-exp (gethash ',record tagmap)))))
                          rest))
                       env-exp)))
          `(progn
             (defun ,(symbolicate name "-WALKER") (form env walker on-binder)
               (declare (ignorable env walker on-binder))
               (let ((tagmap (,(symbolicate name "-TAGGER") form)))
                 (declare (ignorable tagmap))
                 ,@(loop for (ctx-tag . entries) in binds
                         append
                         (loop for (kind record) on entries by #'cddr
                               while record
                               collect (trivia:ematch record
                                         ((list (or (eql '<) (eql '=)) name)
                                          `(loop for name in (gethash ',name tagmap)
                                                 do (funcall on-binder name ,kind)))
                                         ((list name _) ; macro
                                          `(loop for name in (gethash ',name tagmap)
                                                 do (funcall on-binder name ,kind)))
                                         ((type symbol)
                                          `(loop for name in (gethash ',record tagmap)
                                                 do (funcall on-binder name ,kind))))))
                 ,@
                 (loop
                   for (ctx-tag . %entries) in binds
                   for entries := (plist-alist %entries)
                   collect ; (block . (= v)) -> v
                   (let ((seq-tag (third (rassoc-if #'sequential-p entries)))
                         (par-tag (third (rassoc-if #'parallel-p entries)))
                         (ctx-kind (cdr (assoc ctx-tag tag-kinds))))
                     (flet
                         ((walk-function-body (env info augment-body)
                            `(loop initially
                              (flet ((note-binder (binder)
                                       (setf newenv-with-params
                                             (env-with-variables newenv-with-params`(,binder)))
                                       (funcall on-binder binder :variable)))
                                ;; capture of newenv-with-params to pass binder info
                                ,(if (eq ctx-kind '&macro-lambda)
                                     `(map-macro-lambda (function-info-arglist ,info)
                                                        #'note-binder
                                                        (rcurry walker newenv-with-params))
                                     `(map-lambda-list (function-info-arglist ,info)
                                                       #'note-binder
                                                       (rcurry walker newenv-with-params)
                                                       ,(eq ctx-kind '&method-lambda))))
                                   with newenv-with-params := ,env
                                   for body-form in (function-info-body ,info)
                                   do (funcall walker body-form ,augment-body))))
                       (cond
                         (seq-tag ; only for let*-style variable bindings
                          (let ((whole-tag (cdr (assoc seq-tag custom-bind-rest-labels))))
                            `(or ; first detect whether a rest pattern was matched at all
                              (with-lookup (wholeforms (gethash ',whole-tag tagmap))
                                (loop for newenv := ,(augment-env `env entries)
                                        then (if (symbolp whole)
                                                 (env-with-variables newenv `(,whole))
                                                 (env-with-variables newenv `(,(car whole))))
                                      ;; note: must reverse since tags are backwards
                                      ;; we own the tags and exit right afterwards
                                      ;; so it's fine to destructively modify
                                      for whole in (nreverse (first wholeforms))
                                      do (when (listp whole)
                                           ,(if ctx-kind
                                                `(loop for bodyform in (second whole)
                                                       do (funcall walker bodyform newenv))
                                                `(funcall walker (second whole) newenv))))
                                t)
                              (loop for initform
                                      in ,(if ctx-kind
                                              `(apply 'nconc (gethash ',ctx-tag tagmap))
                                              `(gethash ',ctx-tag tagmap))
                                    do (funcall walker initform env)))))
                         ;; to generalise for patterns: compile path to binder instead
                         (par-tag
                          (ecase ctx-kind
                            ((&body &rest &declarations &rest-qualifiers nil)
                             (error "unimplemented"))
                            ((&lambda &macro-lambda &method-lambda)
                             (let ((whole (cdr (assoc par-tag custom-bind-rest-labels))))
                               `(loop
                                  with newenv := ,(augment-env `env entries)
                                  for (name . info) in (first (gethash ',whole tagmap))
                                  ;; parallel bind the block only in the body
                                  ;; newenv-with-params is CAPTURED
                                  do ,(walk-function-body
                                       `newenv `info
                                       `(env-with-blocks newenv-with-params `(,name))))))))
                         (t
                          (ecase ctx-kind
                            ((&declarations &rest-qualifiers))
                            ((&body &rest nil)
                             `(loop with newenv := ,(augment-env `env entries)
                                    for body-form
                                      in ,(if ctx-kind
                                              `(apply 'nconc (gethash ',ctx-tag tagmap))
                                              `(gethash ',ctx-tag tagmap))
                                    do (funcall walker body-form newenv)))
                            ((&lambda &macro-lambda &method-lambda)
                             `(loop with newenv := ,(augment-env `env entries)
                                    for info in (gethash ',ctx-tag tagmap)
                                    do ,(walk-function-body `newenv `info ; vv CAPTURED
                                                            `newenv-with-params)))))))))))

             (defun ,(symbolicate name "-PARSER") (form env walker)
               (declare (ignorable env walker))
               (let ((tagmap (,(symbolicate name "-TAGGER") form))
                     (ast (make-instance ',(symbolicate name "-FORM"))))
                 (declare (ignorable tagmap))
                 ,@
                 (loop
                   for tag in toplevel-parts
                   for entries := (plist-alist (cdr (assoc tag binds)))
                   for tag-kind := (cdr (assoc tag tag-kinds))
                   collect
                   (flet
                       ((walk-function-body (env info augment-body)
                          `(loop
                             with binder-env := +nullenv+
                             with newenv-with-params := ,env
                             with body := (list)
                             with lambda-list
                               := (flet ((note-binder (binder)
                                           (setf newenv-with-params
                                                 (env-with-variables newenv-with-params
                                                                     `(,binder)))
                                           (setf binder-env (env-with-variables binder-env
                                                                                `(,binder)))
                                           (make-instance 'binder :name binder)))
                                    ,(if (eq tag-kind '&macro-lambda)
                                         `(map-macro-lambda
                                           (function-info-arglist ,info)
                                           #'note-binder (rcurry walker newenv-with-params))
                                         `(map-lambda-list
                                           (function-info-arglist ,info)
                                           #'note-binder (rcurry walker newenv-with-params)
                                           ,(eq tag-kind '&method-lambda))))
                             for body-form in (function-info-body ,info)
                             for body-ast = (funcall walker body-form ,augment-body)
                             do (push body-ast body)
                             finally (return
                                       (make-instance
                                        'function-code
                                        :docstring (function-info-documentation ,info)
                                        :declarations (function-info-decls ,info)
                                        :body (nreverse body)
                                        :lambda-list lambda-list)))))
                     (cond
                       ;; &rest special logic
                       ;; XXX basically hardcoded for now since it's not clear how to
                       ;; retain provenance after tagging/parsing: consider LABELS
                       ((eq tag-kind '&rest)
                        (trivia:ematch (cdr (assoc tag rest-patterns))
                          ;; single variable binding
                          ((or (list name-tag value-tag)
                               (list (eql '&or) name-tag (list (eql name-tag) value-tag))
                               ;; allow for multiple forms
                               (list name-tag (eql '&body) value-tag)
                               (list (eql '&or) name-tag
                                     (list (eql name-tag) (eql '&body) value-tag)))
                           (assert (and (symbolp name-tag) (symbolp value-tag)))
                           ;; let*-like
                           (let ((init-binds (plist-alist (cdr (assoc value-tag binds)))))
                             (if (rassoc-if #'sequential-p init-binds)
                                 `(loop
                                    with binder-env := +nullenv+
                                    with res := (list)
                                    for newenv := ,(augment-env `env init-binds)
                                      then (if (symbolp whole)
                                               (env-with-variables newenv `(,whole))
                                               (env-with-variables newenv `(,(car whole))))
                                    for whole in (nreverse (first (gethash ',tag tagmap)))
                                    do (if (symbolp whole)
                                           (progn
                                             (setf binder-env
                                                   (env-with-variables binder-env `(,whole)))
                                             (push (make-instance 'binder :name whole) res))
                                           (let ((b (first whole)))
                                             (push
                                              `(,(make-instance 'binder :name b)
                                                ,,(if (cdr (assoc value-tag tag-kinds))
                                                      `(mapcar (rcurry walker newenv)
                                                               (cdr whole))
                                                      `(funcall walker (second whole)
                                                                newenv)))
                                              res)
                                             ;; note this binder for later initforms
                                             (setf binder-env
                                                   (env-with-variables binder-env `(,b)))))
                                    finally (setf (,tag ast) (nreverse res)))
                                 ;; normal, parallel bindings
                                 `(loop
                                    with res := (list)
                                    with newenv := (env-with-variables
                                                    env (gethash ',name-tag tagmap))
                                    for whole in (nreverse (first (gethash ',tag tagmap)))
                                    do (if (symbolp whole)
                                           (push (make-instance 'binder :name whole) res)
                                           (let ((b (first whole)))
                                             (push
                                              `(,(make-instance 'binder :name b)
                                                ,,(if (cdr (assoc value-tag tag-kinds))
                                                      `(mapcar (rcurry walker env)
                                                               (cdr whole)) ;(b &body ...)
                                                      `(funcall walker (second whole) env)))
                                              res)))
                                    finally (setf (,tag ast) (nreverse res))))))
                          ;; function-like bindings
                          ((list (type symbol) (or (eql '&lambda) (eql '&macro-lambda))
                                 (type symbol))
                           `(loop
                              with res := (list)
                              with newenv := ,(augment-env `env entries)
                              for (name . info) in (first (gethash ',tag tagmap))
                              for fun = ,(walk-function-body
                                          `newenv `info
                                          `(env-with-blocks newenv-with-params (list name)))
                              do (push (cons (make-instance 'binder :name name) fun) res)
                              finally (setf (,tag ast) (nreverse res))))))
                       ;; non-&rest binder
                       ((loop for (ctx . %entries) in binds
                              thereis (cdr (rassoc tag (plist-alist %entries))))
                        `(with-lookup (record (gethash ',tag tagmap))
                           (setf (,tag ast) (make-instance 'binder :name (first record)))))
                       ;; unevaluated - declarations, tags etc.
                       ((not (assoc tag binds))
                        `(with-lookup (record (gethash ',tag tagmap))
                           (setf (,tag ast) (first record))))
                       ;; evaluation contexts
                       (t ; body tags aren't duplicated, so just take the first
                        `(with-lookup (record (gethash ',tag tagmap))
                           ,(ecase tag-kind
                              ((&declarations &rest-qualifiers)
                               `(setf (,tag ast) (first record)))
                              ((&body &rest nil)
                               `(setf (,tag ast)
                                      ,(if tag-kind
                                           `(loop
                                              with newenv := ,(augment-env `env entries)
                                              for body-form in (first record)
                                              collect (funcall walker body-form newenv))
                                           `(funcall walker (first record)
                                                     ,(augment-env `env entries)))))
                              ((&lambda &macro-lambda &method-lambda)
                               `(let ((newenv ,(augment-env `env entries)))
                                  (let ((fun ,(walk-function-body
                                               `newenv `(first record)
                                               `newenv-with-params)))
                                    (setf (,tag ast) fun))))))))))
                 ast))))

       (setf (gethash ',name *special-walkers*) ',(symbolicate name "-WALKER"))
       (setf (gethash ',name *special-parsers*) ',(symbolicate name "-PARSER"))
       (values))))

(defun test-walker (form)
  (disp (hash-table-plist (funcall (symbolicate (car form) "-TAGGER") form)))
  (funcall (symbolicate (car form) "-WALKER")
           form +nullenv+
           (lambda (form env) (disp (list form env)))
           (lambda (binder kind) (disp (list binder kind)))))

(defun literal-parser (form env walker)
  (declare (ignore walker env))
  (make-instance 'literal :str (second form)))
(setf (gethash '*literal-magic* *special-parsers*) 'literal-parser)
(setf (gethash '*literal-magic* *special-walkers*) (constantly nil))

(defform (defmethod name &rest-qualifiers qualifiers &method-lambda fun-code)
  :binds ((fun-code :function name :block name)))

(defform (let (&rest vars)
           &declarations decls
           &body body)
  :rest-patterns ((vars . (&or name (name init))))
  :binds ((init) (body :variable name)))

(defform (let* (&rest vars)
           &declarations decls
           &body body)
  :rest-patterns ((vars . (&or name (name init))))
  :binds ((init :variable (< name))
          (body :variable name)))

(defmethod get-location ((node let*-form) location)
  (trivia:cmatch (location-id location)
    ((eql 'op) 'let*)
    ((eql 'body) (body node))
    ((eql 'vars) (vars node))
    ((eql 'decls) (decls node))
    ((and (type integer) i) (nth i (body node)))
    ((list (eql 'vars) (and (type integer) i))
     (nth i (vars node)))
    ((list (eql 'vars) (and (type integer) i) (eql 0))
     (let ((p (nth i (vars node))))
       (if (symbolp p) p (car p))))
    ((list (eql 'vars) (and (type integer) i) (and (type integer) init))
     (let ((p (nth i (vars node))))
       (nth init p)))))

(defmethod update ((node let*-form) id new-value)
  (trivia:cmatch id
    ((eql 'body)
     (make-instance 'let*-form :vars (vars node) :body new-value :decls (decls node)))
    ((eql 'decls)
     (make-instance 'let*-form :vars (vars node) :body (body node) :decls new-value))
    ((eql 'vars)
     (make-instance 'let*-form :vars new-value :body (body node) :decls (decls node)))
    ((type integer)
     (let ((old-body (body node)))
       (make-instance 'let*-form :vars (vars node)
                                 :decls (decls node)
                                 :body (list-update old-body new-value id))))
    ((list (eql 'vars) (and (type integer) i))
     (let ((old-body (vars node)))
       (make-instance 'let*-form :vars (list-update old-body new-value i)
                                 :decls (decls node)
                                 :body (body node))))
    ((list (eql 'vars) (and (type integer) i) (eql 0))
     (let* ((old-body (vars node))
            (p (nth i old-body)))
       (make-instance 'let*-form :vars (list-update old-body (if (symbolp p)
                                                                 new-value
                                                                 (cons new-value (cdr p)))
                                                    i)
                                 :decls (decls node)
                                 :body (body node))))
    ((list (eql 'vars) (and (type integer) i) (and (type integer) p-idx))
     (let* ((old-body (vars node))
            (p (nth i old-body)))
       (make-instance 'let*-form :vars (list-update old-body (list-update p new-value p-idx) i)
                                 :decls (decls node)
                                 :body (body node))))))

(defform (alexandria:when-let* (&or (name init) (&rest vars))
           &body body)
  :rest-patterns ((vars . (name init)))
  :binds ((init :variable (< name))
          (body :variable name)))

(defform (flet (&rest funs)
           &declarations decls
           &body body)
  :rest-patterns ((funs . (name &lambda funcode)))
  :binds ((body :function name)
          (funcode :block (= name))))

(defform (labels (&rest funs)
           &declarations decls
           &body body)
  :rest-patterns ((funs . (name &lambda funcode)))
  :binds ((body :function name)
          (funcode :block (= name) :function name)))

(defform (macrolet (&rest macro-defs)
           &declarations decls
           &body body)
  :rest-patterns ((macro-defs . (name &macro-lambda macro-code)))
  :binds ((body :function (name macro-defs))
          (macro-code :block (= name))))

(defform (symbol-macrolet (&rest macro-code) ; does not define an eval context
           &declarations decls
           &body body)
  :rest-patterns ((macro-code . (name expansion)))
  :binds ((body :variable (name macro-code))))

(defform (block name &body body)
  :binds ((body :block name)))

(defform (defun name &lambda fun-code)
  :binds ((fun-code :block name :function name)))

(defform (lambda &lambda fun-code)
  :binds ((fun-code)))

(defform (defmacro name &macro-lambda macro-code)
  :binds ((macro-code :block name)))

(defform (function &or (fun-designator) ((%lambda-keyword &lambda fun-code)))
  :binds ((nil :function fun-designator) ; mark as binder
          (fun-code)))

(defform (quote thing))

(defform (if test then &body else) ; optional, but need structural editing
  :binds ((test) (then) (else)))

(defform (setq &body forms)
  :binds ((forms)))

(defform (return-from name value)
  :binds ((value)))

(defform (catch tag &body body)
  :binds ((tag) (body)))
(defform (throw tag result)
  :binds ((tag) (result)))

(defform (load-time-value form &body read-only-p)
  :binds ((form) (read-only-p)))

(defform (eval-when (&rest-qualifiers situations) &body body)
  :binds ((body)))

(defform (locally &declarations decls &body body)
  :binds ((body)))
(defform (the type-specifier form)
  :binds ((form)))

(defform (tagbody &body body) ; XXX tags should be stored in env and not walked
  :binds ((body)))
(defform (go tag))

(defform (unwind-protect protected &body cleanup)
  :binds ((protected) (cleanup)))

(defform (multiple-value-call fun arg &body body)
  :binds ((fun) (arg) (body)))

(defform (multiple-value-prog1 value-form &body body)
  :binds ((value-form) (body)))

(defform (progn &body forms)
  :binds ((forms)))
(defform (progv var-list val-list &body body) ; don't track dynamic bindings
  :binds ((var-list) (val-list) (body)))

(defun walk-form (form env on-form &optional (note-binder (constantly nil)))
  (if (atom form)
      (funcall on-form form env)
      (when (funcall on-form form env)
        (let ((name (car form)))
          (if-let (walker (gethash name *special-walkers*))
            (funcall walker
                     form env
                     (rcurry #'walk-form on-form note-binder)
                     note-binder)
            (multiple-value-bind (newform expanded-p)
                (env-macroexpand form env)
              (if expanded-p
                  (let ((newop (car newform)))
                    (if-let (walker (gethash newop *special-walkers*))
                      (funcall walker
                               newform env
                               (rcurry #'walk-form on-form note-binder)
                               note-binder)
                      ;; must be function call
                      (when (funcall on-form newform env)
                        (mapcar (rcurry #'walk-form env on-form note-binder)
                                (cdr newform)))))
                  (when (funcall on-form form env)
                    (mapcar (rcurry #'walk-form env on-form note-binder)
                            (cdr form))))))))))

;;
;;; macro analysis via perturbation
;;
;; TODO detect non-parametric, effectful macros
(defun macro-call-envmap (form env &optional (walker (constantly nil)))
  "Identifies body forms and binding scopes to return a map of {sexp -> binding env}.
Walks subforms of the call using WALKER during analysis."
  (let ((call-tree-forms (make-hash-table :test 'eq))
        (possible-identifiers (make-hash-table :test 'eq))
        (eval->binders (make-hash-table :test 'eq)))
    ;; identify all forms in the original call for classification
    ;; some may be constants, others are binders and expressions
    ;; record their source sym-paths for reconstruction
    (labels ((walk-call-collecting-forms (form path)
               (loop for i from 0
                     for subform in form
                     for newpath = (cons i path)
                     do (if (consp subform)
                            (progn
                              (push newpath (gethash subform call-tree-forms))
                              (walk-call-collecting-forms subform newpath))
                            (when (and (symbolp subform))
                              (push newpath (gethash subform call-tree-forms)))))))
      (walk-call-collecting-forms form (list)))
    ;; - macroexpand fully up to special (or hardwired macro) forms,
    ;;   and record all binders and obvious evaluation contexts seen in the output
    ;; - call the walker meanwhile
    (walk-form (env-macroexpand form env) env
               (lambda (form env)
                 (when (symbolp form)
                   (setf (gethash form possible-identifiers) :ref))
                 ;; continue if this isn't an cons, or not part of the call
                 (if (and (consp form) (gethash form call-tree-forms))
                     (progn
                       (funcall walker form env) ; recurse subcalls with WALKER
                       (ensure-gethash form eval->binders)
                       nil)
                     t))
               (lambda (sym kind)
                 (setf (gethash sym possible-identifiers) kind)))
    (disp (hash-table-plist call-tree-forms))
    (disp (hash-table-plist possible-identifiers))
    ;; analysis, first to understand syntax before constructing the returned maps
    (let ((gensym->name (make-hash-table :test 'eq))
          (name->gensym (make-hash-table :test 'eq))
          (gensym->refpath (make-hash-table :test 'eq)))
      (labels ((note-ident (name gensym)
                 (setf (gethash gensym gensym->name) name
                       (gethash name name->gensym) gensym))
               ;; substitute the symbol on PATH in FORM for another
               (substitute-sym (path form sym)
                 (loop for rest on (reverse path)
                       for i := (car rest)
                       do (if (null (cdr rest))
                              (setf (nth i form) sym)
                              (setf form (nth i form)))))
               ;; (lookup-path (path form)
               ;;   (loop for i in (reverse path)
               ;;         do (setf form (nth i form))
               ;;         finally (return form)))
               )
        ;; we need to know where the refs and binders are first
        (loop for sym being the hash-keys of possible-identifiers using (hash-value kind)
              do (loop for path in (gethash sym call-tree-forms)
                       for gensym := (gensym "PB")
                       for ident-kind := nil
                       do (substitute-sym path form gensym)
                          (let ((expansion (handler-case (env-macroexpand form env)
                                             (error () +fail+))))
                            (unless (eq expansion +fail+)
                              (block walk
                                (walk-form expansion env
                                           (lambda (form env)
                                             (declare (ignore env))
                                             (cond ((gethash form call-tree-forms) nil)
                                                   ((eq form gensym)
                                                    (setf ident-kind :ref)
                                                    (return-from walk))
                                                   (t t)))
                                           (lambda (sym kind)
                                             (when (and (eq sym gensym) (eq kind :variable))
                                               (setf ident-kind :variable)
                                               (return-from walk)))))))
                          ;; an actual reference will be independent of the binding name
                          ;; counter: step forms may have spurious refs not from the call
                          (if ident-kind
                              (progn
                                (note-ident sym gensym)
                                (when (eq ident-kind :ref)
                                  (setf (gethash gensym gensym->refpath) path)))
                              (substitute-sym path form sym))))
        (disp (hash-table-plist gensym->refpath))
        ;;
        (let ((expansion (handler-case (env-macroexpand form env)
                           (error () +fail+))))
          (unless (eq expansion +fail+)
            (walk-form
             expansion env
             (lambda (form subenv)
               (if (or (and (consp form) (gethash form call-tree-forms))
                       (gethash form gensym->refpath))
                   ;; TODO what about macro-introduced bindings like `it`?
                   ;;      heuristic to distinguish from gensyms?
                   (progn
                     (setf (gethash form eval->binders)
                           (remove-if-not
                            (lambda (v)
                              (loop for i being the hash-keys of gensym->name
                                    thereis (eq v i)))
                            (ldiff (variable-bindings subenv) (variable-bindings env))))
                     nil)
                   t)))))
        (disp (hash-table-plist eval->binders))
        (disp form)
        (values gensym->name eval->binders)))))

;; test:
;; (loop for it from 1 to 10
;;       if (evenp it)
;;         collect it)

;; test:
;; (loop for it from from
;;       do (print it))

(defun parse (form env)
  (if (atom form)
      (if (symbolp form)
          (make-instance 'symbol-ref :name form)
          (error "found atom ~a, not symbol" form))
      (let ((name (car form)))
        (if-let (parser (gethash name *special-parsers*))
          (funcall parser form env #'parse)
          (multiple-value-bind (result local-expansion)
              (env-function-info name env)
            (flet ((parse-function (form)
                     (make-instance 'function-call
                                    :name (parse (car form) env)
                                    :body (mapcar (rcurry #'parse env) (cdr form))))
                   ;; don't expand explicitly, we only care about explicit call subforms
                   (parse-macro (form)
                     (let ((macro-subforms (make-hash-table :test 'eq)))
                       (multiple-value-bind (gensym->name eval->binders)
                           (macro-call-envmap form env
                                              (lambda (form env)
                                                (setf (gethash form macro-subforms)
                                                      (parse form env))))
                         (make-instance 'macro-call :op name :body (cdr form)
                                                    :subform-asts macro-subforms
                                                    :eval-binders eval->binders
                                                    :gensym-names gensym->name)))))
              (cond ((null result) ; global
                     (if (and (symbolp name) ; could be lambda in function position
                              (macro-function name) (not (hardwired-p name)))
                         (parse-macro form)
                         (parse-function form)))
                    ;; local
                    ((null local-expansion) (parse-function form))
                    (t (parse-macro form)))))))))
