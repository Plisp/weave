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
           #:is-atom #:get-body
           #:copy-node

           #:update
           #:get-location #:getloc

           #:location-sort #:locsort

           #:eval-form #:symbol-ref #:binder #:function-call #:literal
           #:unevaluated
           #:body #:name #:str #:vars #:op
           ))
(in-package #:weave-parser)

;;
;;; class defs: mainly we want a structure that's
;;; - close enough to s-expressions for macroexpansion and evaluation
;;; - has tags for tree-structured dispatch (minimal passing of context through wrappers)
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
         :initform :line :type (or (eql :line) (eql :block))
         :accessor kind))
  (:documentation ""))

(defclass atom-form ()
  ())

(defclass eval-form ()
  ((typ :initform nil
        :accessor typ))
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

(defstruct location
  "`id's usually contain a symbol (slot), possibly list index and should respect `cl:equal'.
These are specific to the `node' type."
  (node (error "must provide parent node"))
  (id nil))

(defmethod print-object ((object comment) stream)
  (format stream "<~a>" (str object)))

(defmethod print-object ((object literal) stream)
  (format stream "<~a>" (str object)))

(defmethod print-object ((object symbol-ref) stream)
  (pprint-logical-block (stream (list))
    (format stream "<v ~s@~a>" (name object) (addr-str object))))

(defmethod print-object ((object binder) stream)
  (pprint-logical-block (stream (list))
    (format stream "<b ~s@~a>" (name object) (addr-str object))))

(defmethod print-object ((object function-call) stream)
  (pprint-logical-block (stream (body object) :suffix ")>")
    (write-string "<fn(" stream)
    (write (name object) :stream stream)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (print-object (pprint-pop) stream))))

(defmethod print-object ((object macro-call) stream)
  (pprint-logical-block (stream (body object))
    (write-string "<mc(" stream)
    (write (op object) :stream stream)
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\Space stream)
      (print-object (pprint-pop) stream)))
  (write-string ")>" stream))

;;
;;; interface
;;

;; performs a deep copy of node, only needed for actual node classes,
;; - semantic uniqueness of binders, used for attaching information
;; - uniqueness of locations used for the user interface
(defgeneric copy-node (node)
  (:method ((o comment)) (make-instance 'comment :kind (kind o) :str (str o)))
  (:method ((o literal)) (make-instance 'literal :str (str o)))
  (:method ((o symbol-ref)) (make-instance 'symbol-ref :name (name o)))
  (:method ((o binder)) (make-instance 'binder :name (name o)))
  (:method ((o function-call)) (make-instance 'function-call
                                              :name (copy-node (name o))
                                              :body (mapcar #'copy-node (body o))))
  (:method ((o macro-call))
    (make-instance 'macro-call
                   :subform-asts (let ((new (make-hash-table :test #'eq)))
                                   (maphash (lambda (k v) (setf (gethash k new) (copy-node v)))
                                            (subform-asts o))
                                   new)
                   :eval-binders (eval-binders o)
                   :body (body o) :op (op o)))
  (:method ((o function-code))
    (make-instance 'function-code
                   :lambda-list (copy-node (lambda-list o))
                   :docstring (docstring o)
                   :declarations (declarations o)
                   :body (mapcar #'copy-node (body o)))))

(defgeneric is-atom (node)
  (:method (node) nil))
(defmethod is-atom ((node binder)) t)
(defmethod is-atom ((node literal)) t)
(defmethod is-atom ((node symbol-ref)) t)

(defgeneric get-body (node)
  (:method (node) (values nil nil))
  (:method ((node function-call)) (values (body node) t))
  (:method ((node macro-call)) (values (body node) t))
  (:method ((node function-code)) (values (body node) t)))

(defgeneric to-text (ast) (:documentation "Serialize ast to text"))
(defgeneric to-sexp (ast) (:documentation "convert to sexp for macroexpansion")
  (:method ((ast t)) nil))

(defgeneric location-sort (node id))
(defgeneric get-location (node id)
  (:documentation "Returns the current value at `id'."))
(defun getloc (location)
  (get-location (location-node location) (location-id location)))
(defun locsort (location)
  (location-sort (location-node location) (location-id location)))

(defgeneric update (node id new-value)
  (:documentation "Functionally updates the location corresponding to `id',
returns a new node with all child nodes identical except the hole indicated.
List structure may share conses with the old node."))

(defun location= (n1 n2)
  (and (eq (location-node n1) (location-node n2))
       (equal (location-id n1) (location-id n2))))

(defmethod get-location ((node function-call) id)
  (trivia:cmatch id
    ((eql 'name) (name node))
    ((eql 'body) (body node))
    ((list (eql 'body) (and (type integer) i)) (nth i (body node)))))

(defmethod update ((node function-call) id new-value)
  (trivia:cmatch id
    ((eql 'name)
     (make-instance 'function-call :name new-value :body (body node)))
    ((eql 'body)
     (make-instance 'function-call :name (name node) :body new-value))
    ((list (eql 'body) (and (type integer) i))
     (let ((old-body (body node)))
       (make-instance 'function-call :name (name node)
                                     :body `(,@(subseq old-body 0 i)
                                             ,new-value
                                             ,@(nthcdr (1+ i) old-body)))))))

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
    "variable-bindings: v or (v macroexpansion)
function-bindings: f or (f macro-params-body)
copy-env can exploit structure sharing, remember to PUSH!"
    (%function-bindings (list))
    (%variable-bindings (list))
    (%blocks (list))
    (%tags (list)))

  (defmethod make-load-form ((o env) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots o))

  (define-constant +nullenv+ (make-env) :test #'equalp))

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
(defparameter *hardwired-operators* #(lambda defun defmethod defmacro)
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
               (macroexpand-1 x)))) ; possible global symbol macro
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

(defun unwrap-refs (maybe-symbol-ref)
  (if (typep maybe-symbol-ref 'symbol-ref)
      (name maybe-symbol-ref)
      maybe-symbol-ref))

(deftype symbol-like () '(or symbol symbol-ref))

;; we need to be more permissive for lambda lists. It makes little sense to preserve
;; well-formedness when it often breaks with edits due to positional &keyword context
;; e.g. (&key (a _) (b _)) -?> (a b)  or  (&optional (b _) c) -?> (b &optional c)
(defun map-lambda-list (list on-binder value-mapper specializer-list-p
                        &optional destructure-p alter-identity)
  "Requires a proper list but otherwise doesn't force well-formedness and tries to be
very tolerant. Reconstructs the list structure from the return values of ON-BINDER and
VALUE-MAPPER, notifying ALTER-IDENTITY, if given, of every newly-built cons for comment
provenance. DESTRUCTURE-P allows &optional/&rest/&body vars, and &key vars as a
(keyword-name var) pair, to themselves be nested macro lambda lists (CLHS 3.4.4)."
  ;; note: dot is treated as a symbol-ref by reading, but dotted lists shouldn't come up
  ;; even during macroexpansion since lambda list are nested in evaluation contexts
  (let ((new
          (loop with current-keyword := nil
                with res := (list)
                for elt in list
                for keyword := (car (member (unwrap-refs elt) lambda-list-keywords))
                do (labels ((bind-var (v)
                              (cond ((typep v 'symbol-like) (funcall on-binder v))
                                    ((and destructure-p (consp v))
                                     (map-macro-lambda v on-binder value-mapper
                                                       alter-identity))
                                    (t (funcall value-mapper v))))
                            ;; note: only meaningful for a specializer list
                            (maybe-default (val)
                              (if (and specializer-list-p (not current-keyword))
                                  val
                                  (funcall value-mapper val))))
                     (if keyword
                         (progn (setf current-keyword keyword) (push elt res))
                         (push
                          (case current-keyword
                            ((&rest &body) (bind-var elt))
                            (&key
                             (trivia:match elt
                               ((or (and (type symbol-like) v)
                                    ;; keyword or method-like
                                    (list (and (type symbol-like) v)))
                                (funcall on-binder v))
                               ;; keyword name, optionally wrapping a pattern
                               ((list (list kw-name v) val)
                                (let* ((inner `(,kw-name ,(bind-var v)))
                                       (new `(,inner ,(funcall value-mapper val))))
                                  (when alter-identity
                                    (funcall alter-identity (first elt) inner)
                                    (funcall alter-identity elt new))
                                  new))
                               ;; supplied-p
                               ((list (list kw-name v) val
                                      (and (type symbol-like) supplied-p))
                                (let* ((inner `(,kw-name ,(bind-var v)))
                                       (new `(,inner ,(funcall value-mapper val)
                                                     ,(funcall on-binder supplied-p))))
                                  (when alter-identity
                                    (funcall alter-identity (first elt) inner)
                                    (funcall alter-identity elt new))
                                  new))
                               ;; default value, no keyword-name wrapper
                               ((list (and (type symbol-like) v) val)
                                (let ((new `(,(funcall on-binder v)
                                             ,(funcall value-mapper val))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               ((list (and (type symbol-like) v) val
                                      (and (type symbol-like) supplied-p))
                                (let ((new `(,(funcall on-binder v)
                                             ,(funcall value-mapper val)
                                             ,(funcall on-binder supplied-p))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               (_ (funcall value-mapper elt))))
                            (&optional
                             (trivia:match elt
                               ((or (and (type symbol-like) v)
                                    (list (and (type symbol-like) v)))
                                (funcall on-binder v))
                               ((list v val)
                                (let ((new `(,(bind-var v) ,(funcall value-mapper val))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               ((list v val (and (type symbol-like) supplied-p))
                                (let ((new `(,(bind-var v) ,(funcall value-mapper val)
                                             ,(funcall on-binder supplied-p))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               (_ (funcall value-mapper elt))))
                            (t
                             (trivia:match elt
                               ((or (and (type symbol-like) v)
                                    ;; keyword or method-like
                                    (list (and (type symbol-like) v)))
                                (funcall on-binder v))
                               ;; default value or specializer
                               ((list (and (type symbol-like) v) val)
                                (let ((new `(,(funcall on-binder v) ,(maybe-default val))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               ((list (and (type symbol-like) v) val
                                      (and (type symbol-like) supplied-p))
                                (let ((new `(,(funcall on-binder v) ,(maybe-default val)
                                             ,(funcall on-binder supplied-p))))
                                  (when alter-identity (funcall alter-identity elt new))
                                  new))
                               (_ (funcall value-mapper elt)))))
                          res)))
                finally (return (nreverse res)))))
    (when alter-identity (funcall alter-identity list new))
    new))

(defun map-macro-lambda (list on-binder value-mapper &optional alter-identity)
  (let ((new
          (loop with res := (list)
                for rest on list
                for this = (car rest)
                do (cond ((member (unwrap-refs this) '(&body &rest &key &optional &aux)
                                  :test #'eq)
                          (return (nreconc res (map-lambda-list rest on-binder value-mapper
                                                                nil t alter-identity))))
                         ((member (unwrap-refs this) lambda-list-keywords) (push this res))
                         ((consp this)
                          (push (map-macro-lambda this on-binder value-mapper alter-identity)
                                res))
                         ((null (unwrap-refs this)) (push this res))
                         ((symbolp (unwrap-refs this)) (push (funcall on-binder this) res))
                         (t (push (funcall value-mapper this) res)))
                finally (return (nreverse res)))))
    (when alter-identity (funcall alter-identity list new))
    new))

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

(defparameter *special-walkers* (make-hash-table :test #'eq))
(defparameter *special-parsers* (make-hash-table :test #'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parser-keywords* '(&rest &body &or &declarations &rest-qualifiers
                                    &lambda &method-lambda &macro-lambda))
  (defparameter *arity-1-parser-keywords* '(&rest &body
                                            &lambda &method-lambda &macro-lambda))
  (defun spec-kinds (spec)
    "Validates the spec and returns an alist of tag names and their arity-1 keyword."
    (flet ((spec-keyword-p (sym)
             (member sym *parser-keywords* :test #'eq)))
      (cond ((null spec) nil)
            ((consp (car spec)) ; list pattern
             (append (spec-kinds (car spec)) (spec-kinds (cdr spec))))
            ;; spec keyword, expect a non-NIL symbol immediately after, except for &or
            ((spec-keyword-p (car spec))
             (assert (or (not (eq (car spec) '&or))
                         (notany #'spec-keyword-p (cdr spec))))
             (cond ((member (car spec) *arity-1-parser-keywords* :test #'eq)
                    (assert (= 2 (length spec)))
                    (list (cons (second spec) (car spec))))
                   ((member (car spec) '(&declarations &rest-qualifiers) :test #'eq)
                    (cons (cons (second spec) (car spec)) (spec-kinds (cddr spec))))
                   (t
                    (spec-kinds (cdr spec)))))
            (t ; lone patterns must be symbols
             (assert (symbolp (car spec)))
             (cons (list (car spec)) (spec-kinds (cdr spec)))))))

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
    (body nil))

  (defun eval-ctx-p (tag binds)
    (loop for (ctx) in binds thereis (eq tag ctx)))

  (defun check-binds (tag-kinds rest-patterns binds)
    (let ((bind-rest-tags nil))
      ;; &body must be evaluated
      (loop for (sym . kind) in tag-kinds
            do (when (eq kind '&body)
                 (assert (eval-ctx-p sym binds))))
      ;; check consistency of binds
      (loop
        for (ctx . entries) in binds
        do (assert (symbolp ctx))
           ;; rest forms must not be evaluated
           (assert (not (eq '&rest (cdr (assoc ctx tag-kinds)))))
           (loop
             with special-seen := nil
             for (kind bind-tag) on entries by #'cddr
             do (trivia:ematch bind-tag
                  ((type symbol))
                  ((list (or (eql '<) (eql '=)) (and (type symbol) bind-name))
                   ;; only one allowed for now, change when pattern matching
                   (assert (not special-seen))
                   (setf special-seen t)
                   ;; for now = only for blocks, seq binding only for variables
                   (when (eq (car bind-tag) '=) (assert (eq kind :block)))
                   (when (eq (car bind-tag) '<) (assert (eq kind :variable)))
                   (loop
                     for (rest-tag . pattern) in rest-patterns
                     do (when (search-tree bind-name pattern)
                          (assert (search-tree ctx pattern))
                          (assert
                           (trivia:match pattern
                             ((list (eql bind-name) (eql ctx))
                              (push (cons bind-name rest-tag) bind-rest-tags))
                             ((list (eql bind-name) keyword (eql ctx))
                              (when (member keyword *arity-1-parser-keywords*)
                                (push (cons bind-name rest-tag) bind-rest-tags)))
                             ((list* (eql '&or) rest)
                              (loop
                                for pattern in rest
                                thereis (trivia:match pattern
                                          ((list (eql bind-name) (eql ctx))
                                           (push (cons bind-name rest-tag)
                                                 bind-rest-tags))
                                          ((list (eql bind-name) keyword (eql ctx))
                                           (when (member keyword *arity-1-parser-keywords*)
                                             (push (cons bind-name rest-tag)
                                                   bind-rest-tags)))))))))))
                  ;; non < = so macro binding
                  ((list (type symbol) (type symbol))))))
      bind-rest-tags))

  ;;; location method generation
  (defun bind-tag-name (bind-tag)
    "The tag named by a BINDS plist entry: X, (< X), (= X) and (X data) all name X."
    (if (consp bind-tag)
        (if (member (car bind-tag) '(< =)) (second bind-tag) (first bind-tag))
        bind-tag))

  (defun binder-tag-p (tag binds)
    (loop for (nil . entries) in binds
          thereis (loop for (nil bind-tag) on entries by #'cddr
                        thereis (eq tag (bind-tag-name bind-tag)))))

  (defun tag-sort (tag binds)
    "The location-sort of a spec tag."
    (cond ((eval-ctx-p tag binds) 'eval-form)
          ((binder-tag-p tag binds) 'binder)
          (t 'unevaluated)))

  (defun rest-pattern-tags (pattern)
    (trivia:ematch pattern
      ((list (eql '&or) (type symbol) inner) (rest-pattern-tags inner))
      ((list binder-tag value-kind value-tag)
       (values binder-tag value-tag value-kind))
      ((list binder-tag value-tag)
       (values binder-tag value-tag nil))))

  (defun location-methods (name classname spec-kinds rest-patterns binds)
    "Emits the get-location/update/location-sort methods for a defform class.
PARTS is the SPEC-NAMES alist (tag . arity-1-keyword). Elements of a list slot
are addressed by (name index) and rest-patterns by (name index i)."
    (let ((slots (mapcar #'car spec-kinds))
          (get-clauses (list))
          (update-clauses (list))
          (sort-clauses (list)))
      (flet ((rebuild (slot value)
               `(make-instance ',classname
                               ,@(loop for s in slots
                                       append `(,(make-keyword s)
                                                ,(if (eq s slot) value `(,s node)))))))
        (push `((eql 'op) ',name) get-clauses)
        (loop
          for part in slots
          for kind = (cdr (assoc part spec-kinds))
          do (push `((eql ',part) (,part node)) get-clauses)
             (push `((eql ',part) ,(rebuild part 'new-value)) update-clauses)
             (when (member kind '(&rest &body))
               (push `((list (eql ',part) (and (type integer) i))
                       (nth i (,part node)))
                     get-clauses)
               (push `((list (eql ',part) (and (type integer) i))
                       ,(rebuild part `(list-update (,part node) new-value i)))
                     update-clauses))
             ;; sorts
             (case kind
               (&body
                (push `((list (eql ',part) (type integer)) 'eval-form) sort-clauses))
               (&rest
                (when-let (pattern (cdr (assoc part rest-patterns)))
                  (multiple-value-bind (binder-tag value-tag value-kind)
                      (rest-pattern-tags pattern)
                    ;; internally normalize lone binders to a list
                    (push `((list (eql ',part) (and (type integer) i)
                                  (and (type integer) j))
                            (nth j (ensure-list (nth i (,part node)))))
                          get-clauses)
                    (push `((list (eql ',part) (and (type integer) i)
                                  (and (type integer) j))
                            ,(rebuild part `(list-update
                                             (,part node)
                                             (list-update
                                              (ensure-list (nth i (,part node)))
                                              new-value j)
                                             i)))
                          update-clauses)
                    ;; this has to be checked first
                    (push `((list (eql ',part) (type integer) (eql 0))
                            ',(tag-sort binder-tag binds))
                          sort-clauses)
                    ;; logic for body, or singular value
                    (when (member value-kind '(nil &body))
                      (push `((list (eql ',part) (type integer)
                                    ,(if (eq value-kind '&body)
                                         '(type integer)
                                         '(eql 1)))
                              ',(tag-sort value-tag binds))
                            sort-clauses)))))
               ((nil)
                (push `((eql ',part) ',(tag-sort part binds)) sort-clauses)))))

      `((defmethod get-location ((node ,classname) id)
          (trivia:cmatch id ,@(nreverse get-clauses)))
        (defmethod update ((node ,classname) id new-value)
          (trivia:cmatch id ,@(nreverse update-clauses)))
        (defmethod location-sort ((node ,classname) id)
          (trivia:match id ,@(nreverse sort-clauses)))))))

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
Any binding forces a symbol match.

`on-binder' may be called multiple times by the walker, ensure idempotency."
  (let* ((spec-parser-name (symbolicate name "-SPEC-PARSER"))
         (tag-kinds (nconc (spec-kinds spec)
                           (mapcan (lambda (pair) (spec-kinds (cdr pair)))
                                   rest-patterns)))
         (bind-rest-tags (check-binds tag-kinds rest-patterns binds))
         (classname (symbolicate name "-FORM"))
         (slots (mapcar #'car (spec-kinds spec))))
    `(progn
       (defclass ,classname (irregular-form)
         (,@(loop for name in (remove-duplicates slots :test #'equal)
                  collect `(,name :initarg ,(make-keyword name)
                                  :accessor ,name))))
       ;; methods
       ,(when (member 'body slots)
          `(defmethod get-body ((node ,classname)) (values (body node) t)))
       (defmethod copy-node ((old ,classname))
         (let ((new (make-instance ',classname)))
           ,@(loop for name in (remove-duplicates slots :test #'equal)
                   collect `(setf (,name new) (copy-node (,name old))))))
       ,@(location-methods name classname (spec-kinds spec) rest-patterns binds)
       ;; exports
       (export ',classname)
       ,@(loop for name in (remove-duplicates slots :test #'equal)
               collect `(export ',name))
       ;; spec validated above by spec-kinds ^
       (defmacro ,spec-parser-name (spec)
         (with-gensyms (form body decls doc qualifiers)
           (cond
             ((null spec) ; no more entries in current list, not forced by spec keyword
              `(lambda (,form)
                 (unless (null ,form)
                   (form-parse-error "expected null, got ~a" ,form))))
             ((atom spec)
              `(lambda (,form)
                 (when (and ,(binder-tag-p spec ',binds) ; check symbol if binder
                            (not (or (typep ,form 'symbol) (typep ,form 'symbol-ref))))
                   (form-parse-error "expected binder symbol match: ~a" ,form))
                 (push ,form ,spec)))
             ((consp (car spec))
              `(lambda (,form)
                 (if (listp (car ,form))
                     (progn (funcall (,',spec-parser-name ,(car spec)) (car ,form))
                            (funcall (,',spec-parser-name ,(cdr spec)) (cdr ,form)))
                     (form-parse-error "list expected, got ~a, context ~a" ,form ',spec))))
             (t ; (atom (car spec))
              (case (car spec)
                (&declarations
                 `(lambda (,form)
                    (multiple-value-bind (,body ,decls)
                        (parse-body-declarations ,form nil)
                      (push ,decls ,(second spec))
                      (funcall (,',spec-parser-name ,(cddr spec)) ,body))))
                (&or
                 `(lambda (,form)
                    (block nil
                      ,@(mapcar
                         (lambda (pattern)
                           `(handler-case
                                (return (funcall (,',spec-parser-name ,pattern) ,form))
                              (form-parse-error () nil)))
                         (cdr spec))
                      (form-parse-error "expected one of ~a got ~a" ',(cdr spec) ,form))))
                ((&method-lambda &lambda &macro-lambda)
                 `(lambda (,form)
                    (when (null ,form)
                      (form-parse-error "missing lambda list"))
                    (multiple-value-bind (,body ,decls ,doc)
                        (parse-body-declarations (cdr ,form) t)
                      (push (make-function-info :arglist (car ,form)
                                                :documentation ,doc
                                                :decls ,decls
                                                :body ,body)
                            ,(second spec)))))
                (&body
                 `(lambda (,form)
                    (push ,form ,(second spec))))
                (&rest
                 `(lambda (,form)
                    ,(if-let (pattern (cdr (assoc (second spec) ',rest-patterns)))
                       `(progn
                          (mapc (lambda (form) (funcall (,',spec-parser-name ,pattern) form))
                                ,form)
                          (push ,form ,(second spec)))
                       `(push ,form ,(second spec)))))
                (&rest-qualifiers
                 `(lambda (,form)
                    (let ((,qualifiers
                            (loop for v := (car ,form)
                                  while (or (typep v 'symbol) (typep v 'symbol-ref))
                                  collect v
                                  do (pop ,form))))
                      (push ,qualifiers ,(second spec))
                      (funcall (,',spec-parser-name ,(cddr spec)) ,form))))
                (t ; symbol match car... against e....
                 `(lambda (,form)
                    (if (consp ,form)
                        (progn (funcall (,',spec-parser-name ,(car spec)) (car ,form))
                               (funcall (,',spec-parser-name ,(cdr spec)) (cdr ,form)))
                        (form-parse-error "expected non-nil car: ~a" ,form)))))))))
       ;; this merely does validation and preserves identity of all checked lists
       (defmacro ,(symbolicate "WITH-PARSED-" name) (form &body body)
         `(let (,@',(remove-duplicates (mapcar #'car tag-kinds)))
            (funcall (,',spec-parser-name ,',spec) (cdr ,form))
            ,@body))

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
                                       `(apply #'append ,whole-tag))
                                      ((type symbol) ; binding records may SHARE STRUCTURE
                                       record))))
                                (:function
                                 `(env-with-functions
                                   ,env-exp
                                   ,(trivia:ematch record
                                      ((list (type symbol) whole-tag)
                                       `(loop
                                          for (name . info) in (apply #'append ,whole-tag)
                                          collect
                                          (list* name (function-info-arglist info)
                                                 (function-info-body info))))
                                      ((type symbol) record))))
                                (:block `(env-with-blocks ,env-exp ,record))))
                          rest))
                       env-exp)))
          `(progn
             ;; the walker operates on raw code
             (defun ,(symbolicate name "-WALKER") (rawform env walker on-binder)
               (declare (ignorable env walker on-binder))
               (,(symbolicate "WITH-PARSED-" name) rawform
                ,@(loop for (ctx-tag . entries) in binds
                        append ; shapes validated by `check-binds'
                        (loop for (kind record) on entries by #'cddr
                              while record
                              collect (with-gensyms (b)
                                        `(loop for ,b in ,(bind-tag-name record)
                                               do (funcall on-binder ,b ,kind)))))
                ,@
                (loop
                  for (ctx-tag . %entries) in binds
                  for entries := (plist-alist %entries)
                  collect ; (block . (= v)) -> v
                  (let ((seq-tag (third (rassoc-if #'sequential-p entries)))
                        (par-tag (third (rassoc-if #'parallel-p entries)))
                        (ctx-kind (cdr (assoc ctx-tag tag-kinds))))
                    (flet ((walk-function-body (env info augmented-body)
                             `(loop initially
                               (flet ((note-binder (binder)
                                        (setf newenv-with-params
                                              (env-with-variables newenv-with-params
                                                                  `(,binder)))
                                        (funcall on-binder binder :variable)))
                                 ;; capture of newenv-with-params to pass binder info
                                 ,(if (eq ctx-kind '&macro-lambda)
                                      `(map-macro-lambda
                                        (function-info-arglist ,info)
                                        #'note-binder
                                        (lambda (form) ; capture direct reference vvv
                                          (funcall walker form newenv-with-params)))
                                      `(map-lambda-list
                                        (function-info-arglist ,info)
                                        #'note-binder
                                        (lambda (form)
                                          (funcall walker form newenv-with-params))
                                        ,(eq ctx-kind '&method-lambda))))
                                    with newenv-with-params := ,env
                                    for body-form in (function-info-body ,info)
                                    do (funcall walker body-form ,augmented-body))))
                      (cond
                        (seq-tag ; only for let*-style variable bindings
                         (let ((whole-tag (cdr (assoc seq-tag bind-rest-tags))))
                           (with-gensyms (newenv whole initform)
                             `(or
                               ;; first detect whether a rest pattern was matched at all
                               ;; e.g. alexandria:when-let*
                               (when ,whole-tag
                                 (loop
                                   for ,newenv := ,(augment-env `env entries)
                                     then (if (symbolp ,whole)
                                              (env-with-variables ,newenv `(,,whole))
                                              (env-with-variables ,newenv `(,(car ,whole))))
                                   for ,whole in (first ,whole-tag)
                                   do (when (listp ,whole)
                                        ,(if ctx-kind
                                             `(loop for bodyform in (cdr ,whole)
                                                    do (funcall walker bodyform ,newenv))
                                             `(funcall walker (second ,whole) ,newenv))))
                                 t)
                               ;; (multiple) bodies have no identity? unused
                               (loop for ,initform
                                       in ,(if ctx-kind `(apply #'append ,ctx-tag) ctx-tag)
                                     do (funcall walker ,initform env))))))
                        (par-tag ; basically specific to flet
                         (ecase ctx-kind
                           ((&body &rest &declarations &rest-qualifiers nil)
                            (error "unimplemented"))
                           ((&lambda &macro-lambda &method-lambda)
                            (let* ((whole (cdr (assoc par-tag bind-rest-tags)))
                                   (code-tag (lastcar (cdr (assoc whole rest-patterns)))))
                              (with-gensyms (newenv name info)
                                `(loop
                                   with ,newenv := ,(augment-env `env entries)
                                   ;; tag ref here, not passed to walk-function-body
                                   for ,name in ,par-tag
                                   for ,info in ,code-tag
                                   ;; parallel bind the block only in the body
                                   ;; newenv-with-params is CAPTURED by walk-fn-body
                                   do ,(walk-function-body
                                        newenv info `(env-with-blocks newenv-with-params
                                                                      `(,,name)))))))))
                        (t
                         (ecase ctx-kind
                           ((&declarations &rest-qualifiers))
                           ((&body &rest nil)
                            (with-gensyms (newenv body-form)
                              `(loop with ,newenv := ,(augment-env `env entries)
                                     for ,body-form
                                       in ,(if ctx-kind `(apply #'append ,ctx-tag) ctx-tag)
                                     do (funcall walker ,body-form ,newenv))))
                           ((&lambda &macro-lambda &method-lambda)
                            (with-gensyms (newenv info)
                              `(loop with ,newenv := ,(augment-env `env entries)
                                     for ,info in ,ctx-tag
                                     do ,(walk-function-body newenv info ; vv CAPTURED
                                                             `newenv-with-params))))))))))))
             ;; alter-identity should return the new form
             (defun ,(symbolicate name "-PARSER") (rawform env walker alter-identity)
               (declare (ignorable env walker alter-identity))
               (,(symbolicate "WITH-PARSED-" name) rawform
                (let ((ast (make-instance ',classname)))
                  ,@
                  (loop
                    for tag in slots
                    for entries := (plist-alist (cdr (assoc tag binds)))
                    for tag-kind := (cdr (assoc tag tag-kinds))
                    collect
                    (flet ((walk-function-body (env info augment-body)
                             `(loop
                                with binder-env := +nullenv+
                                with newenv-with-params := ,env
                                with body := (list)
                                with lambda-list
                                  := (flet ((note-binder (binder)
                                              (setf newenv-with-params
                                                    (env-with-variables newenv-with-params
                                                                        `(,(name binder))))
                                              (setf binder-env
                                                    (env-with-variables binder-env
                                                                        `(,(name binder))))
                                              (change-class binder 'binder)))
                                       ,(if (eq tag-kind '&macro-lambda)
                                            `(map-macro-lambda
                                              (function-info-arglist ,info)
                                              #'note-binder
                                              (lambda (form)
                                                (funcall walker form newenv-with-params))
                                              alter-identity)
                                            `(map-lambda-list
                                              (function-info-arglist ,info)
                                              #'note-binder
                                              (lambda (form)
                                                (funcall walker form newenv-with-params))
                                              ,(eq tag-kind '&method-lambda)
                                              nil alter-identity)))
                                for body-form in (function-info-body ,info)
                                for body-ast = (funcall walker body-form ,augment-body)
                                do (push body-ast body)
                                finally
                                   (return
                                     (make-instance
                                      'function-code
                                      :docstring (function-info-documentation ,info)
                                      :declarations (function-info-decls ,info)
                                      :body (nreverse body)
                                      :lambda-list lambda-list)))))
                      (cond
                        ;; &rest special logic
                        ((eq tag-kind '&rest)
                         (trivia:ematch (cdr (assoc tag rest-patterns))
                           ((or (list name-tag value-tag)
                                (list name-tag (eql '&body) value-tag)
                                ;; let style binding
                                (list (eql '&or) name-tag
                                      (list (eql name-tag) (eql '&body) value-tag)))
                            (assert (and (symbolp name-tag) (symbolp value-tag)))
                            ;; let*-like
                            (let ((init-binds (plist-alist (cdr (assoc value-tag binds)))))
                              (if (rassoc-if #'sequential-p init-binds)
                                  (with-gensyms (binder-env res newenv whole new-whole b)
                                    `(loop
                                       with ,binder-env := +nullenv+
                                       with ,res := (list)
                                       for ,newenv := ,(augment-env `env init-binds)
                                         then (if (typep ,whole 'binder)
                                                  (env-with-variables ,newenv `(,,whole))
                                                  (env-with-variables ,newenv
                                                                      `(,(car ,whole))))
                                       for ,whole in (first ,tag)
                                       do (if (typep ,whole 'symbol-ref)
                                              (progn
                                                (setf ,binder-env
                                                      (env-with-variables ,binder-env
                                                                          `(,,whole)))
                                                (push (change-class ,whole 'binder) ,res))
                                              (let* ((,b (first ,whole))
                                                     (,new-whole
                                                       `(,(change-class ,b 'binder)
                                                         ,@(mapcar
                                                            (rcurry walker ,newenv)
                                                            (cdr ,whole)))))
                                                (funcall alter-identity ,whole ,new-whole)
                                                (push ,new-whole ,res)
                                                (setf ,binder-env
                                                      (env-with-variables ,binder-env
                                                                          `(,,b)))))
                                       finally (setf ,res (nreverse ,res))
                                               (setf (,tag ast) ,res)
                                               (funcall alter-identity (first ,tag) ,res)))
                                  ;; normal, parallel bindings
                                  (with-gensyms (whole)
                                    `(progn
                                       (setf
                                        (,tag ast)
                                        (mapcar
                                         (lambda (,whole)
                                           (if (typep ,whole 'symbol-ref)
                                               (change-class ,whole 'binder)
                                               `(,(change-class (first ,whole) 'binder)
                                                 ,,(if (cdr (assoc value-tag tag-kinds))
                                                       `(mapcar (rcurry walker env)
                                                                (cdr ,whole)) ;(b &body ...)
                                                       `(funcall walker (second ,whole)
                                                                 env)))))
                                         (first ,tag)))
                                       (funcall alter-identity (first ,tag) (,tag ast)))))))
                           ;; function-like bindings
                           ((list (type symbol) (or (eql '&lambda) (eql '&macro-lambda))
                                  (type symbol))
                            (let ((name-tag (first (cdr (assoc tag rest-patterns))))
                                  (code-tag (lastcar (cdr (assoc tag rest-patterns)))))
                              (with-gensyms (res newenv name fun info)
                                `(loop
                                   with ,res := (list)
                                   with ,newenv := ,(augment-env `env entries)
                                   for ,name in ,name-tag
                                   for ,info in ,code-tag
                                   for ,fun := ,(walk-function-body
                                                 newenv info
                                                 `(env-with-blocks newenv-with-params
                                                                   `(,,name)))
                                   do (push (cons (change-class ,name 'binder) ,fun) ,res)
                                   finally (setf (,tag ast) ,res)
                                           (funcall alter-identity (first ,tag) ,res)))))))
                        ;; non-&rest binder
                        ((loop for (ctx . %entries) in binds
                               thereis (cdr (rassoc tag (plist-alist %entries))))
                         `(setf (,tag ast) (change-class (first ,tag) 'binder)))
                        ;; unevaluated - declarations, tags etc.
                        ((not (assoc tag binds)) `(setf (,tag ast) (first ,tag)))
                        ;; evaluation contexts
                        (t ; body tags aren't duplicated, so just take the first
                         (ecase tag-kind
                           ((&declarations &rest-qualifiers)
                            `(setf (,tag ast) (first ,tag)))
                           ((&body &rest nil)
                            `(setf (,tag ast)
                                   ,(if tag-kind
                                        (with-gensyms (body-form newenv)
                                          `(loop
                                             with ,newenv := ,(augment-env `env entries)
                                             for ,body-form in (first ,tag)
                                             collect (funcall walker ,body-form ,newenv)))
                                        ;; assume walker will call alter-identity
                                        `(funcall walker (first ,tag)
                                                  ,(augment-env `env entries)))))
                           ((&lambda &macro-lambda &method-lambda)
                            (with-gensyms (newenv)
                              `(let ((,newenv ,(augment-env `env entries)))
                                 (setf (,tag ast)
                                       ,(walk-function-body newenv `(first ,tag)
                                                            `newenv-with-params))))))))))
                  ast)))
             ))
       (setf (gethash ',name *special-walkers*) ',(symbolicate name "-WALKER"))
       (setf (gethash ',name *special-parsers*) ',(symbolicate name "-PARSER"))
       (values))))

(defun test-walker (form)
  (funcall (symbolicate (car form) "-WALKER")
           form +nullenv+
           (lambda (form env) (disp (list form env)))
           (lambda (binder kind) (disp (list binder kind)))))

(defform (defmethod name &rest-qualifiers qualifiers &method-lambda fun-code)
  :binds ((fun-code :function name :block name)))

(defform (let (&rest vars)
           &declarations decls
           &body body)
  :rest-patterns ((vars . (&or name (name &body init))))
  :binds ((init) (body :variable name)))

(defform (let* (&rest vars)
           &declarations decls
           &body body)
  :rest-patterns ((vars . (&or name (name &body init))))
  :binds ((init :variable (< name))
          (body :variable name)))

(defform (alexandria:when-let* (&or (name &body init) (&rest vars))
           &body body)
  :rest-patterns ((vars . (name &body init)))
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

(defform (read-function fun-designator))
(defform (function fun-designator))
(defform (read-quote thing))
(defform (quote thing))

(defform (setq &body forms)
  :binds ((forms)))

(defform (return-from name value)
  :binds ((value)))
;; note: structural editing needed
(defform (if test &body then-else)
  :binds ((test) (then-else)))

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

(defform (tagbody &body body)
  :binds ((body)))
(defform (go tag))

(defform (unwind-protect protected &body cleanup)
  :binds ((protected) (cleanup)))

(defform (multiple-value-call fun arg &body body)
  :binds ((fun) (arg) (body)))
;; note: structural editing needed
(defform (multiple-value-prog1 &body body)
  :binds ((body)))

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

(defun strip-wrappers (form)
  (if (atom form)
      (cond ((typep form 'symbol-ref) (name form))
            ((typep form 'literal) (read-from-string (str form)))
            (t form))
      (cons (strip-wrappers (car form)) (strip-wrappers (cdr form)))))

;;
;;; macro analysis via perturbation
;; TODO block analysis
(defun macro-call-envmap (call env &optional (walker (constantly nil)))
  "Identifies body forms and binding scopes to return a map of {sexp -> binding env}.
Walks subforms of the call using WALKER during analysis."
  (flet ((lookup-path (path form)
           (loop for i in (reverse path)
                 do (setf form (nth i (disp form)))
                 finally (return form))))
    (let ((raw-form->loc (make-hash-table :test #'eq))
          (raw-form->form (make-hash-table :test #'eq))
          (possible-identifiers (make-hash-table :test #'eq))
          (eval->binders (make-hash-table :test #'eq))
          (raw (strip-wrappers call)))
      ;; identify all forms in the original call for classification
      ;; some may be constants, others are binders and expressions
      ;; record their source sym-paths for reconstruction
      (labels ((walk-call-collecting-forms (form path)
                 (loop for i from 0
                       for subform in form
                       for newpath = (cons i path)
                       do (if (consp subform)
                              (progn
                                (push newpath (gethash subform raw-form->loc))
                                (walk-call-collecting-forms subform newpath))
                              (when (symbolp subform)
                                (push newpath (gethash subform raw-form->loc)))))))
        (walk-call-collecting-forms raw (list)))
      ;; - macroexpand fully up to special (or hardwired macro) forms,
      ;;   record *only* binders and obvious evaluation contexts seen in the output
      ;; - call the walker meanwhile
      (walk-form (env-macroexpand raw env) env
                 (lambda (form env)
                   (when (symbolp form)
                     (setf (gethash form possible-identifiers) :ref))
                   ;; continue if this isn't an cons, or not part of the call
                   (if (and (consp form) (gethash form raw-form->loc))
                       ;; recurse known eval'd subforms from the ORIGINAL tree, for parser
                       (let ((call-form
                               (lookup-path (car (gethash form raw-form->loc)) call)))
                         (setf (gethash form raw-form->form) call-form)
                         (ensure-gethash call-form eval->binders)
                         (funcall walker call-form env)
                         nil)
                       t))
                 ;; binder currently may be unused
                 (lambda (sym kind)
                   (setf (gethash sym possible-identifiers) kind)))
      (disp (hash-table-plist raw-form->loc))
      (disp (hash-table-plist possible-identifiers))
      ;; analysis, first to understand syntax before constructing the returned maps
      (let ((gensym->path (make-hash-table :test #'eq))
            (gensym->refpath (make-hash-table :test #'eq)))
        (labels ((substitute-sym (path form sym)
                   (loop for rest on (reverse path)
                         for i := (car rest)
                         do (if (null (cdr rest))
                                (setf (nth i form) sym)
                                (setf form (nth i form))))))
          (loop ; we need to know where the refs and binders are first
                for sym being the hash-keys of possible-identifiers using (hash-value kind)
                do (loop for path in (gethash sym raw-form->loc)
                         for gensym := (gensym "PB")
                         for ident-kind := nil
                         do (substitute-sym path raw gensym)
                            (let ((expansion (handler-case (env-macroexpand raw env)
                                               (error () +fail+))))
                              (unless (eq expansion +fail+)
                                (block walk
                                  (walk-form expansion env
                                             (lambda (form env)
                                               (declare (ignore env))
                                               (cond ((gethash form raw-form->loc) nil)
                                                     ((eq form gensym)
                                                      (setf ident-kind :ref)
                                                      (return-from walk))
                                                     (t t)))
                                             (lambda (sym kind)
                                               (when (and (eq sym gensym)
                                                          (eq kind :variable))
                                                 (setf ident-kind :variable)
                                                 (return-from walk)))))))
                            ;; an actual reference will be independent of the binding name
                            ;; counter: step forms may have spurious refs not from the call
                            (if ident-kind
                                (progn
                                  (setf (gethash gensym gensym->path) path)
                                  (when (eq ident-kind :ref)
                                    (setf (gethash gensym gensym->refpath) path)))
                                (substitute-sym path raw sym))))
          (disp (hash-table-plist gensym->path))
          (disp (hash-table-plist gensym->refpath))
          ;; - map gensyms back to the call and change class to binder
          ;; - key these under the form from the call, via raw-form->form
          (let ((expansion (handler-case (env-macroexpand raw env)
                             (error () +fail+))))
            (unless (eq expansion +fail+)
              (walk-form
               expansion env
               (lambda (form subenv)
                 (flet ((calc-bindings ()
                          (loop with res := (list)
                                for v in (ldiff (variable-bindings subenv)
                                                (variable-bindings env))
                                do (with-lookup (path (gethash (ensure-car v) gensym->path))
                                     (let ((binder (lookup-path path call)))
                                       (assert (typep binder 'symbol-ref))
                                       (change-class binder 'binder)
                                       (push binder res)))
                                finally (return res))))
                   (if (and (consp form) (gethash form raw-form->loc))
                       (progn
                         (setf (gethash (gethash form raw-form->form) eval->binders)
                               (calc-bindings))
                         nil)
                       ;; t -> continue recursion until known
                       (with-lookup (path (gethash form gensym->refpath) t)
                         (setf (gethash (lookup-path path call) eval->binders)
                               (calc-bindings))
                         nil)))))))
          (disp (hash-table-plist eval->binders))
          (values call eval->binders))))))

;; test:
;; (loop for it from 1 to 10
;;       if (evenp it)
;;         collect it)

;; test:
;; (loop for it from from
;;       do (print it))

(defun parse (form env alter-identity)
  (cond
    ((atom form)
     ;; vectors are self evaluating and non-atomic
     (assert (typep form '(or symbol-ref literal vector)))
     form)
    ((not (typep (car form) 'symbol-ref)) (error "lambda in car unimplemented"))
    (t
     (let ((name (name (car form))))
       (if-let (parser (gethash name *special-parsers*))
         (funcall alter-identity form
                  (funcall parser form env (rcurry #'parse alter-identity) alter-identity))
         (multiple-value-bind (result local-expansion)
             (env-function-info name env)
           (flet ((parse-function (form)
                    (funcall
                     alter-identity form
                     (make-instance 'function-call
                                    :name (parse (car form) env alter-identity)
                                    :body (mapcar (rcurry #'parse env alter-identity)
                                                  (cdr form)))))
                  ;; don't expand explicitly, we only care about explicit call subforms
                  (parse-macro (form)
                    (let* ((macro-subforms (make-hash-table :test #'eq))
                           (eval->binders
                             (macro-call-envmap
                              form env
                              (lambda (form env)
                                (let ((parsed-subform (parse form env alter-identity)))
                                  (setf (gethash form macro-subforms) parsed-subform)
                                  (funcall alter-identity form parsed-subform))))))
                      (funcall alter-identity form
                               (make-instance 'macro-call :op (car form) :body (cdr form)
                                                          :subform-asts macro-subforms
                                                          :eval-binders eval->binders)))))
             (cond ((null result) ; global
                    (if (and (symbolp name) ; could be lambda
                             (macro-function name) (not (hardwired-p name)))
                        (parse-macro form)
                        (parse-function form)))
                   ;; local
                   ((null local-expansion) (parse-function form))
                   (t (parse-macro form))))))))))

(defmethod location-sort ((node function-call) id)
  (trivia:match id
    ;; XXX can be lambda, but does anyone besides trivia internals use this?
    ;; ensure that (parse:name lambda) = "lambda" is defined for totality
    ((eql 'name) 'symbol-ref)
    ((list (eql 'body) (type integer)) 'eval-form)))

;;
;;; eclector reader
;;
(defclass my-client (eclector.parse-result:parse-result-client)
  ((source :initarg :source
           :initform (error "no source")
           :reader source)
   (uneval-data :initform (make-hash-table)
                :reader uneval-data)
   (trailing-data :initform (make-hash-table)
                  :reader trailing-data)))

;; and or not
(defclass read-cond (eval-form atom-form)
  ((stuff :initarg :stuff
          :initform (error "no stuff")
          :reader stuff
          :documentation "may include comments between the conditional and object")
   (flags :initarg :flags
          :initform (error "no flags")
          :reader flags)
   (kind :initarg :kind
         :initform (error "no kind")
         :reader kind
         :type (or (eql #\+) (eql #\-)))))

(defmethod print-object ((object read-cond) stream)
  (format stream "<~a~a ~a>" (kind object) (flags object) (stuff object)))

(defstruct read-evaluated form)
(defstruct read-conditional result)

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result t) (children t) (source t))
  (if (null children)
      (if (and (atom result) (constantp result)
               (not (eq nil result)))
          (let* ((s (subseq (source client) (car source) (cdr source)))
                 (c (schar s 0)))
            (cond ((char= c #\:) result)
                  ((and (keywordp result) (= (- (cdr source) (car source))
                                             (length (string result))))
                   (make-read-conditional :result result))
                  (t
                   (make-instance 'literal :str s))))
          (progn
            (assert (symbolp result))
            (make-instance 'symbol-ref :name result)))
      ;; compound form, or some wrapper
      (labels ((frobber ()
                 ;; TODO discards trailing comments, could anchor to parent instead
                 (loop with comments = nil
                       with res = (list)
                       for c in children
                       do (if (not (typep c 'comment))
                              (progn
                                (nconcf (gethash c (uneval-data client)) comments)
                                (push c res)
                                (setf comments nil))
                              (push c comments))
                       finally (return (nreverse res))))
               (frob-cons ()
                 (let ((conds (count-if #'read-conditional-p children)))
                   (if (plusp conds)
                       (if (< 1 conds)
                           (make-read-conditional
                            :result (mapcar #'(lambda (c) (if (read-conditional-p c)
                                                         (read-conditional-result c)
                                                         c))
                                            children))
                           (let ((read-cond-pos
                                   (position-if #'read-conditional-p children))
                                 (form (lastcar children)))
                             (loop
                               for c on children
                               for i below read-cond-pos
                               do (push (car c) (gethash form (uneval-data client)))
                               finally (push (make-instance
                                              'read-cond
                                              :kind (schar (source client)
                                                           (1+ (car source)))
                                              :flags (car c)
                                              :stuff (cdr c))
                                             (gethash form (uneval-data client))))
                             (lastcar children)))
                       ;; note: dotted lists are parsed with the dot as a symbol reference,
                       ;; this is needed to preserve comments
                       (frobber)))))
        (typecase result
          (cons
           (case (car result)
             ;; XXX does not support #+#.(foo), this is relatively common
             (function
              `(,(make-instance 'symbol-ref
                                :name (if (and (typep (first children) 'symbol-ref)
                                               (eq 'function (name (first children))))
                                          'function 'read-function))
                ,(lastcar children)))
             (quote
              `(,(make-instance 'symbol-ref
                                :name (if (and (typep (first children) 'symbol-ref)
                                               (eq 'quote (name (first children))))
                                          'quote 'read-quote))
                ,(lastcar children)))
             (eclector.reader:quasiquote
              `(,(make-instance 'symbol-ref :name 'eclector.reader:quasiquote)
                ,(lastcar children)))
             (eclector.reader:unquote
              `(,(make-instance 'symbol-ref :name 'eclector.reader:unquote)
                ,(lastcar children)))
             (eclector.reader:unquote-splicing
              `(,(make-instance 'symbol-ref :name 'eclector.reader:unquote-splicing)
                ,(lastcar children)))
             (t
              (frob-cons))))
          (read-evaluated
           `(,(make-instance 'symbol-ref :name 'read-eval)
             ,(lastcar children)))
          (vector (apply #'vector (frobber)))
          ;; should be read-conditional wrapped
          (t
           (assert (member-if #'read-conditional-p children))
           (frob-cons))))))

(defmethod eclector.parse-result:make-skipped-input-result
    ((client my-client) (stream t) (reason t) (children t) (source t))
  (flet ((source-str (start end)
           (subseq (source client) start end)))
    (trivia:cmatch reason
      ((cons (eql :line-comment) (type integer))
       (assert (null children))
       (make-instance 'comment :kind :line
                               :str (source-str (car source) (1- (cdr source)))))
      ((eql :block-comment)
       (assert (null children))
       (make-instance 'comment :kind :block
                               :str (source-str (car source) (cdr source))))
      ((cons (eql :sharpsign-plus) res)
       (print children)
       (make-instance 'read-cond :kind #\+ :flags res :stuff (rest children)))
      ((cons (eql :sharpsign-minus) res)
       (make-instance 'read-cond :kind #\- :flags res :stuff (rest children)))
      ((eql '*read-suppress*)
       (assert (null children))
       (source-str (car source) (cdr source))))))

(defmethod eclector.reader:evaluate-expression ((client my-client) (expression t))
  (make-read-evaluated :form expression))

(defmethod eclector.reader:fixup ((client my-client) obj state)
  (declare (ignore obj state))
  (error "TODO handle circular lists, check eclector parse-result suite"))

(defun parse-from-string (s)
  (let ((client (make-instance 'my-client :source s)))
    (multiple-value-bind (form len leading-comments)
        (eclector.parse-result:read-from-string client s)
      (declare (ignore len))
      (let* ((comment-map (uneval-data client))
             (res
               (parse form (make-env :%function-bindings '(read-eval))
                      (lambda (old new)
                        (setf (gethash new comment-map) (gethash old comment-map))
                        (remhash old comment-map)
                        new))))
        (setf (gethash res comment-map) leading-comments)
        (values res (uneval-data client))))))
