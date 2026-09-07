;;;;
;;;; incremental lisp parsing
;;;;
;;

(uiop:define-package #:weave-parser
  (:use :cl #:alexandria-2 #:weave-utils)
  (:export #:make-env
           #:parse
           #:is-atom #:get-body
           #:copy-node

           #:update
           #:get-location #:getloc

           #:location-sort #:locsort

           #:eval-form #:symbol-ref #:binder #:wrapped-symbol
           #:function-call #:literal
           #:unevaluated
           #:body #:name #:str #:vars #:op
           #:ref-list #:elements #:with-elements
           #:function-code #:lambda-list
           #:home-package
           ))
(in-package #:weave-parser)

;;
;;; class defs: mainly we want a structure that's
;;; - close enough to s-expressions for macroexpansion and evaluation
;;; - has tags for tree-structured dispatch (minimal passing of context through wrappers)
;;; - gives identity to semantic units which may need identity under editing
;;;   since we should avoid sequence cursors
;;;   - reader syntax (e.g. literals, eval, quasiquote) need explicit representation
;;;     that are unambiguously separate from plain symbols
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

;; mixin for tracking inactive text
(defclass anchor ()
  ((leading :initarg :leading
            :initform nil
            :accessor leading
            :documentation "comments and reader conditionals written just before")
   (trailing :initarg :trailing
             :initform nil
             :accessor trailing
             :documentation "... written just after, within the list this belongs to")
   (inside :initarg :inside
           :initform nil
           :accessor inside
           :documentation "... written within, only for a list holding no elements")))

(defclass atom-form (anchor)
  ())

(defclass eval-form (anchor)
  ((typ :initform nil
        :accessor typ))
  (:documentation "Form in an evaluation context, perhaps quoted."))

(defclass literal (eval-form atom-form)
  ((str :initarg :str
        :initform (error "literal not provided")
        :reader str
        :type string))
  (:documentation "Atomic literal"))

(defclass symbol-ref (eval-form atom-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :reader name
         :type string)
   ;; note: this assumes the referenced package already exists in the image
   (home-package :initarg :home-package
                 :initform *package*
                 :reader home-package))
  (:documentation "Represents a symbol, possibly referring to a symbol macro."))

(defclass binder (atom-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :reader name
         :type string)
   (home-package :initarg :home-package
                 :initform *package*
                 :reader home-package))
  (:documentation "Represents a binder, NOT a reference in evaluation position."))

(deftype wrapped-symbol () '(or symbol-ref binder))
;; for code shared between the wrapped AST and the real symbols STRIP-WRAPPERS produces
(deftype symbol-like () '(or symbol wrapped-symbol))

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

(defclass ref-list (anchor)
  ((elements :initarg :elements
             :initform nil
             :accessor elements
             :type list)
   ;; note: an array holds one element, the contents as written, so that its rows are
   ;; ordinary lists which can be anchored to and indexed one level at a time
   (kind :initarg :kind
         :initform :list
         :accessor kind
         :type (member :list :vector :array))
   (rank :initarg :rank
         :initform nil
         :accessor rank
         :type (or null integer)
         :documentation "of an :array, which cannot be told from its contents")))

(defclass function-code (anchor)
  ((lambda-list :initarg :lambda-list
                :reader lambda-list
                :type ref-list)
   (lambda-list-kind :initarg :lambda-list-kind
                     :reader lambda-list-kind
                     :type (member &lambda &macro-lambda &method-lambda))
   (docstring :initarg :docstring
              :reader docstring
              :type (or null literal string))
   (declarations :initarg :declarations
                 :reader declarations)
   (body :initarg :body
         :reader body
         :type list))
  (:documentation "(macro) lambda list and body list of eval-forms"))

(defclass reader-marker (symbol-ref)
  ()
  (:documentation "A symbol inserted to represent reader syntax"))

(defclass comment ()
  ((str :initarg :str
        :accessor str)
   (kind :initarg :kind
         :initform :line
         :accessor kind
         :type (or (eql :line) (eql :block))))
  (:documentation ""))

(defclass dot-marker (reader-marker)
  ()
  (:documentation "The dot marker produced by the reader as a symbol.
This class exists because an uninterned symbol also has a null HOME-PACKAGE,
so a name test alone can't distinguish #:|.| from a dot."))

(defclass label-ref (reader-marker)
  ()
  (:documentation "#n#, distinguished from the uninterned #:|n|"))

(defclass label-def (eval-form atom-form)
  ((name :initarg :name
         :initform (error "must provide label name")
         :reader name
         :type string)
   (labeled :initarg :labeled
            :initform (error "must provide labeled form")
            :reader labeled))
  (:documentation "#n=form"))

(defstruct location
  "`id's are typically either (slot) or (slot integer*) and should respect `cl:equal'.
They are specific to the `node' type."
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

(defmethod print-object ((object ref-list) stream)
  (format stream "<~a(~{~a~^ ~})>"
          (ecase (kind object)
            (:list "")
            (:vector "#")
            (:array (format nil "#~aA" (rank object))))
          (elements object)))

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
  (:method ((o cons)) (cons (copy-node (car o)) (copy-node (cdr o))))
  (:method ((o array))
    (let ((new (copy-array o)))
      (dotimes (i (array-total-size new) new)
        (setf (row-major-aref new i) (copy-node (row-major-aref o i))))))
  ;; NIL slots and nodes deliberately left unstructured (reader conditionals).
  (:method (o) o)
  (:method ((o comment)) (make-instance 'comment :kind (kind o) :str (str o)))
  (:method ((o literal)) (make-instance 'literal :str (str o)))
  ;; note: class-of, so a reader-marker doesn't decay into a plain symbol-ref
  (:method ((o symbol-ref))
    (make-instance (class-of o) :name (name o) :home-package (home-package o)))
  (:method ((o label-def)) (make-instance 'label-def :name (name o)
                                                     :labeled (copy-node (labeled o))))
  (:method ((o binder)) (make-instance 'binder :name (name o)
                                               :home-package (home-package o)))
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
                   :body (mapcar #'copy-node (body o)) :op (copy-node (op o))))
  (:method ((o ref-list))
    (make-instance 'ref-list :elements (copy-node (elements o))
                             :kind (kind o) :rank (rank o)))
  (:method ((o function-code))
    (make-instance 'function-code
                   :lambda-list (copy-node (lambda-list o))
                   :lambda-list-kind (lambda-list-kind o)
                   :docstring (copy-node (docstring o))
                   :declarations (copy-node (declarations o))
                   :body (mapcar #'copy-node (body o)))))

(defun copy-anchors (old new)
  "Gives `new' the anchors of `old', which it keeps as it is rebuilt."
  (setf (leading new) (leading old)
        (trailing new) (trailing old)
        (inside new) (inside old))
  new)

(defmethod update :around ((node anchor) id new-value)
  (declare (ignore id new-value))
  (let ((new (call-next-method)))
    (if (and (typep new 'anchor) (not (eq new node)))
        (copy-anchors node new)
        new)))

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

(defun unlock-declaration (name)
  "We only rebuild lexical scopes to macroexpand so this is safe. Needed for
pprint-exit-if-list-exhausted/pop and other forms that introduce local macrolet bindings."
  (declare (ignorable name))
  #+sbcl `((declare (sb-ext:disable-package-locks ,name))))

(defun macrolet-code-wrap (name rest form)
  `(locally ,@(unlock-declaration name)
     (macrolet ((,name ,@rest))
       ,form)))

(defun symbol-macrolet-wrap (name expansion form)
  `(symbol-macrolet ((,name ,expansion))
     ,form))

(defun flet-wrap (name form)
  `(locally ,@(unlock-declaration name)
     (flet ((,name (&rest args) (declare (ignorable args))))
       ,form)))

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
        do (when (eq name entry)
             (return (values name nil)))
           (when (and (consp entry) (eq name (car entry)))
             (return (values (car entry) (cdr entry))))))
(defun env-function-info (name env)
  (loop for entry in (function-bindings env)
        do (when (eq name entry)
             (return (values name nil)))
           (when (and (consp entry) (eq name (car entry)))
             (return (values (car entry) (cdr entry))))))

(defun env-with-variables (env bindings)
  "Expects entry names to be strictly SYMBOLS."
  (let ((new-env (copy-env env)))
    (setf (%variable-bindings new-env)
          ;; note: constants are possible in intermediate editing states
          (nconc (remove-if (lambda (s) (constantp (ensure-car s))) bindings)
                 (%variable-bindings new-env)))
    new-env))

(defun env-with-functions (env bindings)
  "Expects entry names to be strictly SYMBOLS."
  ;; note: CL names are kept - a local binding shadowing one is needed to expand
  ;; macrolets like pprint-exit-if-list-exhausted
  (let ((new-env (copy-env env)))
    (setf (%function-bindings new-env)
          (append bindings (%function-bindings new-env)))
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
(defparameter *hardwired-operators* #(lambda defun defmethod defmacro
                                      eclector.reader:quasiquote
                                      eclector.reader:unquote
                                      eclector.reader:unquote-splicing)
  "The list of nonportable hardwired macros, not macroexpanded.")

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
                    (if (and (symbolp name)
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
  (if (typep maybe-symbol-ref 'wrapped-symbol)
      (name maybe-symbol-ref)
      maybe-symbol-ref))

(defun tag-member (elt symbols)
  "When `elt' unwraps to a string naming one of `symbols' returns it."
  (let ((name (unwrap-refs elt)))
    (and (stringp name) (find name symbols :test #'string-equal))))

(defmethod elements ((x t))
  "Anything which isn't a ref list is its own elements, so a plain list and the raw sexps
the walker sees read the same way. In particular an empty list -> nil."
  x)

(defun ref-list (&rest elements)
  (make-instance 'ref-list :elements elements))

(defun ref-list-p (x)
  (or (typep x 'ref-list) (listp x)))

(defun ref-form-p (x)
  "Whether `x' is a list read as a form, rather than a literal vector or array"
  (and (ref-list-p x)
       (or (not (typep x 'ref-list)) (eq (kind x) :list))))

(defun ref-atom-p (x)
  (or (not (ref-list-p x)) (null (elements x))))

(defun ref-car (x) (car (elements x)))
(defun ref-cdr (x) (cdr (elements x)))
(defun ref-nth (n x) (nth n (elements x)))

(defun ref-tree-ref (tree path)
  "tree-ref but for syntactic lists"
  (cond ((null path) tree)
        ((ref-list-p tree) (ref-tree-ref (ref-nth (car path) tree) (cdr path)))
        (t (tree-ref tree path))))

(defun with-elements (list elements)
  "Replaces a ref-list `list's elements with `elements', copying anchored data.
Otherwise in the walker just returns elements."
  (cond ((typep elements 'ref-list) elements)
        ((typep list 'ref-list)
         (check-type elements list)
         (copy-anchors list (make-instance 'ref-list :elements elements
                                                     :kind (kind list)
                                                     :rank (rank list))))
        (t elements)))

(defun ref-tree-update (tree path new-value)
  "tree-update through syntactic lists, keeping the anchors of each one rebuilt."
  (cond ((null path) new-value)
        ((ref-list-p tree)
         (with-elements tree
           (list-update (elements tree)
                        (ref-tree-update (ref-nth (car path) tree) (cdr path) new-value)
                        (car path))))
        (t (tree-update tree path new-value))))

(defun parsed-list (written elements alter-identity)
  (funcall alter-identity written
           (if (typep written 'ref-list)
               (with-elements written elements)
               (make-instance 'ref-list :elements elements))))

(defun resolve (wrapper)
  (if-let (p (home-package wrapper))
    (find-symbol (name wrapper) p)
    (values (name wrapper) '#:uninterned)))

(defun ref-coerce-symbol (s)
  (if (symbolp s)
      s
      (resolve s)))

;; we need to be more permissive for lambda lists. It makes little sense to preserve
;; well-formedness when it often breaks with edits due to positional &keyword context
;; e.g. (&key (a _) (b _)) -?> (a b)  or  (&optional (b _) c) -?> (b &optional c)
;; note: dot is treated as a symbol-ref by reading, but dotted lists shouldn't come up
;; even during macroexpansion since lambda list are nested in evaluation contexts
(defun map-lambda-list (list on-binder value-mapper on-syntax specializer-list-p
                        &optional destructure-p alter-identity)
  "Doesn't force well-formedness and tries to be very tolerant. Reconstructs the list
structure from the return values of `on-binder', `value-mapper' and `on-syntax' - the
latter sees specifically lambda-list list keywords and the keyword of a ((:key var) ...)
pair. `destructure-p' allows &optional/&rest/&body vars, and &key vars as a
(keyword-name var) pair, to themselves be nested macro lambda lists (CLHS 3.4.4)."
  (let ((new
          (loop
            with current-keyword := nil
            with res := (list)
            for elt in (elements list)
            for keyword := (tag-member elt lambda-list-keywords)
            do (labels ((bind-var (v)
                          (cond ((typep v 'symbol-like) (funcall on-binder v))
                                ((and destructure-p (ref-list-p v))
                                 (map-macro-lambda v on-binder value-mapper on-syntax
                                                   alter-identity))
                                (t (funcall value-mapper v))))
                        ;; note: only meaningful for a specializer list
                        (maybe-default (val)
                          (if (and specializer-list-p (not current-keyword))
                              val
                              (funcall value-mapper val))))
                 (if keyword
                     (progn (setf current-keyword keyword)
                            (push (funcall on-syntax elt) res))
                     (push
                      (case current-keyword
                        ((&rest &body) (bind-var elt))
                        (&key
                         (trivia:match (elements elt)
                           ((and (type symbol-like) v) (funcall on-binder v))
                           ((list* var tail)
                            (destructuring-bind
                                (&optional (val nil val-p) (supplied-p nil supplied)
                                 &rest extra)
                                tail
                              (if (or extra
                                      (and supplied (not (typep supplied-p 'symbol-like)))
                                      (not (and (typep var '(or symbol-like cons
                                                             ref-list))
                                                (elements var))))
                                  (funcall value-mapper elt)
                                  (let* ((new-var
                                           (if (typep var 'symbol-like)
                                               (funcall on-binder var)
                                               (let ((inner
                                                       (with-elements
                                                         var
                                                         `(,(funcall on-syntax (ref-car var))
                                                           ,(bind-var (ref-nth 1 var))))))
                                                 (when alter-identity
                                                   (funcall alter-identity var inner))
                                                 inner)))
                                         (new (with-elements elt
                                                `(,new-var
                                                  ,@(when val-p
                                                      `(,(funcall value-mapper val)))
                                                  ,@(when supplied
                                                      `(,(funcall on-binder supplied-p)))))))
                                    (when alter-identity
                                      (funcall alter-identity elt new))
                                    new))))
                           (_ (funcall value-mapper elt))))
                        (&optional
                         (trivia:match (elements elt)
                           ((and (type symbol-like) v) (funcall on-binder v))
                           ((list* var (and (type list) tail))
                            (destructuring-bind
                                (&optional (val nil val-p) (supplied-p nil supplied)
                                 &rest extra)
                                tail
                              (if (or extra
                                      (and supplied (not (typep supplied-p 'symbol-like))))
                                  (funcall value-mapper elt)
                                  (let ((new (with-elements elt
                                               `(,(bind-var var)
                                                 ,@(when val-p
                                                     `(,(funcall value-mapper val)))
                                                 ,@(when supplied
                                                     `(,(funcall on-binder supplied-p)))))))
                                    (when alter-identity
                                      (funcall alter-identity elt new))
                                    new))))
                           (_ (funcall value-mapper elt))))
                        (t
                         (trivia:match (elements elt)
                           ((or (and (type symbol-like) v)
                                ;; keyword or method-like
                                (list (and (type symbol-like) v)))
                            (funcall on-binder v))
                           ;; default value or specializer
                           ((list (and (type symbol-like) v) val)
                            (let ((new (with-elements elt
                                         `(,(funcall on-binder v) ,(maybe-default val)))))
                              (when alter-identity (funcall alter-identity elt new))
                              new))
                           ((list (and (type symbol-like) v) val
                                  (and (type symbol-like) supplied-p))
                            (let ((new (with-elements elt
                                         `(,(funcall on-binder v) ,(maybe-default val)
                                           ,(funcall on-binder supplied-p)))))
                              (when alter-identity (funcall alter-identity elt new))
                              new))
                           (_ (funcall value-mapper elt)))))
                      res)))
            finally (return (with-elements list (nreverse res))))))
    (when alter-identity (funcall alter-identity list new))
    new))

(defun map-macro-lambda (list on-binder value-mapper on-syntax &optional alter-identity)
  (let ((new (with-elements
               list
               (loop
                 with res := (list)
                 for rest on (elements list)
                 for this = (car rest)
                 do (cond ((tag-member this '(&body &rest &key &optional &aux))
                           (return (nreconc res (map-lambda-list rest on-binder value-mapper
                                                                 on-syntax nil t
                                                                 alter-identity))))
                          ((tag-member this lambda-list-keywords)
                           (push (funcall on-syntax this) res))
                          ((ref-list-p this)
                           (push (map-macro-lambda this on-binder value-mapper on-syntax
                                                   alter-identity)
                                 res))
                          ;; reader dot, or a literal nil placeholder
                          ((or (typep this 'dot-marker)
                               (null (unwrap-refs this)))
                           (push (funcall on-syntax this) res))
                          ((stringp (unwrap-refs this)) (push (funcall on-binder this) res))
                          (t (push (funcall value-mapper this) res)))
                 finally (return (nreverse res))))))
    (when alter-identity (funcall alter-identity list new))
    new))

(defun lambda-list-sorts (node)
  "A tree isomorphic to (lambda-list `node') whose leaves are location sorts.
`lambda-list-kind' selects which grammar to walk it with.
The specializer-list-p argument is always NIL so we can classify specializers."
  (if (eq (lambda-list-kind node) '&macro-lambda)
      (map-macro-lambda (lambda-list node) (constantly 'binder) (constantly 'eval-form)
                        (constantly 'unevaluated))
      (map-lambda-list (lambda-list node) (constantly 'binder) (constantly 'eval-form)
                       (constantly 'unevaluated) nil)))

(defmethod get-location ((node function-code) id)
  (trivia:cmatch id
    ((eql 'lambda-list) (lambda-list node))
    ((list* (eql 'lambda-list) path)
     (when-let (l (elements (lambda-list node)))
       (ref-tree-ref l path)))
    ((eql 'docstring) (docstring node))
    ((eql 'declarations) (declarations node))
    ((eql 'body) (body node))
    ((list (eql 'body) (and (type integer) i)) (nth i (body node)))))

(defmethod update ((node function-code) id new-value)
  (flet ((rebuild (&key (lambda-list (lambda-list node)) (docstring (docstring node))
                     (declarations (declarations node)) (body (body node)))
           (make-instance 'function-code
                          :lambda-list lambda-list :lambda-list-kind (lambda-list-kind node)
                          :docstring docstring :declarations declarations :body body)))
    (trivia:cmatch id
      ((eql 'lambda-list) (rebuild :lambda-list new-value))
      ((list* (eql 'lambda-list) path)
       (rebuild :lambda-list (ref-tree-update (lambda-list node) path new-value)))
      ((eql 'docstring) (rebuild :docstring new-value))
      ((eql 'declarations) (rebuild :declarations new-value))
      ((eql 'body) (rebuild :body new-value))
      ((list (eql 'body) (and (type integer) i))
       (rebuild :body (list-update (body node) new-value i))))))

(defmethod location-sort ((node function-code) id)
  (trivia:match id
    ((eql 'docstring) 'string)
    ((list (eql 'body) (type integer)) 'eval-form)
    ((list* (eql 'lambda-list) path)
     (when-let (sorts (elements (lambda-list-sorts node)))
       (ref-tree-ref sorts path)))))

(defun ref-declaration-p (form)
  "A (declare ...), whatever represents the written form."
  (and (ref-list-p form)
       (let ((head (ref-car form)))
         (or (eq head 'declare)
             (and (typep head 'wrapped-symbol) (eq (resolve head) 'declare))))))

(defun ref-string-p (form)
  (or (stringp form)
      (and (typep form 'literal)
           (plusp (length (str form)))
           (char= #\" (char (str form) 0)))))

(defun parse-body-declarations (body documentation)
  "Splits `body' into the forms, declarations and documentation string it begins with.
Alexandria's parse-body only knows ordinary sexps, where a declaration is a cons whose car
is EQ to DECLARE, so it recognizes nothing at all in our representation. A string in the
last position is a return value rather than documentation."
  (let ((doc nil)
        (decls (list)))
    (loop for form := (car body)
          do (cond ((and documentation (ref-string-p form) (cdr body))
                    (when doc
                      (form-parse-error "two documentation strings"))
                    (setf doc (pop body)))
                   ((ref-declaration-p form) (push (pop body) decls))
                   (t (return))))
    (values body (nreverse decls) doc)))

;;
;;; parsing
;;
;; if lisp is mud then parsing is the experience of it slipping through your fingers
;;

(defparameter *special-walkers* (make-hash-table :test #'eq))
(defparameter *special-parsers* (make-hash-table :test #'eq))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *parser-keywords* '(&rest &body &tree &or &declarations &rest-qualifiers
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
                   ((member (car spec) '(&tree &declarations &rest-qualifiers) :test #'eq)
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
    ;; note: the string as written, which in our representation is a LITERAL node
    (documentation nil)
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

  (defun location-methods (classname spec-kinds rest-patterns binds)
    "Emits the get-location/update/location-sort methods for a defform class.
Generally the tag name indexes the whole slot and (tag integer*) index tree structure."
    (let ((slots (remove-duplicates (mapcar #'car spec-kinds)))
          (get-clauses (list))
          (update-clauses (list))
          (sort-clauses (list)))
      (flet ((rebuild (slot value)
               `(make-instance ',classname
                               :op (op node)
                               ,@(loop for s in slots
                                       append `(,(make-keyword s)
                                                ,(if (eq s slot) value `(,s node)))))))
        (push `((eql 'op) (op node)) get-clauses)
        ;; accessors
        (loop
          for part in slots
          for kind = (cdr (assoc part spec-kinds))
          do (push `((eql ',part) (,part node)) get-clauses)
             ;; note: a whole &rest slot may be replaced by its elements alone, whatever
             ;; was written keeping the provenance anchored to it
             (push `((eql ',part)
                     ,(rebuild part (if (eq kind '&rest)
                                        `(with-elements (,part node) new-value)
                                        'new-value)))
                   update-clauses)
             (case kind
               (&rest
                (push `((list (eql ',part) (and (type integer) i))
                        (nth i (elements (,part node))))
                      get-clauses)
                (push `((list (eql ',part) (and (type integer) i))
                        ,(rebuild part
                                  `(let* ((elts (elements (,part node)))
                                          (old (nth i elts)))
                                     (with-elements
                                         (,part node)
                                       (list-update
                                        elts
                                        (if (and (typep old 'ref-list) (listp new-value))
                                            (with-elements old new-value)
                                            (progn ; non-list e.g. lone symbol in let*
                                              (check-type new-value ref-list)
                                              new-value))
                                        i)))))
                      update-clauses))
               ;; a &body slot is an ordinary list of the forms it holds
               (&body
                (push `((list (eql ',part) (and (type integer) i)) (nth i (,part node)))
                      get-clauses)
                (push `((list (eql ',part) (and (type integer) i))
                        ,(rebuild part `(list-update (,part node) new-value i)))
                      update-clauses))
               (&tree
                (push `((list* (eql ',part) path) (ref-tree-ref (,part node) path))
                      get-clauses)
                (push `((list* (eql ',part) path)
                        ,(rebuild part `(ref-tree-update (,part node) path new-value)))
                      update-clauses)))

             ;; rest-patterns address a further index within each &rest element
             (when (eq kind '&rest)
               (when-let (pattern (cdr (assoc part rest-patterns)))
                 (push `((list (eql ',part) (and (type integer) i)
                               (and (type integer) j))
                         (ref-nth j (nth i (elements (,part node)))))
                       get-clauses)
                 (push `((list (eql ',part) (and (type integer) i)
                               (and (type integer) j))
                         ,(rebuild part
                                   `(let* ((elts (elements (,part node)))
                                           (old (nth i elts)))
                                      (with-elements
                                          (,part node)
                                        (list-update
                                         elts
                                         (with-elements
                                             old (list-update (elements old) new-value j))
                                         i)))))
                       update-clauses)))
             ;; sorts
             (loop
               for part in slots
               for kind = (cdr (assoc part spec-kinds))
               do (case kind
                    (&body
                     (push `((list (eql ',part) (type integer)) 'eval-form) sort-clauses))
                    ;; note: nothing inside quoted data is ever a binder or eval-form
                    ;; TODO refine sorts by value type (symbol/string/number/...)
                    (&tree
                     (push `((eql ',part) 'unevaluated) sort-clauses)
                     (push `((list* (eql ',part) (type list)) 'unevaluated) sort-clauses))
                    (&rest
                     (when-let (pattern (cdr (assoc part rest-patterns)))
                       (multiple-value-bind (binder-tag value-tag value-kind)
                           (rest-pattern-tags pattern)
                         ;; this has to be checked first
                         (push `((list (eql ',part) (type integer) (eql 0))
                                 ',(tag-sort binder-tag binds))
                               sort-clauses)
                         ;; logic for body, singular value (NIL), or a function-code
                         (when (member value-kind '(nil &body &lambda
                                                    &macro-lambda &method-lambda))
                           (push `((list (eql ',part) (type integer)
                                         ,(if (eq value-kind '&body)
                                              '(type integer)
                                              '(eql 1)))
                                   ',(if (member value-kind
                                                 '(&lambda &macro-lambda &method-lambda))
                                         'function-code
                                         (tag-sort value-tag binds)))
                                 sort-clauses)))))
                    ((&lambda &macro-lambda &method-lambda)
                     (push `((eql ',part) 'function-code) sort-clauses))
                    ((nil)
                     (push `((eql ',part) ',(tag-sort part binds)) sort-clauses))))))

      `((defmethod get-location ((node ,classname) id)
          (trivia:cmatch id ,@(nreverse get-clauses)))
        (defmethod update ((node ,classname) id new-value)
          (trivia:cmatch id ,@(nreverse update-clauses)))
        (defmethod location-sort ((node ,classname) id)
          (trivia:match id ,@(nreverse sort-clauses)))))))

(defmacro spec-parser (spec binds rest-patterns ref-p)
  "Expects a valid spec, binds and rest-patterns. ref-p controls whether to check
our reader representation or ordinary sexps."
  (with-gensyms (form body decls doc qualifiers)
    (progn
      (cond
        ((null spec) ; no more entries in current list, not forced by spec keyword
         `(lambda (,form)
            (unless (null (elements ,form))
              (form-parse-error "expected null, got ~a" ,form))))
        ((atom spec)
         `(lambda (,form)
            (when (and ,(binder-tag-p spec binds)
                       ;; binders cannot be literal NIL
                       (not ,(if ref-p
                                 `(typep ,form 'wrapped-symbol)
                                 `(typep ,form 'symbol))))
              (form-parse-error "expected binder symbol match: ~a" ,form))
            (push ,form ,spec)))
        ;; note: catches list nil before car recursion
        ((listp (car spec))
         `(lambda (,form)
            (if (ref-list-p (ref-car ,form))
                (progn (funcall (spec-parser ,(car spec) ,binds ,rest-patterns ,ref-p)
                                (ref-car ,form))
                       (funcall (spec-parser ,(cdr spec) ,binds ,rest-patterns ,ref-p)
                                (ref-cdr ,form)))
                (form-parse-error "list expected, got ~a, context ~a" ,form ',spec))))
        (t ; (atom (car spec))
         (case (car spec)
           (&declarations
            `(lambda (,form)
               ;; note: literal () is irrelevant to parse-body-declarations
               (multiple-value-bind (,body ,decls)
                   (parse-body-declarations (elements ,form) nil)
                 (push ,decls ,(second spec))
                 (funcall (spec-parser ,(cddr spec) ,binds ,rest-patterns ,ref-p) ,body))))
           (&or
            `(lambda (,form)
               (block nil
                 ,@(mapcar
                    (lambda (pattern)
                      `(handler-case
                           (return
                             (funcall (spec-parser ,pattern ,binds ,rest-patterns ,ref-p)
                                      ,form))
                         (form-parse-error () nil)))
                    (cdr spec))
                 (form-parse-error "expected one of ~a got ~a" ',(cdr spec) ,form))))
           ((&method-lambda &lambda &macro-lambda)
            `(lambda (,form)
               (when (null (elements ,form))
                 (form-parse-error "missing lambda list"))
               (multiple-value-bind (,body ,decls ,doc)
                   (parse-body-declarations (ref-cdr ,form) t)
                 (push (make-function-info :arglist (ref-car ,form)
                                           :documentation ,doc
                                           :decls ,decls
                                           :body ,body)
                       ,(second spec)))))
           (&body
            `(lambda (,form)
               (push (elements ,form) ,(second spec))))
           (&tree
            `(lambda (,form)
               (push (ref-car ,form) ,(second spec))
               (funcall (spec-parser ,(cddr spec) ,binds ,rest-patterns ,ref-p)
                        (ref-cdr ,form))))
           (&rest
            `(lambda (,form)
               ,(if-let (pattern (cdr (assoc (second spec) rest-patterns)))
                  `(progn
                     (mapc (spec-parser ,pattern ,binds ,rest-patterns ,ref-p)
                           (elements ,form))
                     ;; the syntactic list is kept, an empty one included
                     (push ,form ,(second spec)))
                  (error "no pattern for rest"))))
           (&rest-qualifiers
            `(lambda (,form)
               (let* ((,form (elements ,form))
                      (,qualifiers
                        (loop for v := (car ,form)
                              while ,form
                              while ,(if ref-p
                                         ;; note: () is NOT a valid qualifier as it's a list
                                         `(typep v 'symbol-ref)
                                         `(typep v 'symbol))
                              collect v
                              do (pop ,form))))
                 (push ,qualifiers ,(second spec))
                 (funcall (spec-parser ,(cddr spec) ,binds ,rest-patterns ,ref-p) ,form))))
           (t ; symbol match car... against e....
            `(lambda (,form)
               (if (elements ,form)
                   (progn (funcall (spec-parser ,(car spec) ,binds ,rest-patterns ,ref-p)
                                   (ref-car ,form))
                          (funcall (spec-parser ,(cdr spec) ,binds ,rest-patterns ,ref-p)
                                   (ref-cdr ,form)))
                   (form-parse-error "expected non-nil car: ~a" ,form))))))))))

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
  (let* ((tag-kinds (nconc (spec-kinds spec)
                           (mapcan (lambda (pair) (spec-kinds (cdr pair)))
                                   rest-patterns)))
         (bind-rest-tags (check-binds tag-kinds rest-patterns binds))
         (classname (symbolicate name "-FORM"))
         (slots (remove-duplicates (mapcar #'car (spec-kinds spec)))))
    `(progn
       (defclass ,classname (irregular-form)
         ((op :initarg :op :initform ',name :accessor op)
          ,@(loop for name in slots
                  collect `(,name :initarg ,(make-keyword name)
                                  :accessor ,name))))
       ;; methods
       ,(when (member 'body slots)
          `(defmethod get-body ((node ,classname)) (values (body node) t)))
       (defmethod copy-node ((old ,classname))
         (let ((new (make-instance ',classname :op (op old))))
           ,@(loop for name in slots
                   collect `(setf (,name new) (copy-node (,name old))))
           new))
       ,@(location-methods classname (spec-kinds spec) rest-patterns binds)
       ;; exports
       (export ',classname)
       ,@(loop for name in slots
               collect `(export ',name))

       ;; this merely does validation and preserves identity of all checked lists
       (defmacro ,(symbolicate "WITH-PARSED-" name) ((form &optional ref-p) &body body)
         `(let (,@',(remove-duplicates (mapcar #'car tag-kinds)))
            (funcall (spec-parser ,',spec ,',binds ,',rest-patterns ,ref-p)
                     (ref-cdr ,form))
            nil ; don't leak
            ,@body))

       ,(labels ((sequential-p (entry) (and (listp entry) (eq (first entry) '<)))
                 (parallel-p   (entry) (and (listp entry) (eq (first entry) '=)))
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
                                       `(mapcar (lambda (r) (ref-coerce-symbol (ref-car r)))
                                                (apply #'append
                                                       (mapcar #'elements ,whole-tag))))
                                      ;; binding records may SHARE STRUCTURE
                                      ((type symbol)
                                       `(mapcar #'ref-coerce-symbol ,record)))))
                                ;; note: functions needed for parsing, normalize env
                                (:function
                                 `(env-with-functions
                                   ,env-exp
                                   ,(trivia:ematch record
                                      ((list (type symbol) whole-tag)
                                       (with-gensyms (name info)
                                         `(loop
                                            for ,name
                                              in ,(first (cdr (assoc whole-tag
                                                                     rest-patterns)))
                                            for ,info
                                              in ,(lastcar (cdr (assoc whole-tag
                                                                       rest-patterns)))
                                            collect
                                            (list* (ref-coerce-symbol ,name)
                                                   (function-info-arglist ,info)
                                                   (function-info-body ,info)))))
                                      ((type symbol)
                                       `(mapcar #'ref-coerce-symbol ,record)))))
                                (:block `(env-with-blocks ,env-exp ,record))))
                          rest))
                       env-exp)))
          `(progn
             ;; the walker operates on raw code
             (defun ,(symbolicate name "-WALKER") (rawform env walker on-binder)
               (declare (ignorable env walker on-binder))
               (,(symbolicate "WITH-PARSED-" name) (rawform)
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
                                          (funcall walker form newenv-with-params))
                                        #'identity)
                                      `(map-lambda-list
                                        (function-info-arglist ,info)
                                        #'note-binder
                                        (lambda (form)
                                          (funcall walker form newenv-with-params))
                                        #'identity
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
               (,(symbolicate "WITH-PARSED-" name) (rawform t)
                (let ((ast (make-instance ',classname
                                          :op (if (typep (ref-car rawform) 'reader-marker)
                                                  ',(symbolicate "READ-" name)
                                                  ',name))))
                  ,@
                  (loop
                    for tag in slots
                    for entries := (plist-alist (cdr (assoc tag binds)))
                    for tag-kind := (cdr (assoc tag tag-kinds))
                    collect
                    (flet ((walk-function-body (env info augment-body kind)
                             `(loop
                                with newenv-with-params := ,env
                                with body := (list)
                                with lambda-list
                                  := (flet ((note-binder (binder)
                                              (change-class binder 'binder)))
                                       ,(if (eq kind '&macro-lambda)
                                            `(map-macro-lambda
                                              (function-info-arglist ,info)
                                              #'note-binder
                                              (lambda (form)
                                                (funcall walker form newenv-with-params))
                                              #'identity
                                              alter-identity)
                                            `(map-lambda-list
                                              (function-info-arglist ,info)
                                              #'note-binder
                                              (lambda (form)
                                                (funcall walker form newenv-with-params))
                                              #'identity
                                              ,(eq kind '&method-lambda)
                                              nil alter-identity)))
                                for body-form in (function-info-body ,info)
                                for body-ast = (funcall walker body-form ,augment-body)
                                do (push body-ast body)
                                finally
                                   (return
                                     (make-instance
                                      'function-code
                                      :lambda-list-kind ',kind
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
                                  (with-gensyms (res newenv whole new-whole b)
                                    `(loop
                                       with ,res := (list)
                                       with ,newenv := ,(augment-env `env init-binds)
                                       for ,whole in (elements (first ,tag))
                                       do (if (typep ,whole 'symbol-ref)
                                              (push (change-class ,whole 'binder) ,res)
                                              (let* ((,b (ref-car ,whole))
                                                     (,new-whole
                                                       (with-elements
                                                         ,whole
                                                         `(,(change-class ,b 'binder)
                                                           ,@(mapcar
                                                              (rcurry walker ,newenv)
                                                              (ref-cdr ,whole))))))
                                                (funcall alter-identity ,whole ,new-whole)
                                                (push ,new-whole ,res)))
                                       finally (setf (,tag ast)
                                                     (parsed-list (first ,tag)
                                                                  (nreverse ,res)
                                                                  alter-identity))))
                                  ;; normal, parallel bindings
                                  (with-gensyms (whole)
                                    `(setf
                                      (,tag ast)
                                      (parsed-list
                                       (first ,tag)
                                       (mapcar
                                        (lambda (,whole)
                                          (if (typep ,whole 'symbol-ref)
                                              (change-class ,whole 'binder)
                                              (funcall
                                               alter-identity ,whole
                                               (with-elements
                                                 ,whole
                                                 `(,(change-class (ref-car ,whole) 'binder)
                                                   ,,(if (cdr (assoc value-tag tag-kinds))
                                                         `(mapcar (rcurry walker env)
                                                                  (ref-cdr ,whole)) ;(b &body)
                                                         `(funcall walker
                                                                   (ref-nth 1 ,whole)
                                                                   env)))))))
                                        (elements (first ,tag)))
                                       alter-identity))))))
                           ;; function-like bindings
                           ((list (type symbol)
                                  (and (or (eql '&lambda) (eql '&macro-lambda)) lambda-kind)
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
                                                                   `(,,name))
                                                 lambda-kind)
                                   ;; note: NAME-TAG and CODE-TAG are accumulated by push,
                                   ;; this loop reverses the order back to normal
                                   do (push (list (change-class ,name 'binder) ,fun) ,res)
                                   finally
                                      (setf (,tag ast)
                                            (parsed-list
                                             (first ,tag)
                                             ;; each definition is rebuilt from what was
                                             ;; written, which keeps its anchors
                                             (mapcar (lambda (written pair)
                                                       (funcall alter-identity written
                                                                (with-elements written
                                                                  pair)))
                                                     (elements (first ,tag)) ,res)
                                             alter-identity))))))))
                        ;; non-&rest binder
                        ((loop for (ctx . %entries) in binds
                               thereis (cdr (rassoc tag (plist-alist %entries))))
                         `(setf (,tag ast)
                                (when-let (b (first ,tag)) (change-class b 'binder))))
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
                                                            `newenv-with-params tag-kind))))))))))
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

;;
;;; quasiquotation
;;
;; Code using reader macros (like in this file) requires markers to tell apart reader
;; syntax from explicitly written symbols. There's also a bug in eclector which causes
;; `(,'eclector.reader:quasiquote) to expand to (append (list (quote quote nil)) 'nil).
;; So instead of macroexpanding, we traverse the ast ourselves with a custom walker
;; using the reader-marker objects inserted in operator position
;;
(defform (eclector.reader:quasiquote &tree thing))

(defun map-quasiquote (thing on-eval &optional (on-data (constantly nil))
                                       (alter-identity (lambda (old new)
                                                         (declare (ignore old))
                                                         new)))
  "Walks `thing', the quoted tree of a quasiquote form, rebuilding an isomorphic tree with
`on-eval' applied to every subform an unquote actually evaluates and `on-data' to every
other atom."
  (labels ((rec (form depth)
             (if (ref-atom-p form)
                 (funcall on-data form)
                 ;; only a READER-MARKER is syntax, so source naming one of these symbols is
                 ;; data. Raw symbols come from the walker since STRIP-WRAPPERS erases types
                 (case (typecase (ref-car form)
                         (symbol (ref-car form))
                         (reader-marker (resolve (ref-car form))))
                   (eclector.reader:quasiquote
                    (funcall alter-identity form
                             (with-elements form
                               (list (funcall on-data (ref-car form))
                                     (rec (ref-nth 1 form) (1+ depth))))))
                   ((eclector.reader:unquote eclector.reader:unquote-splicing)
                    (funcall alter-identity form
                             (with-elements form
                               (list (funcall on-data (ref-car form))
                                     (if (= 1 depth) ; 0 depth underneath
                                         (funcall on-eval (ref-nth 1 form))
                                         (rec (ref-nth 1 form) (1- depth)))))))
                   ;; note: raw sexps from the walker are still built cons by cons, which
                   ;; is also how a dotted tail keeps its shape
                   (t
                    (funcall alter-identity form
                             (if (typep form 'ref-list)
                                 (with-elements form
                                   (mapcar (rcurry #'rec depth) (elements form)))
                                 (cons (rec (car form) depth)
                                       (rec (cdr form) depth)))))))))
    (rec thing 1)))

(defun walk-quasiquote (form env walker on-binder)
  (declare (ignore on-binder))
  (map-quasiquote (second form) (lambda (subform) (funcall walker subform env)))
  nil)

(defun parse-quasiquote (rawform env walker alter-identity)
  (make-instance
   'quasiquote-form
   :op (if (typep (ref-car rawform) 'reader-marker)
           'read-quasiquote
           'eclector.reader:quasiquote)
   :thing (map-quasiquote (ref-nth 1 rawform)
                          (lambda (subform) (funcall walker subform env))
                          #'identity alter-identity)))

(defun quasiquote-sorts (node)
  "See lambda-list-sorts"
  (map-quasiquote (thing node) (constantly 'eval-form) (constantly 'unevaluated)))

(defmethod location-sort ((node quasiquote-form) id)
  (trivia:match id
    ((eql 'thing) 'unevaluated)
    ((list* (eql 'thing) path) (ref-tree-ref (quasiquote-sorts node) path))))

(setf (gethash 'eclector.reader:quasiquote *special-walkers*) #'walk-quasiquote)
(setf (gethash 'eclector.reader:quasiquote *special-parsers*) #'parse-quasiquote)

(defform (defmethod &or (name &rest-qualifiers qualifiers &method-lambda fun-code)
                    ((setf-op name) &rest-qualifiers qualifiers &method-lambda fun-code))
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

(defform (defun &or (name &lambda fun-code) ((setf-op name) &lambda fun-code))
  :binds ((fun-code :block name :function name)))

(defform (lambda &lambda fun-code)
  :binds ((fun-code)))

(defform (defmacro name &macro-lambda macro-code)
  :binds ((macro-code :block name)))

(defform (function fun-designator))
(defform (quote &tree thing))

(defform (setq &body forms)
  :binds ((forms)))

(defform (return-from name &body value)
  :binds ((value)))
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
(defform (multiple-value-prog1 &body body)
  :binds ((body)))

(defform (progn &body forms)
  :binds ((forms)))
(defform (progv var-list val-list &body body) ; don't track dynamic bindings
  :binds ((var-list) (val-list) (body)))

(defun walk-form (form env on-form &optional (note-binder (constantly nil)))
  "Form and env must consist of ordinary s-expressions, remove wrappers prior to walking."
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
                  (if (atom newform)
                      (funcall on-form newform env)
                      (let ((newop (car newform)))
                        (if-let (walker (gethash newop *special-walkers*))
                          (funcall walker
                                   newform env
                                   (rcurry #'walk-form on-form note-binder)
                                   note-binder)
                          ;; must be function call
                          (when (funcall on-form newform env)
                            (mapcar (rcurry #'walk-form env on-form note-binder)
                                    (cdr newform))))))
                  (when (funcall on-form form env)
                    (mapcar (rcurry #'walk-form env on-form note-binder)
                            (cdr form))))))))))

(defun strip-wrappers (form interned)
  "`interned' is a vector of symbols for temporary internment during macro analysis."
  (cond ((typep form 'ref-list)
         (let* ((elts (elements form))
                (dot (position-if (lambda (e) (typep e 'dot-marker)) elts))
                (stripped
                  (mapcar (rcurry #'strip-wrappers interned)
                          (if dot
                              (append (subseq elts 0 dot) (nthcdr (1+ dot) elts))
                              elts))))
           (ecase (kind form)
             (:vector (coerce stripped 'vector))
             (:array (let ((contents (first stripped)))
                       (if (zerop (rank form))
                           (make-array nil :initial-element contents)
                           (make-array (loop repeat (rank form)
                                             for x := contents then (first x)
                                             collect (length x))
                                       :initial-contents contents))))
             (:list (if dot
                        (append (butlast stripped) (car (last stripped)))
                        stripped)))))
        ((atom form)
         (cond ((and (typep form 'wrapped-symbol) (null (home-package form)))
                (make-symbol (name form)))
               ((typep form 'wrapped-symbol)
                (multiple-value-bind (sym status)
                    (resolve form)
                  (if status
                      sym
                      (let ((new (intern (name form) (home-package form))))
                        (vector-push-extend new interned)
                        new))))
               ((typep form 'literal) (read-from-string (str form)))
               (t form)))
        ;; note: the reader represents dotted lists with a "." symbol marker,
        ;; make sure to replace this before attempting ordinary lisp macroexpansion.
        ((and (consp (cdr form)) (typep (cadr form) 'dot-marker))
         (cons (strip-wrappers (car form) interned)
               (strip-wrappers (caddr form) interned)))
        (t (cons (strip-wrappers (car form) interned)
                 (strip-wrappers (cdr form) interned)))))

(defun strip-env (env interned)
  "Converts an env built while parsing into one over real symbols/sexps."
  ;; note: the temp internment phase means we can't strip environments during parse
  (flet ((strip-binding (entry)
           (if (consp entry)
               (cons (strip-wrappers (car entry) interned)
                     (strip-wrappers (cdr entry) interned))
               (strip-wrappers entry interned))))
    (make-env :%function-bindings (mapcar #'strip-binding (function-bindings env))
              :%variable-bindings (mapcar #'strip-binding (variable-bindings env))
              :%blocks (mapcar #'strip-binding (blocks env))
              :%tags (tags env))))

(defmacro with-temporary-interning ((interned) &body body)
  "Runs `body', then uninterns all symbols in the vector `interned'."
  (with-gensyms (sym)
    `(let ((,interned (make-array 0 :adjustable t :fill-pointer t)))
       (multiple-value-prog1 (progn ,@body)
         (loop for ,sym across ,interned
               do (unintern ,sym (symbol-package ,sym)))))))

;;
;;; macro analysis via perturbation
;;
(defun macro-call-envmap (call env &optional (walker (constantly nil)))
  "Identifies body forms and binding scopes to return a map of {sexp -> binding env}.
Walks subforms of the call using WALKER during analysis."
  (with-temporary-interning (interned)
    (labels ((call-nth (i form)
               ;; we need to map paths in the raw source back to the parsed representation
               ;; with symbolic dot markers
               (let ((rest (nthcdr i (elements form))))
                 (if (typep (car rest) 'dot-marker)
                     (cadr rest) ; the real elt is one later
                     (car rest))))
             (lookup-path (path form)
               (loop for i in (reverse path)
                     do (setf form (call-nth i form))
                     finally (return form))))
      (let* ((raw-form->loc (make-hash-table :test #'eq))
             (raw-form->form (make-hash-table :test #'eq))
             (possible-identifiers (make-hash-table :test #'eq))
             (eval->binders (make-hash-table :test #'eq))
             (raw (strip-wrappers call interned))
             (env (strip-env env interned)))
        ;; identify all forms in the original call for classification
        ;; some may be constants, others are binders and expressions
        ;; record their source sym-paths for reconstruction
        (labels ((walk-call-collecting-forms (form path)
                   (loop for rest = form then (cdr rest)
                         for i from 0
                         while (consp rest)
                         for subform = (car rest)
                         for newpath = (cons i path)
                         do (if (consp subform)
                                (progn
                                  (push newpath (gethash subform raw-form->loc))
                                  (walk-call-collecting-forms subform newpath))
                                (when (symbolp subform)
                                  (push newpath (gethash subform raw-form->loc))))
                            ;; a dotted tail's last cdr is itself a subform at this position
                            ;; we only care if it's a symbol as ~(consp rest) from while
                         finally (when (and (not (null rest)) (symbolp rest))
                                   (push (cons i path) (gethash rest raw-form->loc))))))
          (walk-call-collecting-forms raw (list)))
        ;; - macroexpand fully up to special (or hardwired macro) forms,
        ;;   record *only* binders and obvious evaluation contexts seen in the output
        ;; - call the walker meanwhile
        (walk-form (handler-case (env-macroexpand raw env)
                     (error () nil))
                   env
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
                   ;; binder currently may be unused TODO block analysis
                   (lambda (sym kind)
                     (setf (gethash sym possible-identifiers) kind)))
        ;;(disp (hash-table-plist possible-identifiers))
        ;; analysis, first to understand syntax before constructing the returned maps
        (let ((gensym->path (make-hash-table :test #'eq))
              (gensym->refpath (make-hash-table :test #'eq)))
          (labels ((substitute-sym (path form sym)
                     (loop for rest on (reverse path)
                           for i := (car rest)
                           do (if (null (cdr rest))
                                  (if (= i 0)
                                      (setf (car form) sym)
                                      (let ((before (nthcdr (1- i) form)))
                                        (if (consp (cdr before))
                                            (setf (second before) sym)
                                            (setf (cdr before) sym))))
                                  (setf form (nth i form))))))
            (loop ; we need to know where the refs and binders are first
                  for sym being the hash-keys of possible-identifiers
                    using (hash-value kind)
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
                              ;; counter: step forms may have spurious refs generated
                              (if ident-kind
                                  (progn
                                    (setf (gethash gensym gensym->path) path)
                                    (when (eq ident-kind :ref)
                                      (setf (gethash gensym gensym->refpath) path)))
                                  (substitute-sym path raw sym))))
            ;; (disp (hash-table-plist gensym->path))
            ;; (disp (hash-table-plist gensym->refpath))
            ;; - map gensyms back to the call and change class to binder
            ;; - key these under the form from the call, via raw-form->form
            (let ((expansion (handler-case (env-macroexpand raw env)
                               (error () +fail+))))
              (unless (eq expansion +fail+)
                (walk-form
                 expansion env
                 (lambda (form subenv)
                   (flet ((calc-bindings ()
                            (loop
                              with res := (list)
                              for v in (ldiff (variable-bindings subenv)
                                              (variable-bindings env))
                              do (with-lookup (path (gethash (ensure-car v) gensym->path))
                                   (let ((binder (lookup-path path call)))
                                     ;; note: on-binder idempotent callback, may see the
                                     ;; same binder again via ldiff at deeper nesting
                                     (assert (typep binder 'wrapped-symbol))
                                     (unless (typep binder 'binder)
                                       (change-class binder 'binder))
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
            ;; (disp (hash-table-plist eval->binders))
            eval->binders))))))

(defun lambda-expression-p (x)
  "((lambda (v) v) 1) is a call whose operator is written out rather than named."
  (and (ref-form-p x)
       (let ((head (ref-car x)))
         (and (typep head 'symbol-ref) (eq (resolve head) 'lambda)))))

(defun parse-call (form env alter-identity)
  (funcall alter-identity form
           (make-instance 'function-call
                          :name (parse (ref-car form) env alter-identity)
                          :body (mapcar (rcurry #'parse env alter-identity)
                                        (ref-cdr form)))))

(defun parse (form env alter-identity)
  (cond
    ((or (not (ref-form-p form)) (ref-atom-p form))
     ;; note: a LABEL-DEF is atomic here because in practice only literals are labelled
     (assert (typep form '(or symbol-ref literal array label-def ref-list)))
     form)
    ((lambda-expression-p (ref-car form)) (parse-call form env alter-identity))
    ((not (typep (ref-car form) 'symbol-ref))
     (error "non-operator ~a in car" (ref-car form)))
    (t
     (if-let (parser (gethash (resolve (ref-car form)) *special-parsers*))
       (funcall alter-identity form
                (funcall parser form env (rcurry #'parse alter-identity) alter-identity))
       (multiple-value-bind (result local-expansion)
           (env-function-info (resolve (ref-car form)) env)
         (flet ((parse-function (form) (parse-call form env alter-identity))
                ;; don't expand explicitly, we only care about explicit call subforms
                (parse-macro (form)
                  (let ((macro-subforms (make-hash-table :test #'eq)))
                    (funcall alter-identity form
                             (make-instance
                              'macro-call
                              :op (ref-car form) :body (ref-cdr form)
                              :subform-asts macro-subforms
                              :eval-binders
                              (macro-call-envmap
                               form env
                               (lambda (form env)
                                 (let ((parsed-subform (parse form env alter-identity)))
                                   (setf (gethash form macro-subforms) parsed-subform)
                                   (funcall alter-identity form parsed-subform)))))))))
           (cond ((null result) ; global
                  (let ((sym (resolve (ref-car form))))
                    (if (and sym (macro-function sym) (not (hardwired-p sym)))
                        (parse-macro form)
                        (parse-function form))))
                 ;; local
                 ((null local-expansion) (parse-function form))
                 (t (parse-macro form)))))))))

;; for totality of function-call names
(defmethod name ((node lambda-form)) "lambda")
(defmethod location-sort ((node function-call) id)
  (trivia:match id
    ((eql 'name) 'symbol-ref)
    ((list (eql 'body) (type integer)) 'eval-form)))

;;
;;; eclector reader
;;
(defclass my-client (eclector.parse-result:parse-result-client)
  ((source :initarg :source
           :initform (error "no source")
           :reader source)))


;; and or not
(defclass read-cond (eval-form atom-form)
  ((str :initarg :str
        :initform (error "no str")
        :reader str
        :documentation "may include comments between the conditional and object")
   (flags :initarg :flags
          :initform (error "no flags")
          :reader flags)
   (kind :initarg :kind
         :initform (error "no kind")
         :reader kind
         :type (or (eql #\+) (eql #\-)))))

(defmethod copy-node ((old read-cond))
  (make-instance 'read-cond :kind (kind old) :flags (flags old) :str (str old)))

(defmethod print-object ((object read-cond) stream)
  (format stream "<~a~a ~a>" (kind object) (flags object) (str object)))

(defstruct read-evaluated form feature-p)
(defstruct read-conditional
  "Tags a value read as a feature expression, which eclector doesn't distinguish from a
keyword or a list. Consumed by the conditional one level up, never part of the tree."
  result)

(defun label-reference-p (client source)
  "Whether the span is some #n#. Needed for objects in construction."
  (let ((str (source client)) (start (car source)) (end (cdr source)))
    (and (> (- end start) 2)
         (char= (char str start) #\#)
         (char= (char str (1- end)) #\#)
         (every #'digit-char-p (subseq str (1+ start) (1- end))))))

(defun label-name (client source)
  "pulls out the numeric part from #n= or #n#"
  (multiple-value-bind (n end)
      (parse-integer (source client) :start (1+ (car source)) :junk-allowed t)
    (declare (ignore n))
    (subseq (source client) (1+ (car source)) end)))

(defun make-label-ref (client source)
  (make-instance 'label-ref :name (label-name client source) :home-package nil))

(defun wrap-marker (sym)
  "For reader syntax, which must stay distinct from explicit references to that symbol."
  (make-instance 'reader-marker :name (string sym) :home-package (symbol-package sym)))

(defun push-leading (node item)
  "Anchors `item', a comment or reader conditional, as written before `node'."
  (check-type node anchor)
  (push item (leading node)))

(defun anchor-leading (node comments)
  (unless (null comments)
    (check-type node anchor)
    (nconcf (leading node) comments)))

(defun anchor-trailing (node comments)
  (unless (null comments)
    (check-type node anchor)
    (nconcf (trailing node) comments)))

(defun anchor-inside (node comments)
  (unless (null comments)
    (check-type node anchor)
    (nconcf (inside node) comments)))

(defun sharpsign-start (client source)
  "work backwards from the list returned by eclector"
  (let* ((str (source client))
         (hash (position #\# str :end (car source) :from-end t)))
    (if (and hash (loop for i from (1+ hash) below (car source)
                        always (alphanumericp (char str i))))
        hash
        (car source))))

(defmethod eclector.reader:make-structure-instance
    ((client my-client) (name t) (initargs t))
  nil)

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result t) (children t) (source t))
  (if (null children)
      (cond ((label-reference-p client source) (make-label-ref client source))
            ((symbolp result)
             (cond ((and (keywordp result) (= (- (cdr source) (car source))
                                              (length (string result))))
                    ;; note: eclector doesn't distinguish a literal keyword from a feature
                    (make-read-conditional :result result))
                   ((and (null result) (= 2 (- (cdr source) (car source))))
                    (ref-list))
                   ;; note: eclector doesn't distinguish #:|.| and |.|
                   ((and (null (symbol-package result))
                         (= 1 (- (cdr source) (car source)))
                         (string= (string result) "."))
                    (make-instance 'dot-marker :name "." :home-package nil))
                   (t
                    (make-instance 'symbol-ref :name (string result)
                                               :home-package (symbol-package result)))))
            ((and (atom result) (constantp result))
             (cond ((and (vectorp result) (zerop (length result))
                         (char= #\( (schar (source client) (1+ (car source)))))
                    (make-instance 'ref-list :kind :vector))
                   (t
                    (make-instance 'literal :str (subseq (source client)
                                                         (car source) (cdr source))))))
            (t (error "weird constant ~a" result)))
      ;; compound form, or some wrapper
      (labels ((frobber ()
                 "the second value holds contained comments if the list was empty"
                 (loop with comments = nil
                       with noncomments = (list)
                       for c in children
                       do (if (not (typep c '(or comment read-cond)))
                              (progn
                                (anchor-leading c (nreverse comments))
                                (push c noncomments)
                                (setf comments nil)) ; reset after reading form
                              (push c comments))
                       finally (return
                                 (if noncomments
                                     (progn (anchor-trailing (car noncomments)
                                                             (nreverse comments))
                                            (values (nreverse noncomments) nil))
                                     (values nil (nreverse comments))))))
               (written-list (&optional (kind :list) rank)
                 (multiple-value-bind (res comments) (frobber)
                   (let ((list (make-instance 'ref-list :elements res
                                                        :kind kind :rank rank)))
                     ;; nothing but comments were written inside it
                     (anchor-inside list comments)
                     list)))
               (applied-conditional-p ()
                 (let ((str (source client)))
                   (and (< (1+ (car source)) (length str))
                        (char= #\# (schar str (car source)))
                        (member (schar str (1+ (car source))) '(#\+ #\-)))))
               (frob-cons ()
                 (cond
                   ((not (find-if #'read-conditional-p children))
                    (written-list))
                   ;; #+cond form
                   ((applied-conditional-p)
                    (let ((read-cond-pos (position-if #'read-conditional-p children))
                          (form (lastcar children)))
                      (loop
                        for c on children
                        for i below read-cond-pos
                        do (push-leading form (car c))
                        finally (push-leading
                                 form
                                 (make-instance
                                  'read-cond
                                  :kind (schar (source client) (1+ (car source)))
                                  :flags (read-conditional-result
                                          (nth read-cond-pos children))
                                  :str (subseq (source client) (car source)
                                               (cdr source)))))
                      (lastcar children)))
                   (t
                    ;; a feature expression, tagged for the conditional above to consume
                    (make-read-conditional
                     :result (mapcar (lambda (c) (if (read-conditional-p c)
                                                (read-conditional-result c)
                                                c))
                                     children))))))
        (typecase result
          (cons
           (case (car result)
             (function
              (ref-list (if (and (typep (first children) 'symbol-ref)
                                 (eq (resolve (first children)) 'function))
                            (first children)
                            (wrap-marker 'function))
                        (lastcar children)))
             (quote
              (ref-list (if (and (typep (first children) 'symbol-ref)
                                 (eq (resolve (first children)) 'quote))
                            (first children)
                            (wrap-marker 'quote))
                        (lastcar children)))
             (eclector.reader:quasiquote
              (ref-list (wrap-marker 'eclector.reader:quasiquote) (lastcar children)))
             (eclector.reader:unquote
              (ref-list (wrap-marker 'eclector.reader:unquote) (lastcar children)))
             (eclector.reader:unquote-splicing
              (ref-list (wrap-marker 'eclector.reader:unquote-splicing)
                        (lastcar children)))
             (t
              (frob-cons))))
          (read-evaluated
           (let ((form (ref-list (wrap-marker 'read-eval) (lastcar children))))
             ;; as marked by eclector.reader:evaluate-expression below
             (if (read-evaluated-feature-p result)
                 (make-read-conditional :result form)
                 form)))
          (vector (if (char= #\( (schar (source client) (1+ (car source))))
                      (written-list :vector)
                      (written-list :array (parse-integer (source client)
                                                          :start (1+ (car source))
                                                          :junk-allowed t))))
          (array (written-list :array (parse-integer (source client)
                                                     :start (1+ (car source))
                                                     :junk-allowed t)))
          (t
           (cond ((member-if #'read-conditional-p children)
                  (frob-cons))
                 ;; atom with children #C(1 2), #P"/tmp", #S(pt :x 1)
                 (result
                  (make-instance 'literal :str (subseq (source client)
                                                       (car source) (cdr source))))
                 (t
                  (multiple-value-bind (res comments) (frobber)
                    (if res
                        (make-instance 'literal
                                       :str (subseq (source client)
                                                    (sharpsign-start client source)
                                                    (cdr source)))
                        ;; only comments were written, inside a list of no elements
                        (let ((empty (ref-list)))
                          (anchor-inside empty comments)
                          empty))))))))))

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
       (make-instance 'read-cond :kind #\+ :flags res
                                 :str (source-str (car source) (cdr source))))
      ((cons (eql :sharpsign-minus) res)
       (make-instance 'read-cond :kind #\- :flags res
                                 :str (source-str (car source) (cdr source))))
      ((eql '*read-suppress*)
       (source-str (car source) (cdr source))))))

(defmethod eclector.reader:find-character ((client my-client) (designator string))
  (or (name-char designator) (call-next-method)))

;; capture read-evaluated forms explicitly
(defmethod eclector.reader:evaluate-expression ((client my-client) (expression t))
  (make-read-evaluated
   :form expression
   ;; note: assume a feature expression is the sole place with *package* = KEYWORD
   :feature-p (eq (eclector.reader:state-value client '*package*)
                  (find-package '#:keyword))))

(defmethod eclector.reader:evaluate-feature-expression
    ((client my-client) (expression read-evaluated))
  (eclector.reader:evaluate-feature-expression
   client (eval (read-evaluated-form expression))))

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result eclector.parse-result:definition) (children t) (source t))
  "#n=form. The label is read back out of the source because the labeled object itself is
passed with dynamic extent."
  (make-instance 'label-def :name (label-name client source)
                            :labeled (lastcar children)))

(defmethod eclector.parse-result:make-expression-result
    ((client my-client) (result eclector.parse-result:reference) (children t) (source t))
  (declare (ignore children))
  (make-label-ref client source))

(defmethod eclector.reader:fixup-graph-p ((client my-client) (root t))
  "This is a tree editor not a graph editor."
  nil)

(defun migrate-anchors (old new)
  "`new' stands where `old' was written, so it takes over what was anchored to it."
  (when (and (typep old 'anchor) (typep new 'anchor)
             (not (eq old new)))
    (macrolet ((take-over (slot)
                 `(unless (eq (,slot new) (,slot old))
                    (setf (,slot new) (append (,slot old) (,slot new))))))
      (take-over leading)
      (take-over trailing)
      (take-over inside)))
  new)

(defun parse-from-string (s)
  (let ((client (make-instance 'my-client :source s)))
    (multiple-value-bind (form len leading-comments)
        (eclector.parse-result:read-from-string client s)
      (declare (ignore len))
      (let ((res (parse form (make-env :%function-bindings '(read-eval))
                        #'migrate-anchors)))
        ;; note res can be a toplevel atom
        (setf (leading res) (append leading-comments (leading res)))
        res))))
