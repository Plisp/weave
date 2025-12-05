;;;;
;;;; lisp parsing and type inference
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
;;; class defs (note: the meaning of 'parent' may not be code)
;;

(defclass comment ()
  ((str :initarg :str
        :accessor str)
   (kind :initarg :kind
         :initform :line :type (or :line :block)
         :accessor kind))
  (:documentation ""))

(defclass special-form ()
  ((op :initarg :op
       :accessor op))
  (:documentation "macro invocation or special operator"))

(defclass eval-form ()
  ()
  (:documentation "Non-special form in an evaluation position"))

(defclass literal-form ()
  ((form :initarg :form
         :type string :initform (error "literal not provided")
         :accessor form))
  (:documentation "Atomic literal"))

(defclass binder ()
  ((name :initarg :name
         :initform (error "must provide bound symbol")
         :accessor name)
   (refs :initarg :refs
         :initform (list)
         :accessor refs))
  (:documentation "bound variable"))

;; does not point backwards to a binder, since we want to allow for incomplete
;; editing states. Besides looking up the stack is easy
(defclass symbol-ref (eval-form)
  ((name :initarg :name
         :initform (error "must provide symbol ref name")
         :accessor name)
   (kind :initarg :kind
         :initform :lexical :type (or :lexical :special)
         :accessor kind))
  (:documentation "Represents a symbol or symbol macro?"))

(defstruct (ref-info (:conc-name nil))
  (ref nil :type symbol-ref)
  (access-kind :eval :type (or (eql :eval) (eql :set)))
  (inferred-type t))

(defclass function-call (eval-form)
  ((name :initarg :name
         :initform (error "must provide function name")
         :accessor name)
   (args :initarg :args
         :initform (error "must provide function argument list")
         :accessor args))
  (:documentation ""))

(defmethod print-object ((object literal-form) stream)
  (princ (form object) stream))

(defmethod print-object ((object binder) stream)
  (pprint-logical-block (stream (refs object))
    (format stream "~a @" (name object))
    (loop
      (pprint-exit-if-list-exhausted)
      (write-char #\space stream)
      (write-string (addr-str (ref (pprint-pop))) stream))))

(defmethod print-object ((object symbol-ref) stream)
  (pprint-logical-block (stream (list))
    (format stream "~a<~a>" (name object) (addr-str object))))

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

(defun ref-symbol (ref-info)
  (name (ref ref-info)))

;;; special forms

;; block      let*                  return-from
;; catch      load-time-value       setq
;; eval-when  locally               symbol-macrolet
;; flet       macrolet              tagbody
;; function   multiple-value-call   the
;; go         multiple-value-prog1  throw
;; if         progn                 unwind-protect
;; labels     progv                 defun!! (impl-specific)
;; let        quote

(defclass let*-form (special-form)
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

(defmethod to-sexp ((ast let*-form))
  (list* (op ast)
         (mapcar (lambda (b) (list (name (car b)) (to-sexp (cdr b))))
                 (bindings ast))
         (mapcar #'to-sexp (body ast))))

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

;;
;;; code walking
;;

(defun macrolet-code-wrap (name params-and-body form)
  `(macrolet ((,name ,@params-and-body))
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
  (function-bindings (list))
  (variable-bindings (list))
  (blocks (list))
  (tags (list)))

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

(defmethod env-macroexpand-1 (x env) ; only apply to NON-HARDWIRED macros
  (cond
    ((symbolp x)
     (let* ((local-match (find x (variable-bindings env) :key #'first))
            (local-expansion (second local-match)))
       (cond ((null local-match) (macroexpand-1 x)) ; possible global symbol macro
             ((null local-expansion) (values x nil)) ; let bound
             (t (values local-expansion t)))))
    ((consp x)
     (let* ((local-match (find (first x) (function-bindings env) :key #'first))
            (local-content (cdr local-match)))
       (cond ((null local-match)
              (cond ((consp (first x)) (values x nil)) ; lambda in head position
                    ((macro-function (first x)) (macroexpand-with-env x env))
                    (t x)))
             ((null local-content) (values x nil)) ; flet/labels bound
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

(defun env-macroexpand-special-form (form env)
  (let ((op (first form)))
    (handle-special-form op form env)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (declaim (type simple-vector *hardwired-operators*))
  (defparameter *hardwired-operators*
    #(*literal-magic* defun defmethod defmacro cond multiple-value-bind handler-bind)
    "The list of nonportable hardwired macros, not macroexpanded"))

;;
;;; ast conversion
;;

(defun lambda-list-variable-names (l)
  (loop for name in l
        unless (find name lambda-list-keywords)
          collect (cond ((symbolp name) name)
                        ((symbolp (first name)) (first name))
                        (t (second (first name))))
        when (and (listp name) (third name)) collect (third name)))

(defun separate-declarations (entries &key (enable-docstring t))
  (loop
    with docstring := (not enable-docstring)
    with declarations := nil
    with forms := nil
    for header := t then header-continued
    for entry in entries
    for operator := (and (consp entry) (car entry))
    for header-continued := (and header
                                 (or (and (stringp entry)
                                          (not docstring)
                                          (setf docstring entry))
                                     (eq operator 'declare)))
    do (if header-continued (push entry declarations) (push entry forms))
    finally (return ; (lambda () "asd") is the same as (constantly "asd")
              (if (and (null forms) (stringp docstring)
                       (eq docstring (car declarations)))
                  (list (reverse (cdr declarations)) (list docstring))
                  (list (reverse declarations) (reverse forms))))))

(defclass function-information ()
  ((name :initarg :name :accessor function-information-name)
   (named :initarg :named :accessor function-information-named)
   (qualifiers :initarg :qualifiers :accessor function-information-qualifiers)
   (arglist :initarg :arglist :accessor function-information-arglist)
   (declarations :initarg :declarations :accessor function-information-declarations)
   (body :initarg :body :accessor function-information-body)))

(defun canonicalize-bindings (bindings)
  (loop for b in bindings
        collect (if (symbolp b)
                    (list b nil)
                    b)))

;; This makes no attempt to guess general types, as (impl) source may not be available
;; TODO: track macroexpansion depth
;; if a recursive macro invocation is not present in the form, macroexpand again
;; for every body form and symbol in the expansion, try to find it in the form
;;   - these can have meaning, constants do not
;; do simultaneous substitution of every found reference in the form for gensyms
;;   - no need to reexpand for forms, structure is shared
;;   - need per-symbol expansion for symbols, catch errors for stuff like loop keywords
;; match up scope data by searching the expansion for the gensyms
(defun find-macroexpansion-bindings (call env)
  "Form is a single macro call, which is macroexpanded to produce a scope description
for that particular instance."
  (let ((info (make-hash-table)))
    (labels
        ((search-tree (o tree)
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
                 (if (or (special-operator-p op)
                         (find (car call) *hardwired-operators*))
                     (case op
                       (let
                           (destructuring-bind
                               (let bindings &rest decls-body) form
                             (declare (ignore let))
                             (let ((names (mapcar #'first (canonicalize-bindings bindings))))
                               (dolist (name names)
                                 (when (search-tree name call)
                                   (pushnew name (gethash :variable info))))
                               ;; iterate body
                               (mapc (rcurry #'collect-bindings (env-with-variables env names))
                                     (second (separate-declarations decls-body))))))
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
      (assert (and (consp call) (not (find (car call) *hardwired-operators*))))
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

;; block      let*                  return-from
;; catch      load-time-value       setq
;; eval-when  locally               symbol-macrolet
;; flet       macrolet              tagbody
;; function   multiple-value-call   the
;; go         multiple-value-prog1  throw
;; if         progn                 unwind-protect
;; labels     progv                 defun!! (impl-specific)
;; let        quote                 defmethod

;; (let (:let (name init)) :decls body) name* . body
;; (let* (:let* (name init)) :decls body)
;; (flet (:flet (name params :rest fbody)) :decls body)
;;         lambda-list-variable-names params . fbody, fbind: (name . body)
;; (labels (:labels (name params :rest fbody)) :decls body)
;; names bound in all fbody, params bound in fbody, fbind name . body
;; (macrolet (:let (name macro-lambda-list :rest body)) :decls body)
;; (symbol-macrolet (:rest (name expansion)) :decls body) macrobind (name . body)
;; (defun name (function-lambda-list) :decls body)
;; (defmethod name (method-lambda-list) :decls body)
;; (block name body) block: name . body
;; (tagbody tags-and-forms)

;; (defmacro defspecial (name lambda-list)
;;   ())

(defun env-variable-info (name env)
  (find name (variables env)))

;; special operators
(defun env-with-variables (env bindings)
  (let ((new-env (copy-env env)))
    (loop for v in bindings
          do (push (list v) (variable-bindings new-env)))
    new-env))

(defun handle-let (bindings body-forms env)
  (let* ((body-env
           (env-with-variables env (mapcar #'first (canonicalize-bindings bindings))))
         (body-records
           (mapcar (lambda (form) (multiple-value-list (form-ast form body-env)))
                   body-forms))
         (ast (make-instance 'let-form :body (mapcar #'first body-records) :lexenv env))
         (body-refs (reduce #'append (mapcar #'second body-records)))
         (new-refs (list)))
    ;; ensure all bindings exist in the ast, we rely on this below
    (loop for binding in bindings
          do (if (symbolp binding)
                 (push (cons (make-instance 'binder :name binding) nil) (bindings ast))
                 (let ((init-ast-refs
                         (multiple-value-list (form-ast (second binding) env))))
                   (appendf new-refs (second init-ast-refs))
                   (push (cons (make-instance 'binder :name (car binding))
                               (first init-ast-refs))
                         (bindings ast)))))
    ;; process any body bindings of the let variables, leave the rest.
    ;; The ones that match any bound names here are *all* true references, since
    ;; we would already have removed any captures of that name before returning
    (loop for body-var-ref in body-refs
          for body-var-name := (ref-symbol body-var-ref)
          do (if-let (binding
                      (find body-var-name (bindings ast) :key (compose #'name #'car)))
               (push body-var-ref (refs (car binding)))
               (push body-var-ref new-refs)))
    (values ast new-refs)))

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
      (let ((record (find form (disp (variable-bindings env)) :key #'first)))
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
      (let ((record (find (car form) (function-bindings env) :key #'first)))
        (cond ((and record (eq (second record) nil))
               (make-instance 'symbol-ref :name form))
              ((and record (eq (second record) :macro))
               (error "local macro ~s" form))
              ((special-operator-p (car form))
               (handle-special form env))
              ((macroexpand-1 form)
               (error "global macro ~s" form))
              (t
               (error "unknown ~s?" form))))))
