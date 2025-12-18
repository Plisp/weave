;;;;
;;;; incremental lisp parsing
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
       (format t "~%~s~%|> ~a~%" ',form ,res)
       ,res)))

(defun addr-str (obj)
  (let* ((str (delete-if (lambda (c) (member c '(#\# #\< #\> #\Space #\{ #\})))
                         (with-output-to-string (s)
                           (print-unreadable-object (obj s :identity t)))))
         (length (length str)))
    (if (>= length 3)
        (subseq str (- length 3))
        str)))

(defun lfind (item list &key (key #'identity) (test #'eql) (start 0) (end (length list)))
  "NIL-detecting version of find for lists, only searches forwards"
  (declare (optimize speed)
           (type fixnum start end)
           (type list list))
  (assert (<= 0 start end (length list)))
  (loop for elt in (nthcdr start list)
        do (when (funcall (the function test) item (funcall (the function key) elt))
             (return (values elt t)))))

(defmacro with-lookup ((name (&rest mvcall) &optional default) &body then)
  (with-gensyms (blockname present-p)
    `(block ,blockname
       (multiple-value-bind (,name ,present-p)
           ,mvcall
         (when ,present-p
           (return-from ,blockname (progn ,@then))))
       ,default)))

(define-modify-macro or-f (&rest forms) or)

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
  (find name (variable-bindings env) :key 'first))
(defun env-function-info (name env)
  (find name (function-bindings env) :key 'first))

(defun env-with-variables (env bindings)
  (let ((new-env (copy-env env)))
    (setf (variable-bindings new-env) (append bindings (variable-bindings new-env)))
    new-env))

(defun env-with-functions (env bindings)
  (let ((new-env (copy-env env)))
    (setf (function-bindings new-env) (append bindings (function-bindings new-env)))
    new-env))

(defun env-with-blocks (env bindings)
  (let ((new-env (copy-env env)))
    (setf (blocks new-env) (append bindings (blocks new-env)))
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
  (position macro-name *hardwired-operators*))

(defmethod env-macroexpand-1 (x env)
  "Tries macroexpanding a form x in evaluation position once,
does not touch hardwired operators."
  (cond ((symbolp x)
         (if-let (local-expansion (second (env-variable-info x env)))
           (values local-expansion t)
           (macroexpand-1 x))) ; possible global macro, or function call
        ((consp x)
         (let* ((lexical-info (env-function-info (first x) env))
                (local-expansion (cdr lexical-info))
                (op (car x)))
           (cond ((null lexical-info) ; global
                  (if (and (symbolp op) ; could be lambda
                           (macro-function op) (not (hardwired-p op)))
                      (macroexpand-with-env x env)
                      x))
                 ((null local-expansion) x) ; flet/labels bound
                 (t (macroexpand-with-env x env))))) ; local macro
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
                           (list* (list call-name (funcall on-binder x)) value)))
                        ;; everything
                        ((list (list call-name (and (type symbol) x)) val
                               (and (type symbol) supplied-p))
                         (let ((value (maybe-default val)))
                           (list* (list call-name (funcall on-binder x)) value
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
;;; if lisp is mud then parsing is the experience of it slipping through your fingers
;;

(defparameter *special-walkers* (make-hash-table :test 'eq))

(eval-when (:compile-toplevel)
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
             (assert (or (eq (car spec) '&or)
                         (and (second spec)
                              (symbolp (second spec))
                              (not (spec-keyword-p (second spec))))))
             (when (member (car spec) *arity-1-parser-keywords* :test 'eq)
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
    (arglist nil)
    (documentation nil :type (or null string))
    (decls nil)
    (body nil)))

(defmacro defform ((name &rest spec) &key binds rest-patterns)
  "Generates an AST type and scope parser associated with a macro or
special operator NAME. The generated parser performs checking and signals form-parse-error
when runtime matching fails.

Keywords starting with & have special meaning and have arity 1, except for &or.
Only one &body/&rest may occur per scope, designating an implicit progn similar to lambda
lists. Rest-patterns destructure and parse subforms but do not create fields.
Any binding forces a symbol match.

Every entry (ctx . entries) in BINDS denotes an evaluation context 'ctx' in which the
corresponding lexical entries are bound. ctx is a symbol and entries is a plist (see below)
Binding tag names **must not** be (member NIL < =). For each ctx, may have either:
`<` for sequential variable binding
`=` to indicate a rest entry in which parallel block bindings occur
There's also a special syntax for macro bindings, but that isn't settled."
  (let ((parser-name (symbolicate name "-CONS-PARSER"))
        (custom-bind-rest-labels nil)
        (tag-kinds (nconc (spec-names spec)
                          (mapcan (lambda (pair) (spec-names (cdr pair)))
                                  rest-patterns))))
    (loop
      for (ctx . entries) in binds
      do (assert (symbolp ctx))
         (loop
           with special-seen
           for (kind bind-tag) on entries
           while bind-tag
           do (trivia:ematch bind-tag
                ((type symbol))
                ((list (or (eql '<) (eql '=)) bind-name)
                 (assert (not special-seen))
                 (setf special-seen t)
                 (when (eq (car bind-tag) '<) (assert (or (eq kind :variable))))
                 (when (eq (car bind-tag) '=) (assert (or (eq kind :block))))
                 (assert (symbolp bind-name))
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

    `(progn
       (defclass ,(symbolicate name "-FORM") (irregular-form)
         ((op :initform ',name)
          ,@(loop for name in (delete-duplicates (mapcar 'car (spec-names spec)))
                  collect `(,name :initarg ,(make-keyword name)
                                  :accessor ,name))
          ,@(when binds `((envmap :initarg :envmap :initform (error "no env!"))))))
       ;; spec validated above by spec-names ^
       (defmacro ,parser-name (spec)
         (cond
           ((null spec) `(lambda (form tagmap)
                           (declare (ignore tagmap))
                           (if (null form) nil
                               (form-parse-error "expected null, got ~a" form))))
           ((atom spec)
            `(lambda (form tagmap)
               (when (and ,(loop for (ctx . entries) in ',binds
                                 thereis (loop for (kind tag) on entries
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

       (defun ,(symbolicate name "-WALKER") (form env walker on-binder)
         (declare (ignorable walker env on-binder))
         (let ((tagmap (,(symbolicate name "-TAGGER") form)))
           (declare (ignorable tagmap))
           ,@(loop for (ctx-tag . entries) in binds
                   append
                   (loop for (kind record) on entries by 'cddr
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
             collect ;tag-kinds var < (block =)
             (labels ((sequential-p (entry) (and (listp entry) (eq (first entry) '<)))
                      (parallel-p (entry)   (and (listp entry) (eq (first entry) '=)))
                      (augment-env (env-exp entries)
                        (if entries
                            (destructuring-bind ((kind . record) &rest rest)
                                entries
                              (augment-env
                               (if (or (sequential-p record) (parallel-p record))
                                   env-exp
                                   (case kind
                                     (:variable
                                      `(env-with-variables
                                        ,env-exp
                                        ,(trivia:ematch record
                                           ((list (type symbol) whole-tag)
                                            `(apply 'append (gethash ',whole-tag tagmap)))
                                           ((type symbol)
                                            `(mapcar 'list (gethash ',record tagmap))))))
                                     (:function
                                      `(env-with-functions
                                        ,env-exp
                                        ,(trivia:ematch record
                                           ((list (type symbol) whole-tag)
                                            `(loop
                                               for (name . info)
                                                 in (apply 'append
                                                            (gethash ',whole-tag tagmap))
                                               collect
                                               (list* name (function-info-arglist info)
                                                      (function-info-body info))))
                                           ((type symbol)
                                            `(mapcar 'list (gethash ',record tagmap))))))
                                     (:block `(env-with-blocks
                                               ,env-exp (gethash ',record tagmap)))))
                               rest))
                            env-exp)))
               (let ((seq-tag (third (rassoc-if #'sequential-p entries)))
                     (par-tag (third (rassoc-if #'parallel-p entries)))
                     (ctx-kind (cdr (assoc ctx-tag tag-kinds))))
                 (flet ((walk-function-body (env info augment-body)
                          `(loop initially
                            (flet ((note-binder (binder)
                                     (setf newenv-with-params
                                           (env-with-variables newenv-with-params
                                                               (list (list binder))))
                                     (funcall on-binder binder :variable)))
                              ,(if (eq ctx-kind '&macro-lambda)
                                   `(map-macro-lambda (function-info-arglist ,info)
                                                      #'note-binder
                                                      (rcurry walker newenv-with-params))
                                   `(map-lambda-list (function-info-arglist ,info)
                                                     #'note-binder
                                                     (rcurry walker newenv-with-params)
                                                       ,(eq ctx-kind '&method-lambda))))
                                 with newenv-with-params = ,env
                                 for body-form in (function-info-body ,info)
                                 do (funcall walker body-form ,augment-body))))
                   (cond
                     (seq-tag
                      (let ((whole-tag (cdr (assoc seq-tag custom-bind-rest-labels))))
                        ;; to generalise for patterns: compile path to binder instead
                        `(or ; first detect whether a rest pattern was matched at all
                          (with-lookup (wholeforms (gethash ',whole-tag tagmap))
                            (loop for newenv := ,(augment-env `env entries)
                                    then (if (symbolp whole)
                                             (env-with-variables newenv (list whole))
                                             (env-with-variables newenv
                                                                 (list (first whole))))
                                  ;; note: must reverse since tags are backwards
                                  for whole in (reverse (apply #'append wholeforms))
                                  do (when (listp whole)
                                       ,(if ctx-kind
                                            `(loop for bodyform in (second whole)
                                                   do (funcall walker bodyform newenv))
                                            `(funcall walker (second whole) newenv))))
                            t)
                          (loop for initform
                                  in ,(if ctx-kind
                                          `(apply 'append (gethash ',ctx-tag tagmap))
                                          `(gethash ',ctx-tag tagmap))
                                do (funcall walker initform env)))))
                     (par-tag
                      (ecase ctx-kind
                        ((&body &rest nil) (error "unimplemented"))
                        ((&lambda &macro-lambda &method-lambda)
                         (let ((whole (cdr (assoc par-tag custom-bind-rest-labels))))
                           `(loop
                              with newenv = ,(augment-env `env entries)
                              for (name . info)
                                in (apply #'append (gethash ',whole tagmap))
                              ;; parallel bind the block only in the body
                              ;; newenv-with-params is CAPTURED
                              do ,(walk-function-body
                                   `newenv `info
                                   `(env-with-blocks newenv-with-params (list name))))))))
                     (t
                      (ecase ctx-kind
                        ((&body &rest nil)
                         `(loop with newenv = ,(augment-env `env entries)
                                for body-form
                                  in ,(if ctx-kind
                                          `(apply 'append (gethash ',ctx-tag tagmap))
                                          `(gethash ',ctx-tag tagmap))
                                do (funcall walker body-form newenv)))
                        ((&lambda &macro-lambda &method-lambda)
                         `(loop with newenv = ,(augment-env `env entries)
                                for info in (gethash ',ctx-tag tagmap)
                                do ,(walk-function-body `newenv `info ; vv CAPTURED
                                                        `newenv-with-params))))))))))))

       (setf (gethash ',name *special-walkers*) ',(symbolicate name "-WALKER"))
       (values))))

(defun test-walker (form)
  (disp (hash-table-plist (funcall (symbolicate (car form) "-TAGGER") form)))
  (funcall (symbolicate (car form) "-WALKER")
           form (make-env)
           (lambda (form env) (disp (list form env)))
           (lambda (binder kind) (disp (list binder kind)))))

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
  ;; XXX extremely annoying bespoke behavior since names may not line up with initforms
  :binds ((init :variable (< name))
          (body :variable name)))

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

(defform (function symbol-or-lambda)
  :binds ((symbol-or-lambda)))

(defform (quote thing))

(defform (if test then &body else) ; optional, but need structural editing
  :binds ((test) (then) (else)))

(defform (setq &body forms)
  :binds ((forms)))

(defform (return-from name &body value) ; optional, but may need to edit multiple forms
  :binds ((value)))

(defform (catch tag &body body)
  :binds ((body)))
(defform (throw catch-tag result)
  :binds ((result)))

(defform (load-time-value &body form-and-read-only-p)
  :binds ((form-and-read-only-p)))

(defform (eval-when (&rest-qualifiers directives) &body body)
  :binds ((body)))

(defform (locally &body body)
  :binds ((body)))
(defform (the type-specifier form)
  :binds ((form)))

(defform (tagbody &body tags-and-forms) ; XXX tags should be stored in env and not walked
  :binds ((tags-and-forms)))
(defform (go tag))

(defform (unwind-protect protected &body cleanup)
  :binds ((protected) (cleanup)))

(defform (multiple-value-call fun arg &body args)
  :binds ((fun) (arg) (args)))

(defform (multiple-value-prog1 value-form &rest forms)
  :binds ((value-form) (forms)))

(defform (progn &body forms)
  :binds ((forms)))
(defform (progv var-list val-list &body body) ; don't track dynamic bindings
  :binds ((var-list) (val-list) (body)))

(defun walk-form (form env on-form &optional (note-binder (constantly nil)))
  (if (atom form)
      (funcall on-form form env)
      (when (funcall on-form form env)
        (let ((op (car form)))
          (if (gethash op *special-walkers*)
              (funcall (gethash op *special-walkers*)
                       form env
                       (rcurry #'walk-form on-form note-binder)
                       note-binder)
              (multiple-value-bind (newform expanded-p)
                  (env-macroexpand form env)
                (when expanded-p
                  (let ((newop (car newform)))
                    (if (gethash newop *special-walkers*)
                        (funcall (gethash newop *special-walkers*)
                                 newform env
                                 (rcurry #'walk-form on-form note-binder)
                                 note-binder)
                        ;; must be function call
                        (when (funcall on-form newform env)
                          (mapcar (rcurry #'walk-form env on-form note-binder)
                                  (cdr newform))))))))))))

;;
;;; macro analysis via perturbation
;; TODO detect non-parametric, effectful macros, check binding type doesn't vary
(defun macro-call-envmap (form env)
  "Identifies body forms and binding scopes to return an envmap for FORM"
  (let ((call-tree-forms (make-hash-table :test 'eq))
        (possible-binders (make-hash-table :test 'eq)))
    ;; 1. identify all forms in the original call for classification
    ;;    some may be constants, others are binders and expressions
    ;;    record their source sym-paths for reconstruction
    (labels ((walk-call-collecting-forms (form path)
               (cond ((atom form)
                      (when form
                        (push path (gethash form call-tree-forms))))
                     ((consp (car form))
                      (push path (gethash (car form) call-tree-forms))
                      (walk-call-collecting-forms (car form) (cons 'car path))
                      (walk-call-collecting-forms (cdr form) (cons 'cdr path)))
                     (t ; atom in car
                      (push (cons 'car path) (gethash (car form) call-tree-forms))
                      (walk-call-collecting-forms (cdr form) (cons 'cdr path))))))
      (walk-call-collecting-forms form (list))
      (disp (hash-table-keys call-tree-forms)))
    ;; 2. macroexpand fully up to special (or hardwired macro) forms,
    ;;    and record all binders seen in the output
    (walk-form (env-macroexpand form env) env
               (lambda (form env) ; continue if:
                 (declare (ignore env))
                 (and (consp form) (null (gethash form call-tree-forms))))
               (lambda (sym kind)
                 (when (gethash sym call-tree-forms)
                   (setf (gethash sym possible-binders) kind))))
    (disp (hash-table-plist call-tree-forms))
    (disp (hash-table-plist possible-binders))
    ;; 3. for each env entry, substitute with a gensym then macroexpand,
    ;;    reexamine the expansion to see if it's still binding
    ;;    and record the call forms which are under its scope
    (let ((sym-paths (loop for binder being the hash-keys in possible-binders
                           append (loop for path in (gethash binder call-tree-forms)
                                        collect (cons binder path))))
          (path->gensym (make-hash-table :test 'equal))
          (gensym->path (make-hash-table :test 'eq))
          (gensym->call-forms (make-hash-table :test 'eq)))
      (labels ((note-gensym (path gensym)
                 (setf (gethash path path->gensym) gensym
                       (gethash gensym gensym->path) path))
               ;; substitute the symbol on PATH for another
               (substitute-sym (path form sym)
                 (loop for rest on (reverse path)
                       for op := (car rest)
                       do (if (= 1 (length rest))
                              (case op
                                (car (setf (car form) sym))
                                (cdr (setf (cdr form) sym)))
                              (setf form (funcall op form))))))
        (loop
          for (binder . path) in sym-paths
          for gensym := (gensym "PB")
          do (substitute-sym path form gensym)
             (note-gensym path gensym)
             ;;(disp (list 'expanding form))
             (let ((expansion (handler-case (env-macroexpand form env)
                                (error () '#1=#:fail#))))
               (unless (eq expansion '#1#)
                 (walk-form expansion env
                            (lambda (form env)
                              (when (consp form)
                                (if (gethash form call-tree-forms)
                                    (when (or (assoc gensym (variable-bindings env))
                                              (assoc gensym (function-bindings env))
                                              (position gensym (blocks env)))
                                      (push form (gethash gensym gensym->call-forms))
                                      nil)
                                    t))))))
             (substitute-sym path form binder))
        (disp (hash-table-plist gensym->call-forms))))))

;; test:
;; (loop for it from 1 to 10
;;       if (evenp it)
;;         collect it)

;; test:
;; (loop for it from from
;;       do (print it))

;; TODO test anaphoric macros, cffi, quasiquote: with-gensyms, once-only
