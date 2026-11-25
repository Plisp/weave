(in-package :weave-parser)

;;; search helpers
(defun spec-match (subnode query spec)
  (when (typep query 'hole)
    (return-from spec-match t))
  ;; not a hole
  (labels ((fuzz-match-body (query body spec)
             ;; ignores holes as we want a single hole to match empty bodies
             (cond
               ((null (car query)) t)
               ((typep (car query) 'hole) (fuzz-match-body (cdr query) body spec))
               (t
                (when-let (next
                           (member-if (lambda (body-node)
                                        (node-match body-node (car query) spec))
                                      body))
                  (fuzz-match-body (cdr query) (cdr next) spec)))))
           (fuzz-match-spec (query rest spec)
             "requires query and rest to be gen-lists"
             (cond
               ((null (gen-car query)) t)
               ((typep (gen-car query) 'hole) (fuzz-match-spec (gen-cdr query) rest spec))
               (t
                (when-let (nexti
                           (position-if
                            (lambda (rest-node)
                              (spec-match rest-node (gen-car query) spec))
                            (elements rest)))
                  (fuzz-match-spec (gen-cdr query) (gen-nthcdr (1+ nexti) rest) spec)))))
           (tree-match (subnode query)
             (cond
               ((listp subnode)
                (and (listp query) (fuzz-match-spec query subnode 'unevaluated)))
               ((gen-atom-p subnode)
                (and (gen-atom-p query) (node-match subnode query 'unevaluated)))
               ((typep subnode 'ref-list)
                (and (typep query 'ref-list)
                     (eq (kind subnode) (kind query))
                     (fuzz-match-spec query subnode 'unevaluated))))))
    (if (listp spec)
        (case (car spec)
          (&rest (fuzz-match-spec query subnode (second spec)))
          (&or
           (or (spec-match subnode query (second spec))
               (spec-match subnode query (third spec))))
          (t
           (when (and (gen-list-p subnode) (gen-list-p query))
             ;; note: initially would be evaluated after loop initforms, bad for condition
             (loop with required = (or (position-if
                                        (lambda (s)
                                          (find s *parser-keywords*))
                                        spec)
                                       (length spec))
                   ;; allows shorter prefix matches
                   initially (unless (>= (length (elements subnode)) required)
                               (return nil))
                   for spec-rest on spec
                   for subspec = (car spec-rest)
                   for elts on (elements subnode)
                   for e = (car elts)
                   for qelts on (elements query)
                   for q = (car qelts)
                   for i from 0
                   always (case subspec
                            (&rest
                             (assert (= (length spec-rest) 2))
                             (return (loop for e in elts
                                           for q in qelts
                                           always (spec-match e q (second spec-rest)))))
                            (&optional
                             (prog1 (spec-match e q (second spec-rest))
                               (pop spec-rest)))
                            ((&rest-qualifiers &declarations &or)
                             (assert nil nil "unimplemented keywords"))
                            (t (spec-match e q subspec)))))))
        (case spec ; we aren't sure of a match at this point
          (eval-form (node-match subnode query 'eval-form))
          (symbol-ref (node-match subnode query 'symbol-ref))
          (binder (node-match subnode query 'binder))
          (function-code (node-match subnode query 'function-code))
          (fun-designator
           (typecase subnode
             (symbol-ref (node-match subnode query 'symbol-ref))
             (lambda-form (node-match subnode query 'eval-form))
             (t (tree-match subnode query))))
          (&body (fuzz-match-body query subnode 'eval-form))
          (unevaluated (tree-match subnode query))
          ((string-literal integer-literal) (node-match subnode query 'literal))
          (comment t)
          (t (cerror "continue" "spec: ~a" spec))))))

(defun syntax-match (node query sort)
  (cond
    ((and (or (eq sort 'unevaluated) (listp sort))) (spec-match node query 'unevaluated))
    ((typep node 'macro-call) (spec-match (body node) query 'unevaluated))
    ((form-p node) (spec-match (to-syntax node) query 'unevaluated))))

(defun node-match (node query sort)
  "An exact match between evaluated nodes."
  (declare (ignore sort))
  (unless (or (eq (type-of node) (type-of query))
              ;; integer hole?
              (typep query 'hole))
    (return-from node-match))
  ;; special case needed: macros and backquote should be traversed once
  (typecase query
    (null (null node))
    (hole t)
    (symbol-ref (symbol-ref= node query))
    (ratio-literal
     (and (string= (str (numerator-node node)) (str (numerator-node query)))
          (string= (str (denominator-node node)) (str (denominator-node query)))))
    (literal (string= (str node) (str query)))
    (ref-list (and (null (elements node)) (null (elements query))))
    (macro-call
     (and (symbol-ref= (op node) (op query))
          (spec-match (body node) (body query) 'unevaluated)))
    (t ; recursive cases
      (let ((success t))
        (map-node query
                  (lambda (subquery id spec)
                    (and-f success
                      (when (or (listp id) (slot-boundp query id))
                        (and (or (slot-boundp node id) (typep subquery 'hole))
                             (spec-match (get-location node id) subquery spec))))))
        success))))

;; specs strictly are of form spine+optional, requiring no parsing unlike slots
;; (e.g. function-code docstring, declarations, qualifiers)
(defun spec-traverse (node subnode spec id f)
  "Traverses `subnode' of `node'. `f' takes (subnode sort id) and returns true to stop."
  (labels ((child (i)
             (append-id id i))
           (tree-rec (form id)
             (assert (syntax-p form))
             (unless (funcall f form 'unevaluated id)
               (unless (gen-atom-p form)
                 (loop for i from 0
                       for e in (elements form)
                       do (spec-traverse node e 'unevaluated (child i) f))))))
    (cond
      ((symbolp spec)
       (case spec
         (fun-designator
          (typecase subnode
            ((or symbol-ref hole) (funcall f subnode 'fun-designator id))
            ((or lambda-form named-lambda-form) (funcall f subnode 'eval-form id))
            (t
             (assert (syntax-p subnode))
             (tree-rec subnode id))))
         (&body
          (loop for e in subnode
                for i from 0
                do (assert (form-p e))
                   (funcall f e 'eval-form (child i))))
         (unevaluated (tree-rec subnode id))
         ;; necessary for macro calls
         (t
          (if-let (parsed (and (typep node 'macro-call) (gethash subnode (subforms node))))
            (funcall f parsed spec id)
            (funcall f subnode spec id)))))
      ((eq (first spec) '&or)
       (progn
         (assert (= (length spec) 3))
         (if (pattern-matches-p (second spec) subnode)
             (spec-traverse node subnode (second spec) id f)
             (spec-traverse node subnode (third spec) id f))))
      (t
       (assert (listp spec))
       (unless (funcall f subnode spec id)
         (loop for spec-rest on spec
               for subspec = (car spec-rest)
               for elts on (elements subnode)
               for e = (car elts)
               for i from 0
               do (case subspec
                    (&rest
                     (assert (= (length spec-rest) 2))
                     (loop for e in elts
                           do (spec-traverse node e (second spec-rest) (child i) f)
                              (incf i))
                     (return))
                    (&optional
                     (spec-traverse node e (second spec-rest) (child i) f)
                     (pop spec-rest))
                    ((&rest-qualifiers &declarations &or)
                     (assert nil nil "unimplemented keywords"))
                    (t (spec-traverse node e subspec (child i) f)))))))))

(defun node-search (node query stack head-matcher collector)
  "Traverses `node' calling `collector' on matches when `head-matcher' returns true."
  ;; TODO could add a match-fail return value for pruning
  (labels ((rec (node query sort stack)
             (if-let (result (funcall head-matcher node query sort))
               (progn
                 (funcall collector stack)
                 t)
               (unless (or (is-atom node) (gen-list-p node))
                 (map-node node
                           (lambda (subnode id spec)
                             (spec-traverse node
                              subnode spec id
                              (lambda (subform sort id)
                                (rec subform query sort (cons id stack))))))))))
    (rec node query 'eval-form stack)))

;;; replacement
(define-condition rename-error (simple-error)
  ((reason :initarg :reason :initform (error "must provide reason") :reader
    rename-error-reason)
   (node :initarg :node :initform (error "must provide node") :reader rename-error-node))
  (:report (lambda (condition stream)
             (format stream "rename refused: ~a"
                     (case (rename-error-reason condition)
                       (:special
                        "specials cannot be lexically renamed, may break external code")
                       (:binder "not a binder")
                       (:conflict "conflicts with another parallel binding")
                       (:escape "renaming to a special captured by a closure")
                       (:capture "possible capture")
                       (t (rename-error-reason condition))))))
  (:documentation ""))

(defun rebuild-spine (loc updater stack)
  "Requires that (location-node `loc') = (location-node (car `stack'))
Functionally rebuilds the path from `loc' out to the root, applying `updater' to
the value at `loc', but retaining the current focus. Returns the new stack and root."
  (assert (eq (location-node loc) (location-node (car stack))))
  (labels ((rebuild (stack newnode newstack)
             (if (null stack)
                 ;; Macro reanalysis may have copied descendant nodes. Resolve the saved
                 ;; path ids through the new tree
                 (values (path-to-stack (mapcar #'location-id newstack) newnode) newnode)
                 (let* ((this (car stack))
                        (id (location-id this))
                        (new-parent (update (location-node this) id newnode)))
                   (rebuild (cdr stack) new-parent
                            (cons (make-location :node new-parent :id id) newstack))))))
    (rebuild (cons loc (cdr stack)) (funcall updater (node-at loc)) (list))))

;; watch out for semantic bug: confusing stacks and paths
(defun rename-binder (form old-path new-name)
  "assumes old-path is nonempty"
  (flet ((refuse (reason node)
           (error 'rename-error :reason reason :node node)))
    (let (stack
          env
          binder
          new)
      ;; env stack-node
      (multiple-value-bind (parent-stack parent-env)
          (resolve-path form (butlast old-path) +nullenv+)
        (setf stack
              (cons (make-location
                     :node (if parent-stack (node-at (car parent-stack)) form)
                     :id (lastcar old-path))
                    parent-stack)
              env parent-env
              binder (node-at (car stack))
              new
              (copy-anchors binder
               (make-instance 'symbol-ref
                :home-package (reresolve-package new-name binder)
                :name new-name))))
      ;; env is right outside the binding form at stack[0].node
      ;; stack[0].node is the binding form containing binder
      (unless (and (typep binder 'symbol-ref) (eq 'binder (locsort (car stack))))
        (refuse :binder binder))
      ;; conservatively record new name references even if the parent can bind twice,
      ;; references under binders shadowed by the one being replaced will never change
      (let* ((binding-form (location-node (car stack)))
             (has-decls (member 'decls (location-slots binding-form)))
             (kind (typecase binding-form
                     ((or flet-form labels-form macrolet-form) :function)
                     ((or let-form let*-form symbol-macrolet-form) :variable)
                     ((or block-form defun-form defmethod-form) :block)
                     (macro-call (gethash binder (binders binding-form)))
                     (function-code :variable)
                     (t (error "unknown binding position from ~a" binding-form))))
             (matches (list))
             (old-paths (list))
             (new-paths (list))
             ;; a parallel list to new-paths, binders referred to
             (newname-binders (list)))
        ;; cannot rename dynamic variables to lexical
        (when (or (env-special-p binder env)
                  (when has-decls
                    (member binder (declaration-specials (decls binding-form))
                            :test #'symbol-like=)))
          (refuse :special binder))
        ;; check: binding form for parallel binding :conflict
        (when-let* ((existing (and has-decls
                                   (env-lookup new
                                    kind (location-env binding-form 'decls +nullenv+)))))
          (refuse :conflict existing))
        ;; search
        (node-search binding-form
         new (butlast old-path)
         (lambda (node query sort)
           (declare (ignore query))
           (and (typep node 'symbol-ref)
                (cond
                  ((eq kind :variable) (member sort '(eval-form variable-declarator)))
                  ((eq kind :function) (eq sort 'fun-designator))
                  ((eq kind :block) (eq sort 'symbol-ref)))
                (or (symbol-ref= node binder) (symbol-ref= node new))))
         (lambda (stack)
           (push stack matches)))
        ;; filter the new name matches for those that refer to this binder
        ;; TODO build trie instead
        (dolist (match matches)
          (let ((match-path (reverse match)))
            (multiple-value-bind (stack env)
                (resolve-path form match-path +nullenv+)
              (let ((node (node-at (car stack))))
                (if (string= (name node) new-name)
                    (progn
                      (push match-path new-paths)
                      (push (env-lookup node kind env) newname-binders))
                    (when (eq binder (env-lookup node kind env))
                      (push match-path old-paths)))))))
        ;; (disp old-paths)
        ;; (disp new-paths)
        (let* ((oldstack stack)
               (newstack stack))
          (dolist (match old-paths)
            (let ((stack (path-to-stack match (root newstack))))
              (when (and (and has-decls
                              (and (env-special-p new
                                    (location-env binding-form 'decls env))
                                   (not (eq :special
                                            (sb-cltl2:variable-information
                                             (ref-coerce-symbol new))))))
                         (find-if (lambda (n)
                                    (typep n 'function-code))
                                  stack
                                  :key #'location-node))
                (refuse :escape (relookup match oldstack)))
              ;; XXX also need to rebase match stacks
              (setf newstack
                    (rebuild-spine (car stack)
                     (lambda (s)
                       (copy-anchors s
                        (make-instance 'symbol-ref
                         :home-package (reresolve-package new-name s)
                         :name new-name)))
                     stack))))
          ;; replace binder
          (let ((stack (path-to-stack old-path (root newstack))))
            (setf newstack (rebuild-spine (car stack) (constantly new) stack)))
          ;; reresolve binder in case of macro call
          (multiple-value-bind (stack env)
              (resolve-path (root newstack) old-path +nullenv+)
            (declare (ignore env))
            (setf new (node-at (car stack))))
          ;; capture: every old reference to the binder still resolves to the same binder
          (dolist (match old-paths)
            (multiple-value-bind (stack env)
                (resolve-path (root newstack) match +nullenv+)
              (unless (or (env-special-p new env)
                          (eq new (env-lookup (node-at (car stack)) kind env)))
                (refuse :capture (relookup match oldstack)))))
          ;; capture: every old reference with the new name still refers to the same binder
          (loop for match in new-paths
                for newname-binder in newname-binders
                do (multiple-value-bind (stack env)
                       (resolve-path (root newstack) match +nullenv+)
                     (unless (eq newname-binder
                                 (env-lookup (node-at (car stack)) kind env))
                       (refuse :capture (relookup match oldstack)))))
          (root newstack))))))

(defparameter *sort-repr*
  '((eval-form . parsed) (fun-designator . parsed) (unevaluated . syntax)
    (function-code . function-code) (binder . atom) (symbol-ref . atom)
    (string-literal . atom) (integer-literal . atom) (comment . comment) (char . char)))

(defparameter *atom-sorts*
  '(eval-form fun-designator unevaluated binder symbol-ref string-literal integer-literal))

(defun keep-pasted-node? (node from to)
  "Do we retain a parsed node in its pasted position? or take its reparsed form"
  (flet ((representation (sort)
           (cdr (assoc sort *sort-repr*))))
    (cond
      ((equal from to) t)
      ((and (consp from) (consp to))
       (and (= (length from) (length to))
            (every #'keep-pasted-node? (elements node) from to)))
      ((is-atom node) (and (member from *atom-sorts*) (member to *atom-sorts*) t))
      (t (eq (representation from) (representation to))))))
