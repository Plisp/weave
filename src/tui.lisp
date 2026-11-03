;;;;
;;;; terminal frontend
;;;;
;;
;; note: if a function takes a single rect argument, then any geometry it returns is in the
;; same space as the supplied bounds unless specified.
;; Importantly, the render-* functions produce rendered views with rects relative to
;; their parent buffer's top left corner.
;; note: debugger code must NOT error, catch any printing errors

(defpackage #:weave-tui
  (:use :cl #:alexandria-2 #:weave-utils)
  (:import-from #:weave-parser
                #:location #:make-location #:location-node #:location-id #:location=
                #:locsort #:node-at #:update
                #:append-id #:parent-id #:bodylike-id #:id-index
                #:ref-list #:elements #:gen-list-p #:gen-tree-ref
                #:hole
                #:text)
  (:local-nicknames (#:parse #:weave-parser)
                    (#:tui #:uncursed)
                    (#:tui-sys #:uncursed-sys))
  (:export #:main))
(in-package #:weave-tui)

;; dynamic vars for debugging, should not be used
(defvar *state* #.nil)
(defvar *log* (sb-concurrency:make-mailbox :name "log"))
(defvar *log-stop* (gensym))
(defmacro slog (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons ',form ,res))
       ,res)))
(defmacro slog* (form)
  (once-only ((res form))
    `(progn
       (sb-concurrency:send-message *log* (cons nil ,res))
       ,res)))

(defun reading-order-p (views)
  "Does each of `views' start no earlier than the one before it, by line then column?"
  (loop for (a b) on views
        while b
        always (let ((ra (tui:rect a))
                     (rb (tui:rect b)))
                 (or (< (tui:rect-y ra) (tui:rect-y rb))
                     (and (= (tui:rect-y ra) (tui:rect-y rb))
                          (<= (tui:rect-x ra) (tui:rect-x rb)))))))

(defrecord ordered-view (tui:view)
  ()
  (:documentation "A mixin view whose children must be in reading order. Checked at
construction, do not modify."))

(defmethod initialize-instance :after ((view ordered-view) &key)
  (assert (reading-order-p (tui:children view)) ()
          "Children of ~a are not in reading order: ~a" view (tui:children view)))

(defrecord ast-view (ordered-view)
  ((location :mutable t :type location)
   (view-stack :init nil :mutable t :type list)
   (hoverable :init nil :mutable t :type boolean)))

(defrecord completion-state ()
  ((anchor)
   (candidates :type list)
   (selection :init 0 :type (integer 0))))

(defun copy-completion-state (state &key (anchor (anchor state))
                                      (candidates (candidates state))
                                      (selection (selection state)))
  (make-completion-state :anchor anchor :candidates candidates :selection selection))

(defrecord selection ()
  ((buffer-id)
   (context :type list)
   (location :type location)
   (point :type integer)
   (activep :init t :type boolean))
  (:representation :struct))

(defrecord zipper (selection)
  ((stack :type list))
  (:representation :struct))

(defun copy-selection (selection &key (point (selection-point selection))
                                   (activep (selection-activep selection))
                                   (stack (when (typep selection 'zipper)
                                            (zipper-stack selection))))
  "Returns a selection with updated fields. A nonempty `stack' makes it into a zipper."
  (apply (if stack #'make-zipper #'make-selection)
         :buffer-id (selection-buffer-id selection)
         :context (selection-context selection)
         :location (selection-location selection)
         :point point :activep activep
         (when stack (list :stack stack))))

(defrecord undo-state ()
  ((ast)
   (stack :type list))
  (:representation :struct))

(defun selection-anchor (selection)
  (id-index (location-id (selection-location selection))))

(defun selection-range (selection)
  (values (min (selection-anchor selection) (selection-point selection))
          (max (selection-anchor selection) (selection-point selection))))

(defun selection-parent-id (selection)
  (parent-id (location-id (selection-location selection))))

(defun zipper-top-p (buffer stack)
  (when-let (selection (selection buffer))
    (and (typep selection 'zipper)
         (eq (node-at (car stack)) ; top of zipper
             (location-node (lastcar (zipper-stack selection)))))))

(defun location-selected-p (buffer stack)
  (when-let* ((selection (selection buffer))
              (id (location-id (car stack))))
    (and (eq (location-node (selection-location selection))
             (location-node (car stack)))
         (bodylike-id id)
         (equal (selection-parent-id selection) (parent-id id))
         (multiple-value-bind (low high)
             (selection-range selection)
           (<= low (id-index id) high)))))

(defrecord cutbuffer ()
  ((forms :type list)
   (sorts :type list))
  (:representation :struct))

;; computed during redisplay from current toplevel
(defrecord segment ()
  ;; toplevel ast index
  ((index :type (integer 0))
   ;; starting y-offset relative to staging coordinates
   (start :type (integer 0))
   (rows :type (integer 1))
   ;; local buffer containing only this segment for blitting
   (buffer :type (array tui::cell (* *)))
   ;; has staging coordinates
   (view :type tui:view))
  (:representation :struct))

;; top of screen view anchor
(defrecord scroll-position ()
  ;; toplevel ast index
  ((index :type (integer 0))
   ;; row offset from start of rendered toplevel form
   (row :init 0 :type (integer 0)))
  (:representation :struct))

(defrecord scroll-state ()
  ((position :type scroll-position)
   ;; total rows staged
   (rows :type (integer 0))
   ;; viewport: an offset relative to staging coordinates
   (viewport :type (integer 0))
   (segments :type list))
  (:representation :struct))

(defrecord pane-drag ()
  ((buffer-id) (offset-x :type integer) (offset-y :type integer))
  (:representation :struct))

(defrecord ui (tui:elemental)
  ((pane-drag :init nil :mutable t :type (or null pane-drag))
   (buffers :init nil :mutable t :type list)
   (focused-buffer :init nil :mutable t)
   (selection :init nil :mutable t :type (or null selection))
   (zipper :init nil :mutable t :type (or null zipper))
   (cutbuffer :init nil :mutable t :type (or null cutbuffer))
   (redisplayed-since-input :init nil :mutable t :type boolean)
   (revision :init 0 :mutable t :type (integer 0)))
  (:representation :class))

;; MUST BE shallow copyable, always update immutably
(defrecord buffer-state ()
  ((floating :init nil :mutable t :type boolean)
   (buffer-package :init *package* :mutable t :type package)
   (buffer-source :init nil :mutable t :type (or null pathname))
   ;; screen coordinates
   (buffer-x :init 0 :mutable t :type (integer 0))
   (buffer-y :init 0 :mutable t :type (integer 0))
   (ast :mutable t)
   (edit-loc :init nil :mutable t :type (or null location))
   (goal-col :init 1 :mutable t :type positive-fixnum)
   (completion-state :init nil :mutable t :type (or null completion-state))
   (history :init nil :mutable t :type list)
   (future :init nil :mutable t :type list)
   (stack :init nil :mutable t :type list)
   (goal-stacks :init nil :mutable t :type list))
  (:representation :class))

(defun copy-buffer-state (state)
  (shallow-copy-record state))

;; mutable per-render cache, needed for movement commands
(defrecord render-state ()
  ;; rect has absolute coordinates
  ((floating-rect :init nil :mutable t :type (or null tui:rect))
   ;; has staging coordinates
   (buffer-view :init nil :mutable t :type (or null tui:view))
   (stage-cache :init (make-hash-table :test #'eq) :type hash-table)
   (redisplay-cache :init (make-hash-table :test #'equal) :type hash-table)
   (scroll-state :init (make-scroll-state :position (make-scroll-position :index 0)
                                          :rows 0 :viewport 0 :segments nil)
                 :mutable t :type scroll-state)
   ;; both below use staging coordinates
   (focus-rect :init nil :mutable t :type (or null tui:rect))
   (node-views :init (make-hash-table) :type hash-table))
  (:representation :class))

(defrecord buffer ()
  ((buffer-ui)
   (buffer-id :init (gensym "BUFFER-"))
   (buffer-state :mutable t :type buffer-state)
   (render-state :init (make-render-state) :mutable t :type render-state))
  (:representation :class))

(defrecord debug-state ()
  ;; a stack of conditions on which the debugger has been invoked
  ((debug-conditions :init () :mutable t :type list)
   ;; a parallel list to debug-conditions, whose elements are lists of restarts
   ;; captured by compute-restarts at time of handling the error, innermost first
   (debug-restarts :init () :mutable t :type list)
   (buffer-x :init 0 :mutable t :type (integer 0))
   (buffer-y :init 0 :mutable t :type (integer 0)))
  (:representation :class))

(defmethod floating ((state debug-state)) t)

(macrolet ((buffer-state-accessors (state readers &rest accessors)
             `(progn
                ,@(loop for name in (append readers accessors)
                        collect `(defmethod ,name ((buffer buffer))
                                   (,name (,state buffer))))
                ,@(loop for name in accessors
                        collect `(defmethod (setf ,name) (value (buffer buffer))
                                   (setf (,name (,state buffer)) value))))))

  (buffer-state-accessors
   buffer-state ()
   floating buffer-package buffer-source buffer-x buffer-y ast edit-loc goal-col
   completion-state debug-conditions debug-restarts
   history future stack goal-stacks)
  (buffer-state-accessors
   render-state (stage-cache redisplay-cache node-views)
   floating-rect buffer-view scroll-state focus-rect))

(defun buffer-focused-p (buffer)
  (when-let (focused (focused-buffer (buffer-ui buffer)))
    (eq (buffer-id buffer) (buffer-id focused))))

(defmethod (setf focused-buffer) :before (buffer (ui ui))
  (assert (or (null buffer) (member buffer (buffers ui))))
  (setf (redisplayed-since-input ui) nil)
  (when (and (not (eq buffer (focused-buffer ui))) (selection ui))
    (setf (selection ui) (copy-selection (selection ui) :activep nil))))

(defmethod selection ((buffer buffer))
  (let ((selection (selection (buffer-ui buffer))))
    (when (and selection
               (eq (buffer-id buffer) (selection-buffer-id selection)))
      selection)))

(defun focus (buffer)
  (car (stack buffer)))

;;; rendering helpers
(defgeneric render-node (node stack context rect &key &allow-other-keys)
  (:documentation "Drawn contents MUST fit within the provided `rect'."))

(defmacro with-redisplay-cache ((buffer key) &body body)
  "The value of `body', computed once per redisplay of `buffer' for `key'."
  (once-only (buffer key)
    (with-gensyms (value found)
      `(multiple-value-bind (,value ,found) (gethash ,key (redisplay-cache ,buffer))
         (if ,found
             ,value
             (setf (gethash ,key (redisplay-cache ,buffer)) (progn ,@body)))))))

(defvar *render-environments*)

(defun environment-table (node)
  (or (gethash node *render-environments*)
      (setf (gethash node *render-environments*) (make-hash-table :test #'equal))))

(defun precompute-form-environments (location)
  "Visits a staged toplevel form once, associating its locations with lexical environments."
  (when (not (nth-value 1 (gethash (location-id location)
                                   (environment-table (location-node location)))))
    (labels ((visit (loc outer)
               (let* ((node (location-node loc))
                      (id (location-id loc))
                      (table (environment-table node))
                      ;; body elements of ordinary forms share a scope
                      (scope-id (if (and (consp id) (eq (car id) 'parse:body)
                                         (not (typep node 'parse:macro-call)))
                                    '(parse:body 0) id))
                      (env (if (member (locsort loc) '(parse:eval-form parse:function-code))
                               (or (gethash scope-id table)
                                   (setf (gethash scope-id table)
                                         (parse:location-env node id outer)))
                               outer))
                      (value (node-at loc)))
                 (setf (gethash id table) env)
                 (cond
                   ((gen-list-p value)
                    (loop for i below (length (elements value))
                          do (visit (make-location :node node :id (append-id id i)) env)))
                   ((typep value 'parse:function-code)
                    (dolist (slot '(parse:lambda-list parse:body))
                      (visit (make-location :node value :id slot) env)))
                   (t
                    (dolist (slot (remove-duplicates
                                   (append (mapcar #'car (parse:form-slot-kinds
                                                          (type-of value)))
                                           (typecase value
                                             (parse:function-call '(parse:name))
                                             (parse:macro-call '(parse:op))))))
                      (when (slot-boundp value slot)
                        (visit (make-location :node value :id slot) env))))))))
      (visit location (parse:make-env)))))

(defun uncached-environment-at (stack)
  (let ((env (parse:make-env)))
    (dolist (loc (reverse stack))
      (when (member (locsort loc) '(parse:eval-form parse:function-code))
        (setf env (parse:location-env (location-node loc) (location-id loc) env))))
    env))

(defun environment-at (stack)
  (declare (type list stack))
  (let ((loc (car stack)))
    (or (gethash (location-id loc) (environment-table (location-node loc)))
        (uncached-environment-at stack))))

(defun compute-focus-binder (node stack buffer)
  (declare (ignore buffer))
  (let ((loc (car stack)))
    (when (parse:lexical-symbol-ref node loc)
      (let ((kind (cond ((parse:function-position-p loc) :function)
                        ((parse:block-position-p loc) :block)
                        (t :variable)))
            (env (environment-at stack)))
        (multiple-value-bind (entry kind) (parse:env-lookup node kind env)
          (let ((binder (ensure-car entry)))
            (when (and (eq kind :lexical) (typep binder 'parse:binder)) binder)))))))

(defun focus-binder (buffer)
  "The binder referenced by the symbol-ref under the cursor."
  (with-redisplay-cache (buffer :focus-binder)
    (compute-focus-binder (node-at (focus buffer)) (stack buffer) buffer)))

(defun symbol-ref-boundp (node stack buffer)
  (declare (ignore buffer))
  (or (eq :lexical
          (nth-value 1 (parse:env-lookup
                        node
                        (cond ((parse:function-position-p (car stack)) :function)
                              ((parse:block-position-p (car stack)) :block)
                              (t :variable))
                        (environment-at stack))))
      ;; blocks are never global
      (and (not (parse:block-position-p (car stack)))
           (multiple-value-bind (symbol found)
               (parse:resolve node)
             (and found
                  (if (parse:function-position-p (car stack))
                      (fboundp symbol)
                      (boundp symbol)))))))

(defmethod render-node :around (node stack context rect &key)
  (let* ((vals (multiple-value-list (call-next-method)))
         (view (first vals)))
    (setf (gethash node (node-views context)) view)
    ;; occasionally draws body lists
    (when (typep view 'ast-view)
      (setf (view-stack view) stack))
    (when (location= (car stack) (focus context))
      (setf (gethash :focus-view (redisplay-cache context)) view))
    (values-list vals)))

(defparameter *default-key-handlers* (make-hash-table :test #'equal))
(defparameter *ui-key-handlers* (make-hash-table :test #'equal))

(defparameter *global-key-handlers* (make-hash-table :test #'equal))
(defun plain-body-location-p (location)
  "Whether `location' addresses a whole evaluated body, which is an invariant violation
since these aren't a proper location."
  (let ((id (location-id location)))
    (and (symbolp id)
         (eq '&body (cdr (assoc id (parse:form-slot-kinds
                                    (type-of (location-node location)))))))))

(defun global-key-handler (node location)
  "Handler receives its working buffer explicitly at dispatch time."
  (lambda (view event buffer)
    (slog event)
    (assert (location= location (focus buffer)))
    ;; (slog (butlast (stack buffer))) ; suppress toplevel list
    (or (when-let (handler (gethash event *global-key-handlers*))
          (funcall handler view buffer))
        ;; propagate
        (loop for thisnode = node then (location-node location)
              for location in (stack buffer)
              thereis (when (handle-key thisnode view location buffer event)
                        (slog* `(handled-at ,node))
                        t))
        ;; contextual handlers
        (when-let (handler (gethash event *default-key-handlers*))
          (slog* "default handler called")
          (funcall handler view buffer)))
    ;; state updates here
    (when-let (completion (completion-state buffer))
      (setf (completion-state buffer)
            (when (eq (node-at (focus buffer)) (anchor completion))
              (let* ((name (parse:name (anchor completion)))
                     (valid (delete-if-not (lambda (s) (starts-with-subseq name (string s)))
                                           (copy-list (candidates completion)))))
                (copy-completion-state
                 completion
                 :candidates (if (find name valid :test #'string=)
                                 (sort valid #'<
                                       :key (lambda (s)
                                              (mk-string-metrics:damerau-levenshtein
                                               name (string s))))
                                 valid))))))
    (assert (not (plain-body-location-p (focus buffer))) ()
            "focused the body ~a of ~a" (location-id (focus buffer))
            (type-of (location-node (focus buffer))))))

(defun fname (node)
  (parse:name (parse:name node)))

;; note: assumes length 1 symbol mapping for <= and >=
(defparameter *arb-arity-binops* #(/ * + - < > <= >= = /=))
(defun binop-precedence (op-ref)
  (trivia:ematch (parse::ref-coerce-symbol op-ref)
    ((or *) 3)
    ((or / + -) 2)
    ((or < > <= >= = /=) 1)))

(defun is-binop-call (node)
  (and (typep node 'parse:function-call)
       (typep (parse:name node) 'parse:symbol-ref)
       (find (parse:resolve (parse:name node)) *arb-arity-binops*)))

;;; hole
;; note: holes only replace symbols or evaluation contexts

(defmethod render-node ((node hole) stack context rect &key)
  (with-accessors ((text text)) node
    (let* ((location (car stack))
           (text (if (string= text "") "hole" text))
           (focused (location= location (focus context))))
      (tui:puts* text 1 1 rect (if (and focused (buffer-focused-p context))
                                   (tui:make-style :fg #x0 :underlinep t)
                                   (tui:make-style :fg (tui:color 30 200 0) :underlinep t)))
      (make-instance 'ast-view
                     :rect (tui:clamp-rect
                            (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                            rect)
                     :location location
                     :hoverable t
                     ;; leaf handler never called unless focused
                     :key-handler (when focused
                                    (global-key-handler node location))
                     :focused focused))))

(defun is-regular-char-event (event)
  (and (characterp (tui:event-kind event))
       (not (or (tui:event-controlp event)
                (tui:event-altp event) (tui:event-metap event)))))

(defun completion-candidates (context)
  (declare (ignore context))
  (loop for s being the symbols of (find-package "CL") collect s))

(defparameter *symbol-mappings* '((<= . #\≤) (>= . #\≥) (/= . #\≠) (lambda . "λ")))

(defparameter *reader-punctuation* '((parse:read-quote . "'")
                                     (parse:read-function . "#'")
                                     (parse:read-quasiquote . "`")
                                     (unquote . ",") (unquote-splicing . ",@")
                                     (parse:read-eval . "#.")))

(defun to-read-syntax (name)
  (case name
    (cl:quote 'parse:read-quote)
    (cl:function 'parse:read-function)
    (eclector.reader:quasiquote 'parse:read-quasiquote)
    (eclector.reader:unquote 'unquote)
    (eclector.reader:unquote-splicing 'unquote-splicing)
    (t name)))

(defun symbol-ref-text (node package &optional use-mappings)
  (let ((name (parse:name node))
        (home (parse:home-package node)))
    (cond
      ((typep node 'parse:reader-marker)
       (or (assoc-value *reader-punctuation* (to-read-syntax (parse:resolve node)))
           (string name)))
      ((eq home (find-package :keyword)) (format nil ":~(~a~)" name))
      ((and use-mappings
            (assoc-value *symbol-mappings* node :test #'parse:string-and-package=))
       (string (assoc-value *symbol-mappings* node :test #'parse:string-and-package=)))
      ((null home) (format nil "#:~(~a~)" name))
      ((or (eq home package)
           (multiple-value-bind (symbol found) (find-symbol name package)
             (and found
                  (multiple-value-bind (original present) (find-symbol name home)
                    (and present (eq symbol original))))))
       (string-downcase name))
      (t
       (let ((prefix (or (car (rassoc home (slynk-backend:package-local-nicknames package)))
                         (first (sort (cons (package-name home)
                                            (copy-list (package-nicknames home)))
                                      #'< :key #'length)))))
         (format nil "~(~a~a~a~)" prefix
                 (if (external-symbol-p name home) ":" "::") name))))))

(defmethod render-node ((node parse:symbol-ref) stack context rect &key)
  (let* ((location (car stack))
         (str (symbol-ref-text node (buffer-package context) t))
         (focused (location= location (focus context))))
    (tui:puts* str 1 1 rect)
    (make-instance 'ast-view
                   :rect (tui:clamp-rect
                          (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                          rect)
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

(defmethod render-node ((node parse:binder) stack context rect &key)
  (let* ((location (car stack))
         (str (symbol-ref-text node (buffer-package context)))
         (focused (location= location (focus context))))
    (tui:puts* str 1 1 rect (tui:make-style :italicp t))
    (make-instance 'ast-view
                   :rect (tui:clamp-rect
                          (tui:copy-rect rect :rows 1 :cols (tui:display-width str))
                          rect)
                   :location location
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

(defun render-symbol-space (op loc stack context rect)
  "Renders `loc' as a bare focusable token for `op', a string designator,
with a space after it unless specified."
  (let* ((text (if-let (exp (assoc-value *reader-punctuation* op))
                 (string exp)
                 (if-let (exp (assoc-value *symbol-mappings* op))
                   (format nil "~(~a~) " exp)
                   (format nil "~(~a~) " op))))
         (focused (location= loc (focus context)))
         (view
           (make-instance 'ast-view
                          :rect (tui:clamp-rect
                                 (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                                 rect)
                          :location loc
                          :hoverable t
                          :key-handler (when focused (global-key-handler op loc))
                          :focused focused)))
    (tui:puts* text 1 1 rect)
    (setf (view-stack view) (cons loc stack))
    (when focused (setf (gethash :focus-view (redisplay-cache context)) view))
    view))

;;; syntactic list
(defparameter *list-delimiters* '("(" . ")"))

(defmethod parse:get-location ((node parse:ref-list) (id (eql :open)))
  node)

(defmethod parse:location-sort ((node parse:ref-list) (id (eql :open)))
  'parse:unevaluated)

(defun handle-focus-p (buffer list-loc)
  "Is the focus the opening parenthesis handle of the list at `list-loc'?"
  (let ((stack (stack buffer)))
    (and (eq (location-id (first stack)) :open)
         (location= (second stack) list-loc))))

(defun render-delimiter (text loc stack rect &key hoverable)
  (let ((view
          (make-instance 'ast-view
                         :rect (tui:clamp-rect
                                (tui:copy-rect rect :rows 1 :cols (tui:display-width text))
                                rect)
                         :location loc
                         :hoverable hoverable)))
    (tui:puts* text 1 1 rect)
    (setf (view-stack view) stack)
    view))

(defun list-locator (location)
  "Locates the elements of the list at `location' by index."
  (lambda (index)
    (make-location :node (location-node location)
                   :id (append-id (location-id location) index))))

(defun render-elements (elements spec locate stack context rect keys
                        &optional (render #'render-node))
  "Splits `elements' into rows of (indent . n) following `spec', a list of
(indent . count) pairs where count is a number of elements or nil for all remaining
elements on a single line. A zero count produces a blank row. The last pair repeats,
e.g. setq is (2 . 2), and t is a synonym for 1. `spec' may instead be a function from
`elements' to specs. `render' renders each element with the render-node calling
convention."
  (let ((pairs (if (listp spec) spec (funcall spec elements)))
        (remaining elements)
        (transform (or (getf keys :transform) #'identity))
        (index 0))
    (tui:with-vertical (rect)
      (loop while (and remaining (not (tui:full)))
            do (destructuring-bind (indent . n) (car pairs)
                 (setf pairs (or (cdr pairs) pairs))
                 (if (eql n 0)
                     (tui:pad 1)
                     (tui:place (rect)
                       (tui:with-horizontal (rect)
                         (when (plusp indent)
                           (tui:pad indent))
                         (loop
                           with count = (case n
                                          ((t) 1)
                                          ((nil) (length remaining))
                                          (otherwise n))
                           for i from 0 below count
                           with last-elt = nil
                           while remaining
                           do (let ((element (funcall transform (pop remaining)))
                                    (loc (funcall locate index)))
                                (incf index)
                                (unless (or (zerop i)
                                            (and (= index 2)
                                                 (parse:unary-reader-marker-p last-elt)))
                                  (tui:pad 1))
                                (tui:place (rect)
                                  (apply render element (cons loc stack) context rect keys))
                                (setf last-elt element)))))))))))

(defun view-end (view)
  "For placing parens, relative to staging coordinates."
  (if-let (children (tui:children view))
    (view-end (lastcar children))
    (let ((r (tui:rect view)))
      (values (tui:rect-x2 r) (tui:rect-y r)))))

(defparameter *column-limit* 92)

;; note: must be adjustable, bound to *put-buffer* during measurement
(defvar *measurement-scratch*)

(defun ensure-measurement-scratch (rect)
  "Grows the shared scratch array at least to the size of `rect'."
  (let* ((cells *measurement-scratch*)
         (old-rows (array-dimension cells 0))
         (old-cols (array-dimension cells 1))
         (rows (max old-rows (tui:rect-y2 rect)))
         (cols (max old-cols (tui:rect-x2 rect))))
    (unless (and (= rows old-rows) (= cols old-cols))
      (adjust-array cells (list rows cols))
      (loop for y below rows
            do (loop for x from (if (< y old-rows) old-cols 0) below cols
                     do (setf (aref cells y x) (tui::make-cell)))))
    cells))

(defun measure-node (node stack context rect &rest keys)
  "Returns the rendered rectangle in the same coordinate space as `rect'.
Uses shared scratch cells and clears the touched scratch area on exit."
  (let* ((tui::*put-buffer* (ensure-measurement-scratch rect))
         (touched rect))
    (unwind-protect
         (setf touched (tui:rect (apply #'render-node node stack context rect keys)))
      (loop for y from (tui:rect-y touched) below (tui:rect-y2 touched)
            do (loop for x from (tui:rect-x touched) below (tui:rect-x2 touched)
                     for cell = (aref tui::*put-buffer* y x)
                     do (setf (tui::cell-string cell) " "
                              (tui::cell-style cell) tui::*default-style*))))))

(defun ref-list-indentation (elements locate stack context rect keys)
  "Measures children privately, stopping at the first multiline child."
  (let* ((max-width (max 1 (min (tui:rect-cols rect)
                                (- *column-limit* (tui:rect-x rect)))))
         (measuring (make-buffer :buffer-ui (buffer-ui context)
                                 :buffer-id (buffer-id context)
                                 :buffer-state (buffer-state context)))
         (bounds (tui:copy-rect rect :y 0 :rows 2 :cols max-width))
         (transform (or (getf keys :transform) #'identity))
         (rows nil)
         (used 0)
         (count 0)
         (previous nil))
    (when (and (= (length elements) 2)
               (let ((head (apply #'measure-node (funcall transform (car elements))
                                  (cons (funcall locate 0) stack)
                                  measuring bounds keys)))
                 (< (tui:rect-cols head) 10)))
      (return-from ref-list-indentation '((0 . nil))))
    (loop for original in elements
          for index from 0
          for element = (funcall transform original)
          for measured = (apply #'measure-node element (cons (funcall locate index) stack)
                            measuring bounds keys)
          for size = (tui:rect-cols measured)
          for gap = (if (or (zerop count)
                            (and (= index 1) (parse:unary-reader-marker-p previous)))
                        0 1)
          do (when (> (tui:rect-rows measured) 1)
               (return-from ref-list-indentation '((0 . 1))))
             ;; line wrap
             (when (and (plusp count) (> (+ used gap size) max-width))
               (push (cons 0 count) rows)
               (setf count 0
                     used 0
                     gap 0))
             (incf count)
             (incf used (+ gap size))
             (setf previous element))
    (when (plusp count) (push (cons 0 count) rows))
    (or (nreverse rows) '((0 . nil)))))

(defmethod render-node ((l parse:ref-list) stack context rect
                        &key indent delimited (transform #'identity))
  "A `delimited' list is drawn with `*list-delimiters*' around it, recursively unless
`delimited' is :shallow. It's opening parens is selectable."
  (let* ((location (car stack))
         (focused (location= location (focus context)))
         (head (first (elements l))))
    (flet ((contents (rect)
             (let* ((keys `(,@(when (eq delimited t) '(:delimited t))
                            :transform ,transform
                            ,@(when (functionp indent) `(:indent ,indent))))
                    (spec (if indent indent
                              (ref-list-indentation
                               (elements l) (list-locator location) (cdr stack)
                               context rect keys))))
               (render-elements (elements l) spec (list-locator location)
                                (cdr stack) context rect keys))))
      (if (and delimited (not (parse:unary-reader-marker-p head)))
          (let* ((handle (make-location :node l :id :open))
                 (open (render-delimiter (car *list-delimiters*) handle
                                         (cons handle stack) rect
                                         :hoverable t))
                 (inner (contents (tui:clamp-rect
                                   (tui:copy-rect rect :x (tui:rect-x2 (tui:rect open)))
                                   rect)))
                 (close (multiple-value-bind (end-x end-y) (view-end inner)
                          (render-delimiter (cdr *list-delimiters*) location stack
                                            (tui:clamp-rect
                                             (tui:copy-rect rect :x end-x :y end-y)
                                             rect)))))
            (when (location= handle (focus context))
              (setf (tui:key-handler open) (global-key-handler l handle)
                    (tui:focused open) t
                    (gethash :focus-view (redisplay-cache context)) open))
            (make-instance 'ast-view
                           :location (car stack)
                           :key-handler (global-key-handler l location)
                           :focused focused
                           :rect (tui:clamp-rect
                                  (tui:copy-rect
                                   rect :rows (max 1 (tui:rect-rows (tui:rect inner)))
                                   :cols (- (max (tui:rect-x2 (tui:rect inner))
                                                 (tui:rect-x2 (tui:rect close)))
                                            (tui:rect-x rect)))
                                  rect)
                           :children (list open inner close)))
          (let* ((content (contents rect))
                 (view (if (elements l)
                           (make-instance
                            'ast-view
                            :location (car stack)
                            :key-handler (global-key-handler l location)
                            :focused focused
                            :rect (tui:clamp-rect
                                   (tui:copy-rect
                                    rect :rows (max 1 (tui:rect-rows (tui:rect content)))
                                         :cols (- (tui:rect-x2 (tui:rect content))
                                                  (tui:rect-x rect)))
                                   rect)
                            :children (list content))
                           (render-delimiter (format nil "~a~a" (car *list-delimiters*)
                                                     (cdr *list-delimiters*))
                                             location stack rect :hoverable t))))
            (setf (tui:key-handler view) (global-key-handler l location)
                  (tui:focused view) focused)
            view)))))

;; for body lists
(defmethod render-node ((l list) stack context rect
                        &key (indent *horizontal*) delimited)
  (let* ((location (car stack))
         (view (render-elements l indent (list-locator location)
                                (cdr stack) context rect
                                (when delimited (list :delimited delimited)))))
    (setf (tui:key-handler view) (global-key-handler l location)
          (tui:focused view) (location= location (focus context)))
    view))

(defun render-literal-line (line stack context rect &key style)
  (declare (ignore stack context))
  (tui:puts* line 1 1 rect style)
  (make-instance 'tui:view
                 :rect (tui:clamp-rect
                        (tui:copy-rect rect :rows 1 :cols (tui:display-width line))
                        rect)))

(defparameter *horizontal* '((0 . nil)))
(defparameter *vertical* '((0 . t)))

(defun render-literal-text (node text stack context rect)
  (let* ((location (car stack))
         (focused (location= location (focus context)))
         (style (if (and focused (buffer-focused-p context))
                    (tui:make-style :fg #x0)
                    (tui:make-style :fg #x2aa198)))
         (contents (render-elements (split-string text #\newline) *vertical*
                                    (constantly location) (cdr stack) context rect
                                    `(:style ,style) #'render-literal-line)))
    (make-instance 'ast-view
                   :rect (tui:rect contents)
                   :location location
                   :children (list contents)
                   :hoverable t
                   :key-handler (when focused
                                  (global-key-handler node location))
                   :focused focused)))

(defmethod render-node ((node parse:literal) stack context rect &key)
  (render-literal-text node (format nil "~a" (parse:str node)) stack context rect))

(defmethod render-node ((node string) stack context rect &key)
  (render-literal-text node (format nil "~s" node) stack context rect))


;; - unary operators have space removed
;; - for 2 arguments, draw *arb-arity-binops* infix
;;   - higher arity duplicated op views break invariant: always a single focused view
(defun function-keyword-start (node stack)
  "Returns the positional argument count before &key, or NIL if not present."
  (let ((name (parse:name node)))
    (unless (typep name 'parse:symbol-ref)
      (return-from function-keyword-start nil))
    (when (eq :lexical (nth-value 1 (parse:env-lookup name :function
                                                      (environment-at stack))))
      (return-from function-keyword-start nil))
    (let* ((symbol (parse:resolve name))
           (arglist (when (and symbol (fboundp symbol))
                      (slynk-backend:arglist symbol))))
      (when (proper-list-p arglist)
        (loop with count = 0
              for tail on arglist
              for parameter = (car tail)
              do (case parameter
                   (&key (return count))
                   ((&rest &body &whole &environment) (pop tail))
                   (&aux (return nil))
                   ((&optional &allow-other-keys))
                   (otherwise
                    (when (member parameter lambda-list-keywords) (return nil))
                    (incf count))))))))

(defun function-call-indentation (node stack context rect)
  "Packs arguments by width, keeping the &key tail paired per line."
  (let* ((arguments (parse:body node))
         (limit (max 1 (min (tui:rect-cols rect)
                            (- *column-limit* (tui:rect-x rect)))))
         (measuring (make-buffer :buffer-ui (buffer-ui context)
                                 :buffer-id (buffer-id context)
                                 :buffer-state (buffer-state context)))
         (bounds (tui:copy-rect rect :y 0 :cols limit))
         (widths (loop for arg in arguments for index from 0
                       collect (tui:rect-cols
                                (measure-node arg
                                              (cons (make-location
                                                     :node node
                                                     :id (list 'parse:body index))
                                                    stack)
                                              measuring bounds))))
         (keyword-start (function-keyword-start node stack)))
    (labels ((fits (widths)
               (<= (+ (reduce #'+ widths :initial-value 0)
                      (max 0 (1- (length widths)))) limit))
             (greedy (widths)
               (loop with used = 0 with count = 0 with rows = nil
                     for width in widths
                     do (when (and (plusp count) (> (+ used 1 width) limit))
                          (push (cons 0 count) rows)
                          (setf used 0 count 0))
                        (incf used (+ width (if (plusp count) 1 0)))
                        (incf count)
                     finally (when (plusp count) (push (cons 0 count) rows))
                             (return (nreverse rows)))))
      (cond ((fits widths) '((0 . nil)))
            ((or (null keyword-start) (>= keyword-start (length widths)))
             (greedy widths))
            (t
             (let ((keywords (nthcdr keyword-start widths)))
               (append (greedy (subseq widths 0 keyword-start))
                       (if (fits keywords)
                           (list (cons 0 (length keywords)))
                           ;; dangling kw
                           '((0 . 2))))))))))

(defmethod render-node ((node parse:function-call) stack context rect &key)
  (trivia:match (and (= 2 (length (parse:body node))) (is-binop-call node))
    ;; note this case must go first since division is a binop
    ('cl:/
     (let* ((location (car stack))
            (arg1-view (render-node
                        (first (parse:body node))
                        (cons (make-location :node node :id '(parse:body 0)) stack)
                        context
                        rect))
            (arg1-rect (tui:rect arg1-view))
            (arg2-view (render-node
                        (second (parse:body node))
                        (cons (make-location :node node :id '(parse:body 1)) stack)
                        context
                        (tui:clamp-rect (tui:copy-rect rect :y (1+ (tui:rect-y2 arg1-rect)))
                                        rect)))
            (arg2-rect (tui:rect arg2-view))
            (width (1+ (max (tui:rect-cols arg1-rect) (tui:rect-cols arg2-rect))))
            (div-loc (make-location :node node :id 'parse:name))
            (line-view
              (make-instance 'ast-view
                             :location div-loc
                             :hoverable t
                             :rect (tui:clamp-rect
                                    (tui:copy-rect rect :rows 1 :cols width
                                                       :y (tui:rect-y2 arg1-rect))
                                    rect)
                             :key-handler (global-key-handler (parse:name node)
                                                              div-loc)
                             :focused (location= div-loc (focus context)))))
       (setf (gethash (parse:name node) (node-views context)) line-view)
       (setf (view-stack line-view) (cons div-loc stack))
       (when (location= div-loc (focus context))
         (setf (gethash :focus-view (redisplay-cache context)) line-view))
       (tui:puts* (make-string width :initial-element #\─)
                  (1+ (tui:rect-rows arg1-rect)) 1 rect)
       ;;
       (make-instance 'ast-view
                      :location location
                      :rect (tui:clamp-rect
                             (tui:copy-rect rect :cols width
                                                :rows (+ (tui:rect-rows arg1-rect)
                                                         1 (tui:rect-rows arg2-rect)))
                             rect)
                      :children (list arg1-view line-view arg2-view)
                      :key-handler (global-key-handler node location)
                      :focused (location= location (focus context)))))
    ;; need wrapper for args
    ((and op (not nil))
     (flet ((is-bracketed (arg)
              (when-let (arg-op (is-binop-call arg))
                (or (> (binop-precedence op) (binop-precedence arg-op))
                    (and (eq op 'cl:-) (member arg-op '(cl:+ cl:-)))))))
       (let* ((location (car stack))
              (arg1 (first (parse:body node)))
              (arg1-bracketed (is-bracketed arg1))
              (arg1-view
                (if arg1-bracketed
                    (render-node arg1
                                 (cons (make-location :node node :id '(parse:body 0)) stack)
                                 context (tui:clamp-rect
                                          (tui:copy-rect rect :x (1+ (tui:rect-x rect)))
                                          rect))
                    (render-node arg1
                                 (cons (make-location :node node :id '(parse:body 0)) stack)
                                 context rect)))
              (arg1-rect (tui:rect arg1-view))
              (op-view
                (render-node (parse:name node)
                             (cons (make-location :node node :id 'parse:name) stack)
                             context (tui:clamp-rect
                                      (tui:copy-rect rect :x (1+ (tui:rect-x2 arg1-rect)))
                                      rect)))
              (op-rect (tui:rect op-view))
              (arg2 (second (parse:body node)))
              (arg2-bracketed (is-bracketed arg2))
              (arg2-view
                (render-node arg2
                             (cons (make-location :node node :id '(parse:body 1)) stack)
                             context (tui:clamp-rect
                                      (tui:copy-rect rect :x (1+ (tui:rect-x2 op-rect)))
                                      rect)))
              (arg2-rect (tui:rect arg2-view)))
         ;; draw parens
         (when arg1-bracketed
           (let ((rcol (+ 2 (tui:rect-cols arg1-rect))))
             (if (= 1 (tui:rect-rows arg1-rect))
                 (progn
                   (tui:put* #\( 1 1 rect)
                   (tui:put* #\) 1 rcol rect))
                 (progn
                   (tui:put* #\⎛ 1 1 rect)
                   (tui:put* #\⎝ (tui:rect-rows arg1-rect) 1 rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put* #\⎜ y 1 rect))
                   (tui:put* #\⎞ 1 rcol rect)
                   (tui:put* #\⎠ (tui:rect-rows arg1-rect) rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg1-rect)
                         do (tui:put* #\⎟ y rcol rect))))))
         (when arg2-bracketed
           (let ((lcol (- (tui:rect-x arg2-rect) (tui:rect-x rect)))
                 (rcol (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                          (1+ (tui:rect-cols arg2-rect)))))
             (if (= 1 (tui:rect-rows arg2-rect))
                 (progn
                   (tui:put* #\( 1 lcol rect)
                   (tui:put* #\) 1 (+ (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                      (1+ (tui:rect-cols arg2-rect)))
                             rect))
                 (progn
                   (tui:put* #\⎛ 1 lcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put* #\⎜ y (- (tui:rect-x arg2-rect) (tui:rect-x rect))
                                      rect))
                   (tui:put* #\⎝ (tui:rect-rows arg2-rect) lcol rect)
                   (tui:put* #\⎞ 1 rcol rect)
                   (loop for y from 2 below (tui:rect-rows arg2-rect)
                         do (tui:put* #\⎟ y rcol rect))
                   (tui:put* #\⎠ (tui:rect-rows arg2-rect) rcol rect)))))
         (make-instance 'ast-view
                        :location location
                        :rect (tui:clamp-rect
                               (tui:copy-rect rect
                                             :rows (max (tui:rect-rows arg1-rect)
                                                        (tui:rect-rows arg2-rect))
                                             :cols (+ (tui:rect-cols arg1-rect)
                                                      (if arg1-bracketed 1 0)
                                                      2 (tui:rect-cols op-rect)
                                                      (tui:rect-cols arg2-rect)
                                                      (if arg2-bracketed 1 0)))
                               rect)
                        :children (list arg1-view op-view arg2-view)
                        :key-handler (global-key-handler node location)
                        :focused (location= location (focus context))))))
    (_
     (let* ((location (car stack))
            (name-view (render-node (parse:name node)
                                    (cons (make-location :node node :id 'parse:name) stack)
                                    context rect))
            (name-rect (tui:rect name-view))
            (args-bounds (tui:clamp-rect
                          (tui:copy-rect rect :x (+ 1 (tui:rect-x2 name-rect))) rect))
            (args-view
              (render-elements (parse:body node)
                               (function-call-indentation node stack context args-bounds)
                               (list-locator
                                (make-location :node node :id 'parse:body))
                               stack context args-bounds nil))
            (args-rect (tui:rect args-view))
            (fun-rect (tui:clamp-rect
                       (tui:copy-rect rect :rows (max 1 (tui:rect-rows args-rect))
                                           :cols (+ (tui:rect-cols name-rect)
                                                    1 (tui:rect-cols args-rect)))
                       rect)))
       (make-instance 'ast-view
                      :location location
                      :rect fun-rect
                      :children (list name-view args-view)
                      :key-handler (global-key-handler node location)
                      :focused (location= location (focus context)))))))

;;; macro calls
(defparameter *loop-clause-keywords*
  '("NAMED" "WITH" "FOR" "AS" "REPEAT" "WHILE" "UNTIL" "ALWAYS" "NEVER" "THEREIS"
    "COLLECT" "COLLECTING" "APPEND" "APPENDING" "NCONC" "NCONCING" "SUM" "SUMMING"
    "COUNT" "COUNTING" "MAXIMIZE" "MAXIMIZING" "MINIMIZE" "MINIMIZING"
    "DO" "DOING" "RETURN" "INITIALLY" "FINALLY"
    "WHEN" "IF" "UNLESS" "ELSE" "END" "AND"))
(defparameter *loop-prefix-keywords* '("AND" "ELSE"))
(defparameter *loop-body-keywords* '("DO" "DOING" "INITIALLY" "FINALLY"))

(defun find-loop-keyword (element)
  (and (typep element 'parse:symbol-ref)
       (find (parse:name element) *loop-clause-keywords* :test #'string=)))

(defun loop-indentation (elements &optional (package *package*))
  "Indent spec for a loop form. The operator has its own line, then a line indented by one
for every clause keyword, except those following and/else. Forms of a do-like clause after
its first are aligned with the first, and a loop without keywords has a line per form."
  (let* ((indent 1)
         (rows (list (cons 0 1)))
         (column indent)
         (body-column nil)
         (body-started nil)
         (previous (first elements)))
    (loop for element in (rest elements)
          for first-element = t then nil
          for keyword = (find-loop-keyword element)
          do (cond (keyword
                    (if (member (find-loop-keyword previous) *loop-prefix-keywords*
                                :test #'equal)
                        (progn (incf (cdar rows))
                               (incf column
                                     (1+ (length (symbol-ref-text previous package)))))
                        (progn (push (cons indent 1) rows)
                               (setf column indent)))
                    (setf body-column (when (member keyword *loop-body-keywords*
                                                    :test #'string=)
                                        (+ column
                                           (length (symbol-ref-text element package)) 1))
                          body-started nil))
                   (first-element
                    (push (cons indent 1) rows)
                    (setf body-column indent
                          body-started t))
                   ((and body-column body-started)
                    (push (cons body-column 1) rows))
                   (t
                    (incf (cdar rows))
                    (setf body-started t)))
             (setf previous element))
    (nreverse rows)))

(defun macro-call-indentation (op &optional (package *package*))
  "`loop-indentation' for loop. Note `op' can be a hole"
  (if-let (s (and (typep op 'parse:symbol-ref) (parse:resolve op)))
    (cond ((eq s 'loop) (lambda (elements) (loop-indentation elements package)))
          ((member s '(setf psetf psetq)) `((0 . 3) (,(1+ (length (string s))) . 2)))
          ((fboundp s)
           (let* ((arglist (slynk-backend:arglist s)))
             (when (proper-list-p arglist)
               (let ((arglist (remove '&optional arglist)))
                 (if-let (index (position-if (lambda (s) (member s '(&rest &body)))
                                             arglist))
                   (progn
                     (when-let (env-pos (position '&environment arglist))
                       (when (< env-pos index)
                         (decf index 2)))
                     (when (member '&whole arglist)
                       (decf index 2))
                     (case index
                       (0 `((0 . 2) (,(1+ (length (string s))) . t)))
                       (1 `((0 . ,(1+ index)) (1 . t)))
                       (2 (if (uiop:string-prefix-p "DEF" (string s))
                              `((0 . 3) (1 . t)) ; e.g. defgeneric, deftype
                              `((0 . 2) (3 . 1) (1 . t))))
                       (3 `((0 . 3) (1 . t))) ; e.g. defclass define-condition
                       (t `((0 . ,(1+ index)) (1 . t)))))
                   (progn
                     *horizontal*))))))
          (t *horizontal*))
    *horizontal*))

(defun setf-indentation (node stack context rect)
  "Keeps place/value pairs together unless their combined width exceeds the limit."
  (let* ((indent (1+ (length (symbol-ref-text (parse:op node)
                                              (buffer-package context)))))
         (available (max 0 (min (tui:rect-cols rect)
                                (- *column-limit* (tui:rect-x rect)))))
         (measuring (make-buffer :buffer-ui (buffer-ui context)
                                 :buffer-id (buffer-id context)
                                 :buffer-state (buffer-state context)))
         (bounds (tui:copy-rect rect :x (+ (tui:rect-x rect) (min indent available))
                                     :y 0 :cols (max 0 (- available indent))))
         (rows nil))
    (labels ((width (form index)
               (let* ((parsed (or (car (gethash form (parse:subforms node))) form))
                      (measured (measure-node
                             parsed (cons (make-location :node node
                                                         :id (list 'parse:body index))
                                          stack)
                             measuring bounds :delimited t
                             :transform (lambda (elt)
                                          (or (car (gethash elt (parse:subforms node)))
                                              elt)))))
                 (tui:rect-cols measured))))
      (loop for tail on (parse:body node) by #'cddr
            for index from 0 by 2
            for firstp = (zerop index)
            for count = (if firstp 2 1)
            for row-indent = (if firstp 0 indent)
            do (if (and (cdr tail)
                        (> (+ indent (width (car tail) index) 1
                              (width (cadr tail) (1+ index)))
                           available))
                   (progn (push (cons row-indent count) rows)
                          (push (cons indent 1) rows))
                   (push (cons row-indent (+ count (if (cdr tail) 1 0))) rows))))
    (or (nreverse rows) '((0 . 1)))))

(defmethod render-node ((node parse:macro-call) stack context rect &key)
  (let* ((location (car stack))
         (view (render-elements
                (cons (parse:op node) (parse:body node))
                (if (and (typep (parse:op node) 'parse:symbol-ref)
                         (eq (parse:resolve (parse:op node)) 'cl:setf))
                    (setf-indentation node stack context rect)
                    (macro-call-indentation (parse:op node) (buffer-package context)))
                (lambda (index)
                  (make-location :node node
                                 :id (if (zerop index)
                                         'parse:op
                                         (list 'parse:body (1- index)))))
                stack context rect
                `(:delimited t
                  :transform ,(lambda (elt)
                                (or (car (gethash elt (parse:subforms node)))
                                    elt))))))
    (make-instance 'ast-view
                   :location location
                   :children (list view)
                   :rect (tui:rect view)
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

;;; quasiquote
(defmethod render-node ((node parse:quasiquote-form) stack context rect &key)
  (let* ((location (car stack))
         (op-view (render-symbol-space 'parse:read-quasiquote
                                       (make-location :node node :id 'parse:op)
                                       stack context rect))
         (op-rect (tui:rect op-view))
         (view (render-node (parse:thing node)
                            (cons (make-location :node node :id 'parse:thing) stack)
                            context
                            (tui:clamp-rect (tui:copy-rect rect :x (tui:rect-x2 op-rect))
                                            rect)
                            :delimited t
                            :indent (lambda (elements)
                                      (let ((spec (macro-call-indentation
                                                   (first elements)
                                                   (buffer-package context))))
                                        (if (functionp spec)
                                            (funcall spec elements)
                                            spec)))))
         (view-rect (tui:rect view)))
    (make-instance 'ast-view
                   :location location
                   :children (list op-view view)
                   :rect (tui:clamp-rect
                          (tui:copy-rect rect :rows (tui:rect-rows view-rect)
                                              :cols (+ (tui:rect-cols op-rect)
                                                       (tui:rect-cols view-rect)))
                          rect)
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

;;
;;; layout generation
;;
(defvar *default-expansions* (make-hash-table :test #'eq)
  "Operator symbol -> closure constructing a default node for that form.")

(defrecord layout ()
  ((op :type symbol)
   (rows :type list)
   (templates :type list)) ;alist
  (:representation :struct)
  (:documentation
   "Describes the rendered shape of a call to `op'.
`rows' are laid out vertically and `templates' contains (slot . constructor)
pairs building new elements of &rest and &body slots."))

(defvar *layouts* (make-hash-table :test #'eq) "Form class -> its `layout'.")

(defun layout-slots (layout)
  (loop for row in (layout-rows layout)
        append (typecase row
                 (cons (loop for item in row
                             when (symbolp item) collect item
                               when (consp item) collect (car item)))
                 (symbol (list row)))))

(defun layout-template (layout slot)
  (cdr (assoc slot (layout-templates layout))))

(defun slot-kind (layout slot)
  (cdr (assoc slot (parse:form-slot-kinds (layout-op layout)))))

(defgeneric focus-order (node)
  (:method (node)
    (when-let (layout (gethash (type-of node) *layouts*))
      (layout-slots layout)))
  (:method ((node parse:function-call)) '(parse:name parse:body))
  (:method ((node parse:macro-call)) '(parse:op parse:body))
  (:method ((node parse:function-code))
    '(parse:lambda-list parse:docstring parse:declarations parse:body))
  (:documentation "The slots of `node' in reading order."))

(defun slot-end-position (node slot)
  "The last focusable position of `slot', or NIL."
  (let ((value (node-at (make-location :node node :id slot))))
    (cond ((null value) nil)
          ;; plain list bodies are never focused, see `deflayout'
          ((listp value) (append-id slot (1- (length value))))
          (t slot))))

(defun move-back (node id)
  "The position preceding `id' in `node' for deletions. Returns `id' if first."
  (flet ((previous-slot (slot)
           (let ((before (loop for s in (focus-order node) until (eq s slot) collect s)))
             (or (loop for s in (reverse before) thereis (slot-end-position node s))
                 slot))))
    (if (symbolp id)
        (previous-slot id)
        (let ((index (id-index id))
              (parent (parent-id id)))
          (cond ((< 0 index) (append-id parent (1- index)))
                ((symbolp parent) (previous-slot parent))
                (t parent))))))

(defun default-slot-value (kind template)
  (case kind
    ((&rest) (apply #'ref-list (when template (list (funcall template)))))
    ((&body parse::&declarations parse::&rest-qualifiers)
     (when template (list (funcall template))))
    ((parse::&lambda parse::&macro-lambda parse::&method-lambda)
     (make-instance 'parse:function-code
                    :lambda-list-kind (make-keyword (subseq (string kind) 1))
                    :lambda-list (ref-list)
                    :body (list (hole))))
    (t (hole))))

(defun default-node (layout)
  "Initializes the slots of `layout', leaving unused alternative slots unbound."
  (apply #'make-instance (layout-op layout)
         (loop with visible = (layout-slots layout)
               with optional = (parse:form-optional-slots (layout-op layout))
               for (slot . kind) in (remove-duplicates
                                     (parse:form-slot-kinds (layout-op layout))
                                     :key #'car :from-end t)
               unless (and (member slot optional) (not (member slot visible)))
                 append (list (make-keyword slot)
                              (default-slot-value kind (layout-template layout slot))))))

(defun slot-render-keys (node slot)
  (case (cdr (assoc slot (parse:form-slot-kinds (type-of node))))
    (parse::&tree '(:delimited t))
    (parse::&declarations (list :indent *vertical* :delimited t))
    (&rest (list :indent *vertical* :delimited :shallow))
    (t (list :indent *vertical*))))

(defun render-layout-slot (node item stack context rect)
  (cond
    ((consp item)
     (destructuring-bind (slot spec) item
       (apply #'render-node (funcall slot node)
              (cons (make-location :node node :id slot) stack)
              context rect
              :indent spec
              (slot-render-keys node slot))))
    ((eq item 'parse:op)
     (render-symbol-space (parse:op node)
                          (make-location :node node :id 'parse:op)
                          stack context rect))
    ((symbolp item)
     (apply #'render-node (funcall item node)
            (cons (make-location :node node :id item) stack)
            context rect
            (slot-render-keys node item)))
    (t
     (cerror "unexpected layout element ~a" item))))

(defun render-horizontal-layout (node across-layout stack context rect)
  (tui:with-horizontal (rect)
    (dolist (item across-layout)
      (if (integerp item)
          (tui:pad item)
          (tui:place (rect) (render-layout-slot node item stack context rect))))))

(defun render-vertical-layout (node down-layout stack context rect)
  (tui:with-vertical (rect)
    (dolist (item down-layout)
      (cond ((integerp item)
             (tui:pad item))
            ((consp item)
             (tui:place (rect) (render-horizontal-layout node item stack context rect)))
            (t
             (tui:place (rect) (render-layout-slot node item stack context rect)))))))

(defmethod render-node ((node parse:irregular-form) stack context rect &key)
  (let* ((layout (gethash (type-of node) *layouts*))
         (vertical (render-vertical-layout node (layout-rows layout) stack context rect))
         (location (car stack)))
    (make-instance 'ast-view
                   :location location
                   :children (list vertical)
                   :rect (tui:rect vertical)
                   :key-handler (global-key-handler node location)
                   :focused (location= location (focus context)))))

(defun register-layout (layout)
  (assert (ends-with-subseq "-FORM" (string (layout-op layout))))
  (let ((len (length "-FORM")))
    (setf (gethash (layout-op layout) *layouts*) layout
          (gethash (find-symbol (string-drop (string (layout-op layout)) len)
                                "WEAVE-PARSER")
                   *default-expansions*)
          (lambda () (default-node layout)))))

(defmacro deflayout (type templates rows)
  "Lays out forms of class `type' in `rows' of slots, where `templates' are forms building
new elements for its &rest and &body slots.
ASSUMES that &rest slots accept (slot i j) even if the list is optional like let.
ASSUMES we never focus a plain list body."
  `(register-layout
    (make-layout :op ',type
                 :rows ',rows
                 :templates (list ,@(loop for (slot . template) in templates
                                          collect `(cons ',slot (lambda () ,template)))))))

(deflayout parse:let*-form ((parse:vars . (ref-list (hole) (hole)))
                            (parse:body . (hole)))
  ((parse:op parse:vars)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:let-form ((parse:vars . (ref-list (hole) (hole)))
                           (parse:body . (hole)))
  ((parse:op parse:vars)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:flet-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:funs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:labels-form
    ((parse:funs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:funs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:macrolet-form
    ((parse:macro-defs
      . (ref-list (hole)
                  (make-instance 'parse:function-code
                                 :lambda-list-kind :macro-lambda
                                 :lambda-list (ref-list)
                                 :body (list (hole)))))
     (parse:body . (hole)))
  ((parse:op parse:macro-defs)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:symbol-macrolet-form ((parse:macro-code . (ref-list (hole) (hole)))
                                       (parse:body . (hole)))
  ((parse:op parse:macro-code)
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:block-form ((parse:body . (hole)))
  ((parse:op parse:name)
   (1 parse:body)))

(deflayout parse:catch-form ((parse:body . (hole)))
  ((parse:op parse:tag)
   (1 parse:body)))

(deflayout parse:throw-form ()
  ((parse:op parse:tag 1 parse:result)))

(deflayout parse:return-from-form ((parse:value . (hole)))
  ((parse:op parse:name 1 parse:value)))

(deflayout parse:if-form ((parse:then-else . (hole)))
  ((parse:op parse:test)
   (3 parse:then-else)))

(deflayout parse:setq-form ((parse:forms . (hole)))
  ((parse:op (parse:forms ((0 . 2))))))

(deflayout parse:progn-form ((parse:forms . (hole)))
  (parse:op
   (1 parse:forms)))

(deflayout parse:tagbody-form ((parse:body . (hole)))
  (parse:op
   (1 parse:body)))

(deflayout parse:go-form ()
  ((parse:op parse:tag)))

(deflayout parse:the-form ()
  ((parse:op parse:type-specifier 1 parse:form)))

(deflayout parse:unwind-protect-form ((parse:cleanup . (hole)))
  ((parse:op)
   (3 parse:protected)
   (1 parse:cleanup)))

(deflayout parse:multiple-value-prog1-form ((parse:body . (hole)))
  ((parse:op parse:values-form)
   (1 parse:body)))

(deflayout parse:multiple-value-call-form ((parse:body . (hole)))
  ((parse:op parse:fun 1 parse:arg 1 parse:body)))

(deflayout parse:progv-form ((parse:body . (hole)))
  ((parse:op parse:var-list 1 parse:val-list)
   (1 parse:body)))

(deflayout parse:locally-form ((parse:body . (hole)))
  (parse:op
   (1 parse:decls)
   (1 parse:body)))

(deflayout parse:eval-when-form ((parse:body . (hole)))
  ((parse:op parse:situations)
   (1 parse:body)))

(deflayout parse:load-time-value-form ()
  ((parse:op parse:form 1 parse:read-only-p)))

(deflayout parse:function-form ()
  ((parse:op parse:fun-designator)))

(deflayout parse:quote-form ()
  ((parse:op parse:thing)))

(deflayout parse:defmacro-form ()
  ((parse:op parse:name)
   (1 parse:macro-code)))

;; TODO (setf f) printing
(deflayout parse:defun-form ()
  ((parse:op parse:name)
   (1 parse:fun-code)))

(deflayout parse:defmethod-form ()
  ((parse:op parse:name 1 parse:qualifiers)
   (1 parse:fun-code)))

(deflayout parse:lambda-form ()
  ((parse:op parse:fun-code)))
;; override for nonempty lambda list
(setf (gethash 'lambda *default-expansions*)
      (lambda ()
        (make-instance 'parse:lambda-form
                       :fun-code (make-instance 'parse:function-code
                                                :lambda-list-kind :lambda
                                                :lambda-list (ref-list (hole))
                                                :body (list (hole))))))

;;; function-code (lambda-list, documentation and body, shared by defun/defmacro/
;;; defmethod/lambda/flet/labels), including declarations.
(defmethod render-node ((node parse:function-code) stack context rect &key)
  (let* ((location (car stack))
         (lambda-list-view
           (render-node (parse:lambda-list node)
                        (cons (make-location :node node :id 'parse:lambda-list) stack)
                        context rect :delimited t))
         (ll-rect (tui:rect lambda-list-view))
         (docstring (parse:docstring node))
         (docstring-view
           (when docstring
             (render-node docstring
                          (cons (make-location :node node :id 'parse:docstring) stack)
                          context
                          (tui:clamp-rect
                           (tui:copy-rect rect :x (tui:rect-x rect)
                                               :y (tui:rect-y2 ll-rect))
                           rect))))
         (declarations-y (if docstring-view
                             (tui:rect-y2 (tui:rect docstring-view))
                             (tui:rect-y2 ll-rect)))
         (declarations-view
           (when (parse:declarations node)
             (render-node (parse:declarations node)
                          (cons (make-location :node node :id 'parse:declarations) stack)
                          context
                          (tui:clamp-rect (tui:copy-rect rect :y declarations-y) rect)
                          :indent *vertical* :delimited t)))
         (body-y (if declarations-view
                     (tui:rect-y2 (tui:rect declarations-view))
                     declarations-y))
         (body-view
           (render-elements (parse:body node) *vertical*
                            (list-locator (make-location :node node :id 'parse:body))
                            stack context
                            (tui:clamp-rect
                             (tui:copy-rect rect :x (tui:rect-x rect) :y body-y)
                             rect)
                            nil))
         (children (remove nil (list lambda-list-view docstring-view
                                     declarations-view body-view))))
    (make-instance
     'ast-view
     :location location
     :children children
     :rect (tui:clamp-rect
            (tui:copy-rect
             rect
             :rows (reduce #'+   children :key (lambda (c) (tui:rect-rows (tui:rect c))))
             :cols (reduce #'max children :key (lambda (c) (tui:rect-cols (tui:rect c)))))
            rect)
     :key-handler (global-key-handler node location)
     :focused (location= location (focus context)))))

;;; completions
(defun render-completion (sym index state rect)
  (when (plusp (tui:rect-rows rect))
    (let ((prefix (parse:name (anchor state)))
          (selectedp (= index (selection state))))
      (flet ((style (bold)
               (tui:make-style :fg #xeeeeee
                               :bg (if selectedp #x2aa198 #x0)
                               :boldp bold
                               :italicp nil
                               :reversep nil
                               :underlinep nil)))
        (tui:fill-rect (style nil) (tui:copy-rect rect :rows 1)
                       :char #\space)
        (tui:puts* (string-downcase prefix) 1 1 rect (style t))
        (tui:puts* (string-downcase
                    (nth-value 1 (starts-with-subseq prefix (string sym) :return-suffix t)))
                   1 (+ 1 (length prefix)) rect (style nil)))
      (make-instance 'tui:view :rect (tui:copy-rect rect :rows 1)))))

(defun render-completion-window (buffer)
  (when-let (state (completion-state buffer))
    (let* ((bounds (tui::screen-rect))
           (anchor (anchor state))
           (anchor-rect (node-screen-rect buffer anchor))
           (maxlen (loop for candidate in (candidates state)
                         maximize (tui:display-width (string candidate))))
           (rect (tui:clamp-rect
                  (tui:make-rect :x (tui:rect-x anchor-rect)
                                 :y (tui:rect-y2 anchor-rect)
                                 :rows (tui:rect-rows bounds)
                                 :cols maxlen)
                  bounds)))
      (tui:with-vertical (rect)
        (loop for candidate in (candidates state)
              for index from 0
              until (tui:full)
              do (tui:place (rect) (render-completion candidate index state rect)))))))

(defun make-cell-buffer (rows cols)
  (let ((buffer (make-array (list rows cols))))
    (dotimes (i (array-total-size buffer) buffer)
      (setf (row-major-aref buffer i) (tui::make-cell)))))

(defun render-one-toplevel (index start forms buffer screen-rows cols)
  "Renders the form at `index' into a segment placed at `start', relative to the staging
coordinates. `cols' is a strict bound on the render's width.
`screen-rows' is a hint, as staging may draw clipped portions beyond the screen's limits.
The returned view uses staging coordinates but all drawing is done to a local buffer."
  (precompute-form-environments (make-location :node forms :id index))
  (loop with form = (nth index forms)
        ;; cap=1 would always retry since the render will use a positive number of rows
        for capacity = (max 2 (gethash form (stage-cache buffer) screen-rows))
          then (* 2 capacity)
        for cells = (make-cell-buffer capacity cols)
        do (let* ((tui::*put-buffer* cells)
                  (rect (tui:make-rect :x 0 :y 0 :rows capacity :cols cols))
                  (location (make-location :node forms :id index)))
             (tui:fill-rect (tui:make-style :bg #x0) rect)
             (let* ((view (render-node form (list location) buffer rect))
                    (used (tui:rect-y2 (tui:rect view))))
               ;; equality can mean clipping; require spare space
               (when (< used capacity)
                 ;; slightly >1 to avoid thrashing on insertions
                 (setf (gethash form (stage-cache buffer)) (ceiling (* 1.2 used)))
                 (tui:view-traverse
                  view
                  (lambda (v)
                    (setf (tui:rect v)
                          (tui:copy-rect (tui:rect v)
                                         :y (+ start (tui:rect-y (tui:rect v)))))))
                 (return (make-segment :index index :start start :rows used
                                       :buffer cells :view view)))))))

(defun segment-end (segment toplevel-count)
  "Returns the staging-relative row after `segment', including its inter-form separator."
  (+ (segment-start segment) (segment-rows segment)
     (if (< (segment-index segment) (1- toplevel-count)) 1 0)))

(defun reveal-focus-row (viewport rect rows)
  "Returns the minimum viewport adjustment needed to show `rect'."
  (cond ((< (tui:rect-y rect) viewport) (tui:rect-y rect))
        ((> (tui:rect-y2 rect) (+ viewport rows))
         (if (> (tui:rect-rows rect) rows)
             (tui:rect-y rect)
             (- (tui:rect-y2 rect) rows)))
        (t viewport)))

(defun stage-toplevel-window (buffer scroll focus-index bounds)
  "Returns staged segments and the viewport row revealing the focus."
  (let* ((forms (ast buffer))
         (count (length forms))
         (screen-rows (tui:rect-rows bounds))
         (index (scroll-position-index scroll)))
    ;; keep a predecessor above both anchor and focus for upward movement.
    (loop with viewport = 0
          for i from (max 0 (1- (min index focus-index))) below count
          for start = 0 then (segment-end segment count)
          for segment = (render-one-toplevel i start forms buffer
                                             screen-rows (tui:rect-cols bounds))
          do (when (and (= i index) (<= index focus-index))
               (setf viewport (+ start (min (scroll-position-row scroll)
                                            (segment-rows segment)))))
             (when (= i focus-index)
               (let ((rect (tui:rect (gethash :focus-view (redisplay-cache buffer)))))
                 (setf viewport (if (< focus-index index)
                                    (tui:rect-y rect)
                                    (reveal-focus-row viewport rect screen-rows)))))
          collect segment into segments
          ;; include one whole successor beyond the resolved viewport and focus.
          until (and (> i focus-index) (>= start (+ viewport screen-rows)))
          finally (return (values segments viewport)))))

(defun scroll-position-at-row (segments y-offset)
  "Returns the index of the toplevel form and the offset we're at, given a `y-offset'
 relative to staged `segments'."
  (let* ((segment (or (find-if (lambda (segment)
                                 (<= y-offset
                                     (+ (segment-start segment) (segment-rows segment))))
                               segments)
                      (lastcar segments)))
         (offset (max 0 (- y-offset (segment-start segment)))))
    (make-scroll-position :index (segment-index segment)
                          :row (min offset (segment-rows segment)))))

(defun render-toplevel-window (buffer buffer-rect)
  "Stages a window, clamps to focus and copies its *visible* cells to uncursed's screen
relative to `buffer-rect'. Returns a view rendered with staging coordinates.
note: the returned view includes unseen forms not clipped to `buffer-rect'."
  (let* ((forms (ast buffer))
         (rows (tui:rect-rows buffer-rect))
         (cols (tui:rect-cols buffer-rect))
         (state (scroll-state buffer))
         (focus-index (location-id (lastcar (stack buffer))))
         (scroll (if (not (find focus-index (scroll-state-segments state)
                                :key #'segment-index))
                     ;; for large forward jumps, reset
                     (make-scroll-position :index focus-index)
                     (scroll-state-position state))))
    (multiple-value-bind (segments initial-viewport)
        (stage-toplevel-window buffer scroll focus-index buffer-rect)
      (let* ((height (segment-end (lastcar segments) (length forms)))
             (location (lastcar (stack buffer)))
             (root (make-instance 'ordered-view
                                  :rect (tui:make-rect :x 0 :y 0 :rows height :cols cols)
                                  :children (mapcar #'segment-view segments)
                                  :key-handler (global-key-handler forms location)
                                  :focused nil))
             (focus-view (gethash :focus-view (redisplay-cache buffer)))
             (rect (tui:rect focus-view))
             ;; y-offset relative to the staging coordinates
             (viewport (max 0 (min initial-viewport (- height rows)))))
        ;; copy segments to the screen
        (flet ((blit-segment (segment viewport)
                 (let ((start (- viewport (segment-start segment))))
                   (loop
                     for row from (max 0 start)
                       below (min (segment-rows segment) (+ rows start))
                     do (loop for col below cols
                              do (setf (aref tui::*put-buffer*
                                             (- (+ row (tui:rect-y buffer-rect)) start)
                                             (+ (tui:rect-x buffer-rect) col))
                                       (aref (segment-buffer segment) row col)))))))
          (dolist (segment segments)
            (blit-segment segment viewport)))
        (setf (scroll-state buffer)
              (make-scroll-state :position (scroll-position-at-row segments viewport)
                                 :rows height
                                 :segments segments
                                 :viewport viewport))
        ;; edit: split up setf
        (setf (focus-rect buffer) rect
              (gethash forms (node-views buffer)) root)
        root))))

;;
;;; main render loop
;;

;; using oklab
(defparameter *grayramp* #(0 7 #x16 #x20 #x2e #x3b #x48 #x50 #x63 #x71))

(defun staging-to-absolute-coordinates (rect viewport buffer-rect)
  "Transforms `rect' from staging coordinates for `buffer-rect' to absolute
screen coordinates, given the top of the buffer's visual content in staging coordinates
as `viewport'."
  (let* ((origin (tui:rect-y buffer-rect))
         (y (max origin (+ origin (- (tui:rect-y rect) viewport))))
         (bottom (+ origin (- (tui:rect-y2 rect) viewport))))
    ;; rect coordinates are unsigned; clip first
    (tui:clamp-rect
     (tui:copy-rect rect :x (+ (tui:rect-x buffer-rect) (tui:rect-x rect))
                         :y y :rows (max 0 (- bottom y)))
     buffer-rect)))

(defun node-screen-rect (buffer node)
  "Returns the clipped screen rect for `node' in `buffer'."
  (if-let (view (gethash node (node-views buffer)))
    (staging-to-absolute-coordinates
     (tui:rect view) (scroll-state-viewport (scroll-state buffer))
     (buffer-rect buffer))
    (error "no staged view corresponding to ~a" node)))

(defun draw-semantic-backgrounds (buffer view viewport buffer-rect)
  "`viewport' is relative to the `buffer's staging area and hence all rendered node views."
  (tui:view-traverse
   view
   (lambda (view)
     (when (typep view 'ast-view)
       (let* ((stack (view-stack view))
              (node (node-at (car stack)))
              (rect (staging-to-absolute-coordinates (tui:rect view) viewport buffer-rect))
              (real-stack-len (1- (length stack)))
              (i (cond
                   ((<= 0 real-stack-len (1- (length *grayramp*)))
                    (truncate (svref *grayramp* (min (1- (length *grayramp*))
                                                         real-stack-len))))
                   (t
                    (min 255 (truncate (+ (last-elt *grayramp*)
                                          (* (- real-stack-len (1- (length *grayramp*)))
                                             10))))))))
         (unless (parse:is-atom node)
           (tui:fill-rect (tui:make-style :bg (tui:color i i i))
                          rect))
         ;;
         (if (eq node (focus-binder buffer))
             (tui:fill-rect (tui:make-style :bg (tui:color #x85 #x99 #x00))
                            rect)
             (when (and (parse:lexical-symbol-ref node (car stack))
                        (not (symbol-ref-boundp node stack buffer)))
               (tui:fill-rect (tui:make-style :bg (tui:color #xcb #x4b #x16))
                              rect)))
         ;;
         (when (parse:unexpanded-macro-op-p (car stack))
           (tui:fill-rect (tui:make-style :bg (tui:color #xdc #x32 #x2f))
                          rect))))
     t)))

(defun draw-highlights (buffer view viewport buffer-rect)
  "`viewport' is the top of the buffer's visual content in staging coordinates."
  (tui:view-traverse
   view
   (lambda (view)
     (when (typep view 'ast-view)
       (let ((stack (view-stack view))
             (rect (staging-to-absolute-coordinates (tui:rect view) viewport buffer-rect)))
         (when (zipper-top-p buffer stack)
           (tui:fill-rect (tui:make-style :bg #xb58900) rect :blend 0.35))
         (when (location-selected-p buffer stack)
           (tui:fill-rect (tui:make-style :bg #x6c71c4) rect :blend 0.35))))
     t))
  (when-let (focus-rect (and (buffer-focused-p buffer) (focus-rect buffer)))
    (let ((rect (staging-to-absolute-coordinates focus-rect viewport buffer-rect)))
      (tui:fill-rect (tui:make-style :bg #xb58900) rect :blend 0.7))))

(defun buffer-outer-rect (buffer)
  "The absolute screen area available to `buffer'."
  (when (and (floating buffer) (floating-rect buffer))
    (return-from buffer-outer-rect (floating-rect buffer)))
  (let ((ui (buffer-ui buffer)))
    (tui:make-rect :x (min (buffer-x buffer) (tui:cols ui))
                   :y (min (buffer-y buffer) (tui:rows ui))
                   :rows (max 0 (- (tui:rows ui) (buffer-y buffer)))
                   :cols (max 0 (- (tui:cols ui) (buffer-x buffer))))))

(defun buffer-rect (buffer &optional (col-bound most-positive-fixnum))
  "Returns an absolute coordinate rect bounding the content viewport of `buffer'."
  (let* ((rect (buffer-outer-rect buffer))
         (inset (if (floating buffer) 1 0)))
    (tui:copy-rect rect
                   :x (+ (tui:rect-x rect) (min inset (tui:rect-cols rect)))
                   :y (+ (tui:rect-y rect) (min inset (tui:rect-rows rect)))
                   :rows (max 0 (- (tui:rect-rows rect) (* 2 inset)))
                   :cols (max 0 (min (- (tui:rect-cols rect) (* 2 inset))
                                     col-bound)))))

(defun fit-floating-buffer (buffer)
  "Fits `buffer' to its contents, keeping its border inside the screen."
  (let* ((ui (buffer-ui buffer))
         (rows (max 0 (- (tui:rows ui) 2)))
         (cols (max 0 (- (tui:cols ui) 2)))
         (height 0)
         (width 0))
    (when (and (plusp rows) (plusp cols))
      (let ((measuring (make-buffer :buffer-ui ui :buffer-id (buffer-id buffer)
                                    :buffer-state (buffer-state buffer))))
        (loop for index below (length (ast buffer))
              for segment = (render-one-toplevel index 0 (ast buffer) measuring rows cols)
              do (incf height (+ (segment-rows segment) (if (zerop index) 0 1)))
                 (setf width (max width (tui:rect-x2 (tui:rect (segment-view segment))))))))
    ;; add back border and render for real after measuring
    (let ((height (min (tui:rows ui) (+ height 2)))
          (width (min (tui:cols ui) (+ width 2))))
      (setf (floating-rect buffer)
            (tui:make-rect :x (min (buffer-x buffer) (- (tui:cols ui) width))
                           :y (min (buffer-y buffer) (- (tui:rows ui) height))
                           :rows height :cols width)))))

(defparameter *buffer-focus-border* #xb0b0b0)
(defparameter *buffer-unfocus-border* #x606060)
(defun draw-buffer-border (buffer)
  (let* ((rect (buffer-outer-rect buffer))
         (rows (tui:rect-rows rect))
         (cols (tui:rect-cols rect))
         (style (tui:make-style :fg (if (buffer-focused-p buffer)
                                        *buffer-focus-border*
                                        *buffer-unfocus-border*)
                                :bg 0)))
    (when (and (>= rows 2) (>= cols 2))
      (loop for col from 2 below cols
            do (tui:put #\─ 1 col rect style)
               (tui:put #\─ rows col rect style))
      (loop for row from 2 below rows
            do (tui:put #\│ row 1 rect style)
               (tui:put #\│ row cols rect style))
      (tui:put #\┌ 1 1 rect style)
      (tui:put #\┐ 1 cols rect style)
      (tui:put #\└ rows 1 rect style)
      (tui:put #\┘ rows cols rect style))))

(defmethod draw-buffer (buffer (state buffer-state))
  (when (floating buffer)
    (fit-floating-buffer buffer)
    (draw-buffer-border buffer))
  (let ((buffer-rect (buffer-rect buffer)))
    (when (and (plusp (tui:rect-rows buffer-rect))
               (plusp (tui:rect-cols buffer-rect)))
      (tui:fill-rect (tui:make-style :bg #x0) buffer-rect
                     :char #\space)
      (let* ((content (render-toplevel-window buffer buffer-rect))
             (offset (scroll-state-viewport (scroll-state buffer))))
        (draw-semantic-backgrounds buffer content offset buffer-rect)
        (draw-highlights buffer content offset buffer-rect)
        (setf (buffer-view buffer) content)))))

(defparameter *debugger-entries* 10)
(defparameter *max-debugger-width* 60)
(defun debugger-text (printer)
  "Captures `printer' output without recursively debugging printing failures."
  (handler-case
      (let ((*print-circle* t)
            (*print-level* 5)
            (*print-length* 20))
        (substitute #\space #\newline (with-output-to-string (out)
                                        (funcall printer out))))
    (error () "<error printing debugger entry>")
    (slynk-backend:sly-db-condition () "<unavailable debugger entry>")))

(defun restart-depth (restart state)
  "Returns the oldest debugger depth containing `restart' in `state'."
  (loop for captured in (debug-restarts state)
        for level downfrom (length (debug-conditions state))
        when (member restart captured :test #'eq)
          minimize level))

(defmethod draw-buffer (buffer (state debug-state))
  (let* ((ui (buffer-ui buffer))
         (condition (first (debug-conditions state)))
         (restarts (first (debug-restarts state)))
         (lines (append
                 (list (debugger-text
                        (lambda (out) (slynk-backend:print-condition condition out)))
                       "Restarts:")
                 (loop for restart in restarts for i from 0
                       collect (debugger-text
                                (lambda (out)
                                  (format out "~d [depth ~d]: ~a"
                                          i (restart-depth restart state) restart))))
                 (list "Backtrace:")
                 (loop for frame in (slynk-backend:compute-backtrace 0 *debugger-entries*)
                       for i from 0
                       collect (debugger-text
                                (lambda (out)
                                  (format out "~d: ~a" i (slynk::frame-to-string frame)))))))
         (width (min *max-debugger-width* (tui:cols ui)))
         (height (min (+ 2 (length lines)) (tui:rows ui))))
    (setf (floating-rect buffer)
          (tui:make-rect :x (min (buffer-x buffer) (- (tui:cols ui) width))
                         :y (min (buffer-y buffer) (- (tui:rows ui) height))
                         :rows height :cols width))
    (let* ((rect (buffer-rect buffer))
           (children nil))
      (tui:fill-rect (tui:make-style :fg #xeeeeee :bg 0)
                     (floating-rect buffer) :char #\space)
      (draw-buffer-border buffer)
      (loop for text in lines for row from 0 below (tui:rect-rows rect)
            do (tui:puts* text (1+ row) 1 rect)
               (when (<= 2 row (1+ (length restarts)))
                 (let* ((restart (nth (- row 2) restarts))
                        (bounds (tui:make-rect :x 0 :y row :rows 1
                                               :cols (tui:rect-cols rect))))
                   (push (make-instance
                          'tui:view :rect bounds
                          :mouse-handler
                          (lambda (view event)
                            (let ((mouse (tui:event-kind event)))
                              (when (and (eq (tui:mouse-data-state mouse) :click)
                                         (eq (tui:mouse-data-button mouse) :left)
                                         (tui:mouse-within mouse (tui:rect view)))
                                (invoke-restart-interactively restart)))))
                         children))))
      (setf (buffer-view buffer)
            (make-instance
             'tui:view
             :rect (tui:make-rect :x 0 :y 0 :rows (tui:rect-rows rect)
                                  :cols (tui:rect-cols rect))
             :children (nreverse children) :focused t
             :key-handler
             (lambda (view event working)
               (declare (ignore view working))
               (when (is-regular-char-event event)
                 (when-let* ((index (digit-char-p (tui:event-kind event)))
                             (restart (nth index restarts)))
                   (invoke-restart-interactively restart)))))))))

(defun render-buffer (buffer)
  (let* ((old (render-state buffer))
         (working (make-buffer
                   :buffer-ui (buffer-ui buffer)
                   :buffer-id (buffer-id buffer)
                   :buffer-state (buffer-state buffer)
                   :render-state (make-render-state
                                  :stage-cache (copy-hash-table (stage-cache old))
                                  :scroll-state (scroll-state old)))))
    (draw-buffer working (buffer-state working))
    ;; commit only after success, old version can be used recursively by debugging
    (setf (render-state buffer) (render-state working))))

(defmethod tui:render :around ((ui ui))
  ;; a recursive redisplay needs to own its scratch space
  (let ((*measurement-scratch* (make-array '(0 0) :adjustable t))
        (*render-environments* (make-hash-table :test #'eq)))
    (call-next-method)))

(defmethod tui:render ((ui ui))
  (tui::clear-buffer tui::*put-buffer*)
  (dolist (buffer (buffers ui))
    (render-buffer buffer))
  ;; completion is global
  (let ((completion (when-let (buffer (focused-buffer ui))
                      (when (and (buffer-view buffer)
                                 (typep (buffer-state buffer) 'buffer-state))
                        (render-completion-window buffer)))))
    (make-instance 'tui:view :rect (tui::screen-rect)
                             :children (when completion (list completion)))))

;; note: must go below definitions, next to tui:redisplay
(let ((render-node-methods nil)
      (draw-buffer-methods nil)
      (old-render-around-method nil)
      (old-render-method nil)
      (old-redisplay-method nil)
      (fnames '(render-toplevel-window render-buffer draw-buffer-border
                debugger-text restart-depth enter-debugger-loop
                measure-node
                function-call-indentation
                ref-list-indentation symbol-ref-text render-literal-text render-literal-line
                render-delimiter render-symbol-space render-elements
                render-layout-slot slot-render-keys
                macro-call-indentation setf-indentation loop-indentation
                draw-semantic-backgrounds draw-highlights
                render-completion-window render-completion))
      (funs (make-hash-table :test 'eq)))

  (defun save-ui ()
    (setf draw-buffer-methods (copy-list (closer-mop:generic-function-methods #'draw-buffer)))
    (setf old-render-around-method (find-method #'tui:render '(:around) '(ui)))
    (setf render-node-methods (closer-mop:generic-function-methods #'render-node))
    (setf old-redisplay-method (find-method #'tui:redisplay '(:around) '(ui)))
    (setf old-render-method (find-method #'tui:render nil '(ui)))
    (loop for name in fnames
          do (setf (gethash name funs) (fdefinition name))))

  (defun reset-ui ()
    (dolist (method (copy-list (closer-mop:generic-function-methods #'draw-buffer)))
      (remove-method #'draw-buffer method))
    (dolist (method draw-buffer-methods)
      (add-method #'draw-buffer method))
    (when-let (method (find-method #'tui:render '(:around) '(ui) nil))
      (remove-method #'tui:render method))
    (add-method #'tui:render old-render-around-method)
    (remove-method #'tui:render (find-method #'tui:render nil '(ui)))
    (add-method #'tui:render old-render-method)
    (remove-method #'tui:redisplay (find-method #'tui:redisplay '(:around) '(ui)))
    (add-method #'tui:redisplay old-redisplay-method)
    (loop for method in (closer-mop:generic-function-methods #'render-node)
          do (remove-method #'render-node method))
    (loop for method in render-node-methods
          do (add-method #'render-node method))
    (loop for name in fnames
          do (setf (fdefinition name) (gethash name funs)))))

(defun enter-debugger-loop (ui condition)
  (slog* (debugger-text
          (lambda (out) (format out "entering-recursive-loop on ~s" condition))))
  (incf (revision ui))
  (let ((debugger (find-if #'debug-state-p (buffers ui) :key #'buffer-state))
        (previous-focus (focused-buffer ui)))
    (unless debugger
      (let ((buffer (make-buffer
                     :buffer-ui ui
                     :buffer-state (make-instance 'debug-state
                                                  :buffer-x 10
                                                  :buffer-y 10))))
        (appendf (buffers ui) (list buffer))
        (setf debugger buffer)))
    (push condition (debug-conditions debugger))
    ;; note: restart tests may error
    (push (ignore-errors (compute-restarts condition)) (debug-restarts debugger))
    (setf (focused-buffer ui) debugger
          (redisplayed-since-input ui) nil)
    (unwind-protect
         (slynk-backend:call-with-debugging-environment
          (lambda () (tui:run-loop ui)))
      (pop (debug-conditions debugger))
      (pop (debug-restarts debugger))
      (when (eq (focused-buffer ui) debugger)
        (setf (focused-buffer ui)
              (if (member previous-focus (buffers ui))
                  previous-focus
                  (find-if (lambda (buffer) (not (eq buffer debugger))) (buffers ui)))))
      (setf (redisplayed-since-input ui) nil)
      (when (null (debug-conditions debugger))
        (removef (buffers ui) debugger)))))

;; TODO consider continue restarts
(let ((redisplay-failed nil))
  (defmethod tui:redisplay :around ((ui ui))
    (slynk-backend:call-with-debugger-hook
     (lambda (condition previous-hook)
       (declare (ignore previous-hook))
       (reset-ui)
       (if redisplay-failed
           (progn (slog 'fallback-redisplay-failed) (tui:stop ui))
           (progn
             (setf redisplay-failed t)
             (enter-debugger-loop ui condition))))
     (lambda ()
       (unless (redisplayed-since-input ui)
         (with-timing (lambda (time)
                        (slog* (format nil "~aRedisplay done in ~Ams"
                                       (make-string 70 :initial-element #\-)
                                       (truncate time 1000))))
           (call-next-method))
         (setf redisplay-failed nil))))))

(defun buffer-at-point (ui row col)
  "Finds the *last drawn* buffer containing the absolute point `row', `col' (1-based)."
  (find-if (lambda (buffer)
             (when-let (view (buffer-view buffer))
               (tui:rect-has-point-p
                (if (floating buffer)
                    (buffer-outer-rect buffer)
                    (staging-to-absolute-coordinates
                     (tui:rect view) (scroll-state-viewport (scroll-state buffer))
                     (buffer-rect buffer)))
                row col)))
           (buffers ui)
           :from-end t))

(defrecord ui-effects ()
  ((pane-drag :init nil :type (or null pane-drag))
   (selection :type (or null selection))
   (zipper :type (or null zipper))
   (cutbuffer :type (or null cutbuffer))
   (focused-buffer)
   (new-buffers :init nil :type list)
   (closed-buffers :init nil :type list))
  (:representation :class))

(defun handle-pane-drag (ui event target)
  "Moves a alt-clicked buffer until mouse release, consuming the associated events."
  (when (tui:mouse-event-p event)
    (let* ((mouse (tui:event-kind event))
           (state (tui:mouse-data-state mouse))
           (drag (pane-drag ui)))
      (cond
        ((eq state :release)
         (setf (pane-drag ui) nil)
         (not (null drag)))
        (drag
         (when (eq state :drag)
           (when-let (buffer (find (pane-drag-buffer-id drag) (buffers ui) :key #'buffer-id))
             (let ((rect (buffer-outer-rect buffer)))
               (setf (buffer-x buffer)
                     (max 0 (min (- (tui:mouse-data-col mouse) (pane-drag-offset-x drag))
                                 (- (tui:cols ui)
                                    (if (floating buffer) (tui:rect-cols rect) 1)))))
               (setf (buffer-y buffer)
                     (max 0 (min (- (tui:mouse-data-row mouse) (pane-drag-offset-y drag))
                                 (- (tui:rows ui)
                                    (if (floating buffer) (tui:rect-rows rect) 1))))))))
         t)
        ((and target (eq state :click) (eq (tui:mouse-data-button mouse) :left)
              (tui:event-altp event))
         (let ((rect (buffer-outer-rect target)))
           (setf (focused-buffer ui) target)
           (setf (pane-drag ui)
                 (make-pane-drag
                  :buffer-id (buffer-id target)
                  :offset-x (- (tui:mouse-data-col mouse) (tui:rect-x rect))
                  :offset-y (- (tui:mouse-data-row mouse) (tui:rect-y rect)))))
         t)))))

(defun dispatch-ui-event (ui event)
  (when-let (handler (gethash event *ui-key-handlers*))
    (funcall handler ui)
    (return-from dispatch-ui-event))
  (let* ((mouse (when (tui:mouse-event-p event) (tui:event-kind event)))
         ;; currently only completion
         (overlay (when (and mouse (tui:root-view ui))
                    (find-if (lambda (view)
                               (tui:mouse-within mouse (tui:rect view)))
                             (tui:children (tui:root-view ui)))))
         (target (when mouse
                   (buffer-at-point ui (tui:mouse-data-row mouse)
                                    (tui:mouse-data-col mouse)))))
    (when (handle-pane-drag ui event (unless overlay target))
      (return-from dispatch-ui-event))
    ;; completion should capture events early
    (when overlay
      (tui:view-traverse overlay (lambda (view)
                                   (funcall (tui:mouse-handler view) view event)
                                   t))
      (return-from dispatch-ui-event))
    ;;
    (when (and target (eq (tui:mouse-data-state mouse) :click)
               (member (tui:mouse-data-button mouse) '(:left :middle :right)))
      (setf (focused-buffer ui) target))
    ;;
    (when-let* ((buffer (focused-buffer ui))
                (view (buffer-view buffer)))
      (when (or (not mouse) (eq target buffer))
        (let ((event
                (if mouse
                    (let ((data (copy-structure mouse))
                          (rect (buffer-rect buffer)))
                      (incf (tui:mouse-data-row data)
                            (- (scroll-state-viewport (scroll-state buffer))
                               (tui:rect-y rect)))
                      (decf (tui:mouse-data-col data) (tui:rect-x rect))
                      (tui-sys:make-event :kind data
                                          :shiftp (tui:event-shiftp event)
                                          :altp (tui:event-altp event)
                                          :controlp (tui:event-controlp event)
                                          :metap (tui:event-metap event)))
                    event)))
          (tui:view-traverse
           view (lambda (view)
                  (if mouse
                      (funcall (tui:mouse-handler view) view event)
                      (when (tui:focused view)
                        (funcall (tui:key-handler view) view event buffer)))
                  t)))))))

(defun handle-ui-event (ui event)
  "Returns working buffers and global effects without changing the published `ui'."
  (let* ((working (shallow-copy-record ui))
         (originals (buffers ui))
         (copies (mapcar (lambda (buffer)
                           (make-buffer :buffer-ui working :buffer-id (buffer-id buffer)
                                        :buffer-state
                                        (copy-buffer-state (buffer-state buffer))
                                        :render-state (render-state buffer)))
                         originals)))
    (setf (buffers working) copies
          (slot-value working 'focused-buffer) (when-let (focused (focused-buffer ui))
                                                (find (buffer-id focused) copies
                                                      :key #'buffer-id)))
    (dispatch-ui-event working event)
    (let ((new (mapcar (lambda (buffer)
                         (make-buffer :buffer-ui ui :buffer-id (buffer-id buffer)
                                      :buffer-state (buffer-state buffer)))
                       (remove-if (lambda (buffer) (member buffer copies)) (buffers working)))))
      (values copies
              (make-ui-effects
               :selection (selection working) :zipper (zipper working)
               :cutbuffer (cutbuffer working)
               :pane-drag (pane-drag working) :new-buffers new
               :closed-buffers (loop for copy in copies
                                     unless (member copy (buffers working))
                                       collect (buffer-id copy))
               :focused-buffer (when-let (focused (focused-buffer working))
                                 (find (buffer-id focused) (append originals new)
                                       :key #'buffer-id)))))))

(defmethod tui:dispatch-event ((ui ui) event)
  (let ((revision (revision ui)))
    (multiple-value-bind (copies effects)
        (handle-ui-event ui event)
      ;; This is necessary to handle reentrancy as recursive dispatch may
      ;; already have published a newer state, in which case we don't commit
      (when (= revision (revision ui))
        (loop for buffer in (buffers ui) for copy in copies
              do (setf (buffer-state buffer) (buffer-state copy)))
        (setf (buffers ui) (append (remove-if (lambda (buffer)
                                           (member (buffer-id buffer)
                                                   (closed-buffers effects)))
                                         (buffers ui))
                                  (new-buffers effects)))
        (setf (pane-drag ui) (pane-drag effects)
              (selection ui) (selection effects)
              (zipper ui) (zipper effects)
              (cutbuffer ui) (cutbuffer effects)
              (slot-value ui 'focused-buffer) (focused-buffer effects))
        (incf (revision ui))))))

(defmethod tui:dispatch-event :around ((ui ui) event)
  (with-simple-restart (abort "ignore event-handling error")
    (if (and (not (tui:mouse-event-p event))
             (equal (tui:event-kind event) #\q)
             (tui:event-controlp event))
        (progn (tui:stop ui)) ; XXX currently doesn't exit recursively
        (slynk-backend:call-with-debugger-hook
         (lambda (condition previous-hook)
           (declare (ignore previous-hook))
           (enter-debugger-loop ui condition))
         (lambda () (call-next-method))))
    (unless (and (tui:mouse-event-p event)
                 (eq :hover (tui:mouse-data-button (tui:event-kind event))))
      (slog* (format nil "~a~a" (make-string 70 :initial-element #\-) 'event-handled)))))

(defmethod tui:dispatch-event :after ((ui ui) event)
  (declare (ignore event))
  (setf (redisplayed-since-input ui) nil)
  (tui:redisplay ui)
  (setf (redisplayed-since-input ui) t))

(defmethod tui:handle-resize :after ((ui ui))
  (setf (redisplayed-since-input ui) nil))
