(defpackage #:weave-serialization
  (:use #:cl #:alexandria-2 #:weave-utils #:weave-parser #:weave-indentation)
  (:import-from #:weave-parser
                #:anchor
                #:leading
                #:trailing
                #:inside
                #:comment
                #:kind
                #:read-cond
                #:flags
                #:input-prefix
                #:dot-marker
                #:label-ref
                #:label-def
                #:labeled
                #:rank
                #:funcode-syntax)
  (:export #:serialize
           #:serialize-node
           #:serialize-file
           #:save-file-checked
           #:*long-call-indent*))

(in-package #:weave-serialization)

(defparameter *long-call-indent* 1
  "Indentation from the opening parenthesis after a long call's first argument.")

(defvar *comment-ended-line* nil)

(defun serialize (forms stream &key (package *package*) (column-limit 92))
  "Writes top-level `forms' to `stream', resolving names relative to `package'."
  (let ((*package* (find-package package))
        (*print-right-margin* column-limit)
        (*print-pretty* t)
        (*print-miser-width* nil)
        (*print-case* :downcase)
        (*print-level* nil)
        (*print-length* nil)
        (*comment-ended-line* t))
    (loop for node in forms
          for firstp = t then nil
          do (unless firstp
               (terpri stream))
             (pprint-logical-block (stream nil)
               (serialize-node node stream))
             (fresh-line stream)
             (setf *comment-ended-line* t)
             (when (and (typep node 'macro-call) (eq (resolve (op node)) 'in-package))
               (let* ((argument (first (body node)))
                      (name (typecase argument
                              (symbol-ref (name argument))
                              (literal
                               (let ((*read-eval* nil))
                                 (read-from-string (str argument)))))))
                 (setf *package*
                       (or (find-package name) (error "Unknown package ~s" name))))))))

(defun serialize-file
  (forms pathname &key (package *package*) (column-limit 92) (if-exists :error))
  "Writes `forms' to `pathname'. If the file exists, `if-exists' will error by default."
  (with-open-file (stream pathname :direction :output :if-exists if-exists
                          :if-does-not-exist :create :external-format :utf-8)
    (serialize forms stream :package package :column-limit column-limit))
  pathname)

(defgeneric serialize-node (node stream)
  (:documentation "Writes `node' as lisp source to `stream'."))

(defmethod serialize-node :around ((node anchor) stream)
  (dolist (comment (leading node))
    (serialize-node comment stream)
    (unless (and (typep comment 'comment) (eq (kind comment) :line))
      (pprint-newline :mandatory stream)))
  (setf *comment-ended-line* nil)
  (call-next-method)
  (dolist (comment (trailing node))
    (unless *comment-ended-line*
      (write-char #\Space stream))
    (serialize-node comment stream)))

(defun own-line-comment-p (node)
  (and (typep node 'comment)
       (eq (kind node) :line)
       (not (weave-parser::single-semicolon-comment-p node))))

(defmethod serialize-node ((node comment) stream)
  (when (and (own-line-comment-p node) (not *comment-ended-line*))
    (pprint-newline :mandatory stream))
  (write-string (str node) stream)
  (when (eq (kind node) :line)
    (pprint-newline :mandatory stream)
    (setf *comment-ended-line* t)))

(defmethod serialize-node ((node read-cond) stream)
  (if (weave-parser::applied-p node)
      (progn
        (format stream "#~a" (kind node))
        (serialize-node (flags node) stream)
        (write-char #\Space stream))
      (write-string (str node) stream)))

(defmethod serialize-node ((node hole) stream)
  (write-string "#:hole" stream))

(defmethod serialize-node ((node literal) stream)
  (write-string (str node) stream))

(defmethod serialize-node ((node ratio-literal) stream)
  (if (or (typep (numerator-node node) 'hole) (typep (denominator-node node) 'hole))
      (write-string "#:hole" stream)
      (write-string (str node) stream)))

(defun serialize-token (name stream)
  (let ((*print-gensym* nil)
        (*print-escape* t))
    (write (make-symbol name) :stream stream)))

(defmethod serialize-node ((node symbol-ref) stream)
  (let* ((home (home-package node))
         (visible (and home
                       (multiple-value-bind (original present)
                           (find-symbol (name node) home)
                         (multiple-value-bind (accessible found)
                             (find-symbol (name node) *package*)
                           (and present found (eq original accessible)))))))
    (cond
      ((null home) (write-string "#:" stream))
      ((eq home (find-package :keyword)) (write-char #\: stream))
      ((or (eq home *package*) visible))
      (t
       (serialize-token
         (or (car (rassoc home (slynk-backend:package-local-nicknames *package*)))
             (first (stable-sort
                      (cons (package-name home) (copy-list (package-nicknames home)))
                     #'<
                     :key #'length)))
        stream)
       (write-string
         (if (eq :external (nth-value 1 (find-symbol (name node) home))) ":" "::")
        stream)))
    (serialize-token (name node) stream)))

(defmethod serialize-node ((node dot-marker) stream)
  (write-char #\. stream))

(defmethod serialize-node ((node label-ref) stream)
  (format stream "#~a#" (name node)))

(defmethod serialize-node ((node label-def) stream)
  (format stream "#~a=" (name node))
  (serialize-node (labeled node) stream))

(defun serialization-prefix (node)
  (when (typep node 'reader-marker)
    (cond
      ((some (lambda (s)
               (string-and-package= node s))
             '(quote read-quote))
       "'")
      ((some (lambda (s)
               (string-and-package= node s))
             '(function read-function))
       "#'")
      ((some (lambda (s)
               (string-and-package= node s))
             '(eclector.reader:quasiquote read-quasiquote))
       "`")
      ((string-and-package= node 'eclector.reader:unquote) ",")
      ((string-and-package= node 'eclector.reader:unquote-splicing) ",@")
      ((string-and-package= node 'read-eval) "#."))))

(defvar *serialization-rule* nil)

(defun serialize-with-rule (node stream rule)
  (let ((*serialization-rule* rule))
    (serialize-node node stream)))

(defun call-keyword-start (elements)
  (let ((head (first elements)))
    (when (typep head 'symbol-ref)
      (let ((symbol (resolve head)))
        (or (operator-section-start symbol '(&key))
            (when (and (eq symbol 'apply) (> (length elements) 3))
              ;; apply's final argument cannot be a keyword
              (position-if
                (lambda (node)
                  (and (typep node 'symbol-ref)
                       (eq (home-package node) (find-package :keyword))))
               (rest elements)
               :start 1
               :end (- (length elements) 2))))))))

(defmethod serialize-node ((node ref-list) stream)
  (let* ((elements (elements node))
         (prefix (and (= (length elements) 2) (serialization-prefix (first elements))))
         (rule (or *serialization-rule*
                   (and (eq (kind node) :list) (indent-rule elements))))
         (*serialization-rule* nil)
         (data (eq rule :data))
         (spec (unless data
                 rule))
         (lambdap (eq (getf spec :style) :lambda))
         (loop-rows (when (and (not rule)
                               (typep (first elements) 'symbol-ref)
                               (eq (resolve (first elements)) 'loop))
                      (loop-indentation elements)))
         (row-left (cdar loop-rows))
         (loop-offset (if (and loop-rows (find-loop-keyword (second elements))) 4 0))
         (keyword-start (unless rule
                          (call-keyword-start elements)))
         (listp (or data
                    lambdap
                    (eq (getf spec :style) :list)
                    (not (eq (kind node) :list))))
         (header (getf spec :header))
         (body-start (and header (1+ header)))
         (body-break (if (getf spec :compact-body)
                         (if (= (length elements) (+ header 2)) :linear :mandatory)
                         (or (getf spec :body-break) :mandatory)))
         (long-call-p (and (not listp)
                           (null header)
                           (not loop-rows)
                           (not (getf spec :break))
                           (typep (first elements) 'symbol-ref)
                           (>= (length (name (first elements))) 10))))
    (cond
      (prefix
       (write-string prefix stream)
       (serialize-with-rule (second elements) stream (if (equal prefix "'") :data nil)))
      ((eq (kind node) :array)
       (format stream "#~dA" (rank node))
       (serialize-node (first elements) stream))
      (t
       (pprint-logical-block (stream nil :prefix (if (eq (kind node) :vector) "#(" "(")
                                     :suffix ")")
         (pprint-indent :block
          (cond
            (listp 0)
            ((and long-call-p (eql keyword-start 0)) (1- *long-call-indent*))
            (t 1))
          stream)
         (loop with previous = nil
               for element in elements
               for index from 0
               for comment-break = (and (typep element 'anchor)
                                        (own-line-comment-p (first (leading element))))
               for lambda-key = (and lambdap
                                     (typep element 'symbol-ref)
                                     (member (resolve element) lambda-list-keywords))
               for operand = (if listp index (1- index))
               for bodyp = (and body-start (>= index body-start))
               for child-rule = (weave-indentation::child-rule rule index)
               do (when (plusp index)
                    (unless *comment-ended-line*
                      (write-char #\Space stream))
                    (when (and (= index 1)
                               (not listp)
                               (null header)
                               (not loop-rows)
                               (typep (first elements) 'symbol-ref))
                      (when (and long-call-p
                                 (not (eql keyword-start 0))
                                 (not comment-break))
                        (pprint-newline :fill stream))
                      (unless long-call-p
                        (pprint-indent :current 0 stream)))
                    (when (and long-call-p (>= index 2))
                      (pprint-indent :block (1- *long-call-indent*) stream))
                    (when bodyp
                      (pprint-indent :block
                       (if (and (eql header 0) (getf spec :compact-body)) 0 1) stream))
                    (when (eql index (getf spec :argument-break))
                      (pprint-indent :block 3 stream))
                    (when lambdap
                      (cond
                        (lambda-key (pprint-indent :block 0 stream))
                        ((and (typep previous 'symbol-ref)
                              (member (resolve previous) lambda-list-keywords))
                         (pprint-indent :current 0 stream))))
                    (when (and loop-rows (zerop row-left))
                      (pop loop-rows)
                      (setf row-left (cdar loop-rows))
                      (pprint-indent :block (+ (caar loop-rows) loop-offset) stream)
                      (unless (and (= index 1) (plusp loop-offset))
                        (unless (or *comment-ended-line* comment-break)
                          (pprint-newline :mandatory stream))))
                    (unless (or *comment-ended-line*
                                comment-break
                                loop-rows
                                (and keyword-start
                                     (> operand keyword-start)
                                     (oddp (- operand keyword-start))
                                     (not (and (= index (1- (length elements)))
                                               (typep (first elements) 'symbol-ref)
                                               (eq (resolve (first elements)) 'apply))))
                                (and lambdap
                                     (typep previous 'symbol-ref)
                                     (member (resolve previous) lambda-list-keywords))
                                (and (not listp)
                                     (not bodyp)
                                     (= index 1)
                                     (not (eql keyword-start 0))))
                      (pprint-newline
                        (cond
                          ((eql index (getf spec :argument-break)) :mandatory)
                          ((and keyword-start (>= operand keyword-start)) :linear)
                          ((and long-call-p (= index 2)) :linear)
                          (lambda-key :linear)
                          (bodyp body-break)
                          ((and (getf spec :pairs) (> index 1) (oddp index)) :linear)
                          ((getf spec :pairs) :fill)
                          (t (or (getf spec :break) :fill)))
                       stream)))
                  (serialize-with-rule element stream child-rule)
                  (when loop-rows
                    (decf row-left))
                  (setf previous element))
         (loop for comment in (inside node)
               for firstp = t then nil
               do (unless firstp
                    (write-char #\Space stream))
                  (serialize-node comment stream)))))))

(defmethod serialize-node :after ((node ref-list) stream)
  (declare (ignore stream))
  (setf *comment-ended-line* nil))

(defmethod serialize-node ((node function-call) stream)
  (serialize-node (apply #'ref-list (name node) (body node)) stream))

(defmethod serialize-node ((node irregular-form) stream)
  ;; the original node's anchors are handled by the around method
  (let ((syntax (to-syntax node)))
    (setf (leading syntax) nil (trailing syntax) nil)
    (serialize-node syntax stream)))

(defmethod serialize-node ((node function-code) stream)
  (serialize-node (apply #'ref-list (funcode-syntax node)) stream))

(defmethod serialize-node ((node cons) stream)
  (serialize-node (apply #'ref-list node) stream))

(defmethod serialize-node ((node symbol) stream)
  (write node :stream stream :escape t))

(defmethod serialize-node ((node string) stream)
  (write node :stream stream :escape t))

(defun conditional-key (node)
  (labels ((feature (value)
             (typecase value
               (symbol (list :symbol (symbol-name value) (symbol-package value)))
               (cons (mapcar #'feature value))
               (symbol-ref (list :symbol (name value) (home-package value)))
               (t (syntax-key value)))))
    (list :conditional (kind node) (feature (flags node)) (weave-parser::applied-p node)
          (unless (weave-parser::applied-p node)
            (str node)))))

(defun syntax-key (node)
  (let ((syntax (to-syntax node)))
    (list (syntax-content-key syntax)
          (when (typep syntax 'anchor)
            (mapcar (lambda (comments)
                      (mapcar #'syntax-key comments))
                    (list (leading syntax) (trailing syntax) (inside syntax)))))))

(defun syntax-content-key (node)
  "Normalizes reader syntax for checking a serialized AST."
  (let ((syntax (to-syntax node)))
    (typecase syntax
      (read-cond (conditional-key syntax))
      (comment (list :comment (kind syntax) (str syntax)))
      (null nil)
      (cons (mapcar #'syntax-key syntax))
      (ref-list
       (list :list (kind syntax)
             (when (eq (kind syntax) :array)
               (rank syntax))
             (mapcar #'syntax-key (elements syntax))))
      (symbol-ref
       (list (if (typep syntax 'reader-marker) :marker :symbol) (name syntax)
             (home-package syntax)))
      (hole '(:symbol "HOLE" nil))
      (literal (list :literal (str syntax)))
      (label-ref (list :label-ref (name syntax)))
      (label-def (list :label-def (name syntax) (syntax-key (labeled syntax))))
      (t (error "Cannot verify syntax of ~s" syntax)))))

(defun save-file-checked
  (forms pathname parse &key (package :cl-user) (column-limit 92) original check-only)
  "Checks and saves `forms', using `parse' to verify the serialized AST.
`original' supplies source as a string, otherwise the file is read before serialization."
  (let* ((*readtable* (copy-readtable nil))
         (*package* (find-package package))
         (original (or original (uiop/stream:read-file-string pathname)))
         (formatted (with-output-to-string (stream)
                      (serialize forms stream
                                 :package package
                                 :column-limit column-limit)))
         (temporary (make-pathname
                     :name (format nil ".~a-~a" (pathname-name pathname) (gensym "SAVE"))
                     :type "tmp"
                     :defaults pathname)))
    (unless (equal (syntax-key forms) (syntax-key (funcall parse formatted)))
      (error "AST equivalence failed for ~a" pathname))
    (unwind-protect (progn
                      (with-open-file (stream temporary :direction :output :if-exists
                                              :error :external-format :utf-8)
                        (write-string formatted stream))
                      (unless (string= original (uiop/stream:read-file-string pathname))
                        (error "Source changed during saving: ~a" pathname))
                      (unless check-only
                        (uiop/filesystem:rename-file-overwriting-target temporary
                         pathname)))
      (when (probe-file temporary)
        (delete-file temporary)))
    pathname))
