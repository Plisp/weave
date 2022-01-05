;;; C language support

(defpackage #:weave-c
  (:use :cl))
(in-package #:weave-c)

;;; lexer

(defvar *line-number*)

(defun lexer-read-char (in)
  (declare (optimize (speed 3) (safety 1))
           (type stream in))
  (let ((c (read-char in nil)))
    (declare (type (or null character) c))
    (when (eql c #\Newline)
      (incf *line-number*))
    c))

(defun lexer-unread-char (c in)
  (declare (type character c)
           (type stream in))
  (when (eql c #\Newline)
    (decf *line-number*))
  (unread-char c in))

(defun lexer-read-line (in)
  (coerce (loop for c = (lexer-read-char in)
                while (and c (char/= c #\Newline))
                collect c)
          'simple-string))

(defvar *comments* (list))

(defparameter *c-keywords*
  '("sizeof"
    "typedef" "extern" "static" "auto" "register"
    "void" "char" "short" "int" "long" "float" "double" "signed" "unsigned"
    "_Bool" "_Complex" "_Imaginary" "__builtin_va_list"
    "struct" "union" "enum"
    "const" "__const" "restrict" "__restrict" "volatile"
    "inline" "__inline" "__inline__"
    "case" "default" "if" "else" "switch" "while" "do" "for"
    "goto" "continue" "break" "return"
    "asm" "__asm__"))

(defparameter *cpp-directives* '("define" "undef" "include" "line" "error" "pragma"
                                 "if" "ifdef" "ifndef" "else" "elif" "endif"))

(defun string-to-symbol (s)
  (declare (simple-string s))
  (if (some #'upper-case-p s)
      (intern s 'weave-c)
      (intern (string-upcase s) 'weave-c)))

(defun skip-whitespace (in &optional c)
  (declare (optimize (speed 3) (safety 1))
           (type (or character null) c))
  (let ((c (or c (lexer-read-char in))))
    (declare (type (or null character) c))
    (loop while (member c '(#\Space #\Tab))
          do (setf c (lexer-read-char in))
          finally (return c))))

(defun get-numeric-token (in c)
  (declare (character c))
  (labels
      ((decimal-digit-p (c) (char<= #\0 c #\9))
       (octal-digit-p (c) (char<= #\0 c #\7))
       (hex-digit-p (c) (or (decimal-digit-p c)
                            (char<= #\A c #\Z)
                            (char<= #\a c #\z)))
       (digit-p (c radix)
         (cond ((= radix 10) (decimal-digit-p c))
               ((= radix 8) (octal-digit-p c))
               ((= radix 16) (hex-digit-p c))
               (t (error "Unexpected radix ~A" radix))))
       (digit-value (c)
         (cond
           ((decimal-digit-p c)
            (- (char-code c) (char-code #\0)))
           ((char<= #\A c #\Z) (+ 10 (- (char-code c) (char-code #\A))))
           ((char<= #\a c #\z) (+ 10 (- (char-code c) (char-code #\a))))
           (t (error "Unexpected character ~A" c))))
       (parse-integer-suffix (value c)
         (cond
           ((null c) value)
           ((member c '(#\u #\U))
            (parse-integer-suffix (cons :unsigned c) (lexer-read-char in)))
           ((member c '(#\l #\L))
            (parse-integer-suffix (cons :long c) (lexer-read-char in)))
           (t (lexer-unread-char c in) value)))
       (parse-float (value c)
         (cond
           ((member c '(#\e #\E #\d #\D)) (parse-float-exponent value c))
           ((eql c #\.) (parse-float-fraction value c))
           (t (error "Unexpected character ~A" c))))
       (parse-float-exponent (value c)
         (let ((float-type (member c '(#\e #\E)))
               (exp 0))
           (loop
             (setf c (lexer-read-char in))
             (unless (char<= #\0 c #\9)
               (unless (null c) (setf c (lexer-read-char in)))
               (setf value (* value (expt 10 exp)))
               (return-from parse-float-exponent
                 (if float-type (cons :float value) value)))
             (setf exp (+ (- (char-code c) (char-code #\0))
                          (* 10 exp))))))
       (parse-float-fraction (value c)
         (let ((mult 0.1)
               (value (coerce value 'float)))
           (loop
             (setf c (lexer-read-char in))
             (unless (char<= #\0 c #\9)
               (return-from parse-float-fraction
                 (cond
                   ((member c '(#\e #\E #\d #\D))
                    (parse-float-exponent value c))
                   (t (unless (null c) (lexer-unread-char c in))
                      value))))
             (setf value (+ value (* mult (- (char-code c) (char-code #\0))))
                   mult (/ mult 10.0))))))
    (let ((value 0) (radix 10))
      (cond
        ((eql c #\0)
         (setf radix 8)
         (setf c (lexer-read-char in))
         (cond
           ((member c '(#\x #\X))
            (setf radix 16)
            (setf c (lexer-read-char in)))
           ((octal-digit-p c) nil)
           ((member c '(#\. #\e #\E #\d #\D)) (setf radix 10))
           (t (return-from get-numeric-token (parse-integer-suffix 0 c))))))
      (loop
        (cond
          ((digit-p c radix) (setf value (+ (* radix value) (digit-value c))))
          ((member c '(#\. #\e #\E #\d #\D))
           (unless (= 10 radix) (error "Unexpected character ~A" c))
           (return-from get-numeric-token (parse-float value c)))
          (t (return-from get-numeric-token (parse-integer-suffix value c))))
        (setf c (lexer-read-char in))))))

(defun get-string-token (in c)
  (let ((string (make-array 20 :element-type 'character
                               :fill-pointer 0 :adjustable t)))
    (when (member c '(#\l #\L))
      (vector-push-extend c string)
      (setf c (lexer-read-char in)))
    (unless (member c '(#\" #\'))
      (error "Unexpected character ~A" c))
    (let ((terminator c))
      (loop
        (setf c (lexer-read-char in))
        (cond
          ((null c) (error "Unexpected end of file in string"))
          ((eql c terminator)
           (return-from get-string-token (coerce string 'simple-string)))
          ((eql c #\\)
           (let ((d (lexer-read-char in)))
             (unless (eql d #\newline)
               (vector-push-extend c string)
               (vector-push-extend d string))))
          (t
           (vector-push-extend c string)))))))

(defun get-identifier-token (in c)
  (declare (optimize (speed 3) (safety 1))
           (character c))
  (let ((string (make-array 20 :element-type 'character
                               :fill-pointer 0 :adjustable t)))
    (vector-push-extend c string)
    (loop
      (setf c (lexer-read-char in))
      (cond
        ((or (char<= #\A c #\Z)
             (char<= #\a c #\z)
             (char<= #\0 c #\9)
             (eql c #\_))
         (vector-push-extend c string))
        (t
         (lexer-unread-char c in)
         (return-from get-identifier-token (coerce string 'simple-string)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *punctuator-tokens-1*
    '(#\[ #\] #\( #\) #\{ #\} #\.
      #\& #\* #\+ #\- #\~ #\!
      #\/ #\% #\< #\> #\^ #\|
      #\? #\: #\;
      #\=
      #\, #\#))

  (defparameter *punctuator-tokens-2*
    '("->"
      "++" "--"
      "<<" ">>" "<=" ">=" "==" "!=" "&&" "||"
      "*=" "/=" "%=" "+=" "-=" "<<=" ">>=" "&=" "~=" "|="
      "<:" ":>" "<%" "%>" "%:"))

  (defparameter *punctuator-tokens-3*
    '("..."
      "<<=" ">>="))

  (defparameter *punctuator-tokens-2-1*
    (append
     (mapcar #'(lambda (s) (aref s 0)) *punctuator-tokens-2*)
     (mapcar #'(lambda (s) (aref s 0)) *punctuator-tokens-3*)))

  (defparameter *punctuator-tokens-3-2*
    (mapcar #'(lambda (s) (subseq s 0 2)) *punctuator-tokens-3*)))

(defun get-punctuator-token (in c)
  (declare (optimize (speed 3) (safety 1))
           (character c))
  (when (member c *punctuator-tokens-2-1* :test 'equal)
    (let* ((d (lexer-read-char in))
           (cd (coerce (list c d) 'string)))
      (when (member cd *punctuator-tokens-3-2* :test 'equal)
        (let* ((e (lexer-read-char in))
               (cde (coerce (list c d e) 'string)))
          (when (member cde *punctuator-tokens-3* :test 'equal)
            (return-from get-punctuator-token cde))
          (lexer-unread-char e in)))
      (when (member cd *punctuator-tokens-2* :test 'equal)
        (return-from get-punctuator-token cd))
      (lexer-unread-char d in)))
  (when (member c *punctuator-tokens-1* :test 'equal)
    (return-from get-punctuator-token (string c)))
  (error "Unexpected punctuator ~A" c))

(defvar *typedef-names*)

(defun typedef-name-p (token)
  (and (stringp token)
       (gethash token *typedef-names*)))

(defun notice-typedef (token)
  (declare (string token))
  (setf (gethash token *typedef-names*) t))

(defun get-comment (in c)
  (lexer-read-char in)
  (case c
    (#\/
     (let ((comment (lexer-read-line in)))
       (lexer-unread-char #\Newline in)
       comment))
    (#\*
     (with-output-to-string (string)
       (loop
         (setf c (lexer-read-char in))
         (if (and (char= c #\*)
                  (eql #\/ (peek-char nil in nil)))
             (progn
               (lexer-read-char in)
               (return))
             (write-char c string)))))))

(defun get-token-string (in c)
  (values 'string
          (with-output-to-string (s)
            (lexer-unread-char c in)
            (loop for line = (lexer-read-line in)
                  for length = (length line)
                  do (cond ((zerop length) (loop-finish)) ; zero length token-string
                           ((eql #\\ (schar line (1- length)))
                            (write-string line s :start 0 :end (1- length)))
                           (t
                            (write-string line s)
                            (lexer-unread-char #\Newline in)
                            (loop-finish)))))))

(defvar *in-cpp*)

(defun get-preprocessor-token (in c preprocessor-state)
  (case preprocessor-state
    ((nil)
     (alexandria:if-let (c (skip-whitespace in)) ; we don't do null directives here
       (let ((id (get-identifier-token in c)))
         (if (member id *cpp-directives* :test #'equal)
             (let* ((s (string-to-symbol id))
                    (type (case s
                            (if 'ppif)
                            (else 'ppelse)
                            (t s))))
               (values type type s)) ; s is the state
             (error "~s is not a valid preprocessor directive" id)))
       (values nil nil)))
    ((include line-filename) ; <> "", no degenerate filenames
     (let ((filename (make-array 20 :element-type 'character
                                    :fill-pointer 0 :adjustable t))
           (terminator (case c
                         (#\< #\>)
                         (#\" #\")
                         (t (return-from get-preprocessor-token)))))
       (loop
         (setf c (lexer-read-char in))
         (cond ((char= c terminator)
                (return (values 'string filename nil)))
               (t
                (vector-push-extend c filename))))))
    (line
     (values 'number (get-numeric-token in c) 'line-filename))
    ;; pragma is unused
    ;; parser has nothing to do with with error garbage strings, so just lex like so
    ((pragma error)
     (get-token-string in c))
    ((ifdef ifndef undef if elif else endif)
     (setf *in-cpp* t)
     (lexer-unread-char c in)
     (get-next-token in nil))
    ;; allow expanding to expressions (including function calls)
    ;; disallow type alias and definition generation for now
    (define
     (setf *in-cpp* t)
     (let ((id (get-identifier-token in c)))
       (if (eql #\( (peek-char nil in nil))
           (values 'identifier id 'define-fn)
           (values 'identifier id nil))))
    (define-fn ; marks function-like macro
     (values 'define-params-start 'define-params-start))
    ))

(defun get-next-token (in preprocessor-state)
  (declare (stream in))
  (prog (c)
   start
     (setf c (skip-whitespace in))
     (cond (preprocessor-state
            (go preprocessor))
           (*in-cpp* ; falls through to # scan/normal processing
            (case c
              (#\\
               (when (eql (peek-char nil in nil) #\Newline)
                 (lexer-read-char in)) ; throw away newline
               (go start)) ; do not start a new directive
              (#\Newline
               (setf *in-cpp* nil)
               (lexer-unread-char c in)
               (return (values 'cpp-end 'cpp-end))))))
   newline-loop ; normal lexing doesn't care for newlines, only the pp
     (let ((first-char? (= 1 (file-position in))) ; we just read the 0th char
           (newline? (char= c #\Newline)))
       (if (or first-char? newline?)
           (progn
             (when newline?
               (setf c (or (skip-whitespace in)
                           (return (values nil nil)))))
             (case c
               (#\# (go preprocessor))
               (#\Newline (go newline-loop))
               (t (go simple))))
           (go simple)))
   preprocessor
     (return (get-preprocessor-token in c preprocessor-state))
   simple
     (return
       (cond
         ((null c) (values nil nil))
         ((or (char<= #\A c #\Z)
              (char<= #\a c #\z)
              (eql c #\_))
          (let ((d (peek-char nil in nil)))
            (if (and (member c '(#\l #\L))
                     (member d '(#\' #\")))
                ;; these may contain escaped literals, evaluate before execution
                (values (if (eql d #\') 'wide-character 'wide-string)
                        (get-string-token in c))
                (let ((id (get-identifier-token in c)))
                  (cond
                    ((member id *c-keywords* :test #'equal)
                     (let ((s (string-to-symbol id)))
                       (values s s)))
                    ((typedef-name-p id)
                     (values 'typedef-name id))
                    (t
                     (values 'identifier id)))))))
         ((char<= #\0 c #\9) ; TODO don't canonicalize ints
          (values 'number (get-numeric-token in c)))
         ((or (char= c #\') (char= c #\"))
          (values (if (eql c #\') 'character 'string) (get-string-token in c)))
         ((find c "[](){}.-+&*~!/%<>=|&^?:;,#")
          (let ((next (peek-char nil in nil)))
            (if (and (char= c #\/)
                     (member next '(#\* #\/)))
                (let ((fpos (file-position in)))
                  (push (list fpos (get-comment in next) (file-position in))
                        *comments*)
                  (go start))
                (let ((s (intern (get-punctuator-token in c) 'weave-c)))
                  (values s s)))))
         (t
          (error "Unexpected character \"~A\" on line ~d" c *line-number*))))))

(defun make-c-lexer (in)
  (declare (stream in))
  (let ((preprocessor-state nil))
    #'(lambda ()
        (multiple-value-bind (type value next-preprocessor-state)
            (get-next-token in preprocessor-state)
          (setf preprocessor-state next-preprocessor-state)
          (values type value)))))

;; the lexer works, though it needs the typedef table first (TODO remember macro #defs too)
(defun lex-c-file (file)
  (let ((*line-number* 1)
        (*typedef-names* (make-hash-table :test 'equal))
        (*comments* (list))
        (*in-cpp* nil))
    (declare (special *typedef-names* *line-number* *comments* *in-cpp*))
    (values (with-open-file (in file)
              (loop with lexer = (make-c-lexer in)
                    for token = (multiple-value-list (funcall lexer))
                    while (car token)
                    collect token))
            ;; (list :comments *comments*)
            )))

(defun preprocess-c-file (file)
  (setf file (namestring (truename file)))
  (uiop:run-program
   (list "gcc" "-E" "-P" "-D__extension__=" "-D __attribute__(x)=" file
         "-o" (concatenate 'string file ".i"))))
