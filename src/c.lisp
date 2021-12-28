;;; C language support

;; TODO single-line, multiline comments
;; TODO preprocessor - include files

(defpackage #:kira-c
  (:use :cl))
(in-package #:kira-c)

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

(defparameter *c-keywords*
  '("sizeof"
    "typedef" "extern" "static" "auto" "register" ; SC
    "void" "char" "short" "int" "long" "float" "double" "signed" "unsigned" ; int
    "_Bool" "_Complex" "_Imaginary"
    "struct" "union" "enum"
    "const" "__const" "restrict" "__restrict" "volatile" ; cvr
    "inline" "__inline" "__inline__"
    "case" "default" "if" "else" "switch" "while" "do" "for" ; flow
    "goto" "continue" "break" "return"
    "asm" "__asm__"))

(defun string-to-symbol (s)
  (declare (simple-string s))
  (if (some #'upper-case-p s)
      (intern s 'kira-c)
      (intern (string-upcase s) 'kira-c)))

(defun skip-whitespace (in &optional c)
  (declare (optimize (speed 3) (safety 1))
           (type (or character null) c))
  (let ((c (or c (lexer-read-char in))))
    (declare (type (or null character) c))
    (loop while (member c '(#\Space #\Tab #\Newline))
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
          ((and (eql c terminator) (eql terminator #\"))
           (let ((next (skip-whitespace in)))
             (unless (eql next #\")
               (when next
                 (lexer-unread-char next in))
               (return-from get-string-token (copy-seq string)))))
          ((eql c terminator)
           (return-from get-string-token (copy-seq string)))
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
         (return-from get-identifier-token (copy-seq string)))))))

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
      "##"
      "<:" ":>" "<%" "%>" "%:"))

  (defparameter *punctuator-tokens-3*
    '("..."
      "<<=" ">>="))

  (defparameter *punctuator-tokens-2-1*
    (append
     (mapcar #'(lambda (s) (aref s 0)) *punctuator-tokens-2*)
     (mapcar #'(lambda (s) (aref s 0)) *punctuator-tokens-3*)))

  (defparameter *punctuator-tokens-3-2*
    (mapcar #'(lambda (s) (subseq s 0 2)) *punctuator-tokens-3*))
  )

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

(defun get-next-token (in)
  (declare (stream in))
  (let ((c (skip-whitespace in)))
    (cond
      ((null c) (values nil nil))
      ((or (char<= #\A c #\Z)
           (char<= #\a c #\z)
           (eql c #\_))
       (let ((d (peek-char nil in nil)))
         (if (and (member c '(#\l #\L))
                  (member d '(#\' #\")))
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
      ((char<= #\0 c #\9)
       (values 'number (get-numeric-token in c)))
      ((or (char= c #\') (char= c #\"))
       (values (if (eql c #\') 'character 'string)
               (get-string-token in c)))
      ((find c "[](){}.-+&*~!/%<>=|&^?:;,#")
       (let ((s (intern (get-punctuator-token in c) 'kira-c)))
         (values s s)))
      (t
       (error "Unexpected character \"~A\"" c)))))

(defun make-c-lexer (in)
  (declare (stream in))
  #'(lambda () (get-next-token in)))

(defun lex-c-file (file)
  (let ((*line-number* 1)
        (*typedef-names* (make-hash-table :test 'equal)))
    (declare (special *typedef-names* *line-number* *c-parser*))
    (with-open-file (in file)
      (loop for token = (multiple-value-list (get-next-token in))
            while (car token)
            collect token))))

;;; parser

(defun parse-c-file (in &key typedefs)
  (let ((*line-number* 1)
        (*typedef-names* (make-hash-table :test 'equal)))
    (declare (special *typedef-names* *line-number* *c-parser*))
    (dolist (tok typedefs) (notice-typedef tok))
    (handler-case
        (yacc:parse-with-lexer (make-c-lexer in) *c-parser*)
      (yacc:yacc-parse-error (e)
        (error "Parse error at line ~A:~%~A" *line-number* e)))))
