(in-package #:kira-c)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun rcons (l x)
    "Reverse cons."
    (declare (list l))
    (append l (list x)))

  (defun rcons3 (a b c)
    (declare (ignore b))
    (append a (list c)))

  (defun extract (&rest items)
    (declare (list items))
    #'(lambda (&rest list)
        (mapcar
         #'(lambda (item) (if (integerp item) (nth item list) item))
         items)))

  (defun to-prefix (a b c)
    (list b a c))
  )

;; Returned decls := (storage|qualifiers* type) ((* pointer-qualifiers*) name)*
;; type := standard-type | (struct string)
;; name := ((* qualifiers) name) | NIL | string | function-decl | array-decl
;; function-decl := (FUNCTION string-name param-decl*)
;; array-decl := (ARRAY num|star (pointer-qualifiers *) name), implicitly a pointer

;; n.b. any change to the terminal list must occur here and in the lexer (+typespec rule)
;; XXX don't break on fn(int(T)), declaring int function taking T

(yacc:define-parser *c-parser*
  (:start-symbol translation-unit)
  (:muffle-conflicts (3 0))
  (:terminals (identifier number string wide-string character wide-character
                          |(| |)| |[| |]| |.| |->| |++| |--| |{| |}| |,| |;|
                          |&| |*| |/| |%| |+| |-| |~| |!| |<<| |>>| |<| |>| |<=| |>=|
                          |==| |!=| |^| \| |&&| \|\| |?| |:|
                          |=| |*=| |/=| |%=| |+=| |-=| |<<=| |>>=| |&=| |^=| |...| |\|=|
                          typedef-name sizeof
                          typedef extern static auto register
                          void char short int long float double signed unsigned
                          |_Bool| |_Complex| |_Imaginary|
                          struct union enum
                          const __const restrict __restrict volatile
                          inline __inline __inline__
                          case default if else switch while do for
                          goto continue break return
                          asm __asm__
                          ))

  (string-literal (string #'(lambda (s) (list 'string s)))
                  (wide-string #'(lambda (s) (list 'wide-string s)))
                  (character #'(lambda (s) (list 'character s)))
                  (wide-character #'(lambda (s) (list 'wide-character s))))

  (primary-expression
   identifier
   number
   string-literal
   (|(| expression |)| #'(lambda (a b c) (declare (ignore a c)) b))
   )

  (postfix-expression
   primary-expression
   (postfix-expression |[| expression |]|  (extract 'aref 0 2))
   (postfix-expression |(| argument-expression-list-opt |)|  (extract 'apply 0 2))
   (postfix-expression |.| identifier #'to-prefix)
   (postfix-expression |->| identifier #'to-prefix)
   (postfix-expression |++|  (extract 'post-++ 0))
   (postfix-expression |--|  (extract 'post--- 0))
   (|(| type-name |)| |{| initializer-list |}| (extract 'compound-literal 1 4))
   )

  (argument-expression-list
   (assignment-expression)
   (argument-expression-list |,| assignment-expression #'rcons3)
   )

  (argument-expression-list-opt
   argument-expression-list
   ())

  (unary-expression
   postfix-expression
   (|++| unary-expression)
   (|--| unary-expression)
   (unary-operator cast-expression)
   (sizeof unary-expression  (extract 'sizeof-expression 1))
   (sizeof |(| type-name |)|  (extract 'sizeof-type 2))
   )

  (unary-operator |&| |*| |+| |-| |~| |!|)

  (cast-expression
   unary-expression
   (|(| type-name |)| cast-expression  (extract 'cast 3 1))
   )

  (multiplicative-expression
   cast-expression
   (multiplicative-expression |*| cast-expression #'to-prefix)
   (multiplicative-expression |/| cast-expression #'to-prefix)
   (multiplicative-expression |%| cast-expression #'to-prefix)
   )

  (additive-expression
   multiplicative-expression
   (additive-expression |+| multiplicative-expression #'to-prefix)
   (additive-expression |-| multiplicative-expression #'to-prefix)
   )

  (shift-expression
   additive-expression
   (shift-expression |<<| additive-expression #'to-prefix)
   (shift-expression |>>| additive-expression #'to-prefix)
   )

  (relational-expression
   shift-expression
   (relational-expression |<| shift-expression #'to-prefix)
   (relational-expression |>| shift-expression #'to-prefix)
   (relational-expression |<=| shift-expression #'to-prefix)
   (relational-expression |>=| shift-expression #'to-prefix)
   )

  (equality-expression
   relational-expression
   (equality-expression |==| relational-expression #'to-prefix)
   (equality-expression |!=| relational-expression #'to-prefix)
   )

  (and-expression
   equality-expression
   (and-expression |&| equality-expression #'to-prefix)
   )

  (exclusive-or-expression
   and-expression
   (exclusive-or-expression |^| and-expression #'to-prefix))

  (inclusive-or-expression
   exclusive-or-expression
   (inclusive-or-expression \| exclusive-or-expression #'to-prefix))

  (logical-and-expression
   inclusive-or-expression
   (logical-and-expression |&&| inclusive-or-expression #'to-prefix))

  (logical-or-expression
   logical-and-expression
   (logical-or-expression \|\| logical-and-expression #'to-prefix))

  (conditional-expression
   logical-or-expression
   (logical-or-expression |?| expression |:| conditional-expression
                          (extract '? 0 2 4))
   )

  (assignment-expression
   conditional-expression
   (unary-expression assignment-operator assignment-expression #'to-prefix))

  (assignment-expression-opt
   ()
   assignment-expression)

  (assignment-operator |*=| |/=| |%=| |+=| |-=| |<<=| |>>=| |&=| |^=| |=| |\|=|)

  (expression
   assignment-expression
   (expression |,| assignment-expression #'to-prefix))

  (expression-opt
   ()
   expression)

  (constant-expression conditional-expression)

  (declaration-no-semicolon
   (declaration-specifiers init-declarator-list-opt
                           #'(lambda (ds idl)
                               (when (member 'typedef ds)
                                 (mapc
                                  #'(lambda (name) (notice-typedef (init-declarator-name name)))
                                  idl))
                               (append (list ds) idl))))

  (declaration
   (declaration-no-semicolon |;|
                             #'(lambda (a b) (declare (ignore b)) (cons 'declaration a)))
   )

  (init-declarator-list-opt
   ()
   init-declarator-list)

  (declaration-specifiers
   (storage-class-specifier declaration-specifiers-opt #'cons)
   (type-specifier declaration-specifiers-opt #'cons)
   (type-qualifier declaration-specifiers-opt #'cons)
   (function-specifier declaration-specifiers-opt #'cons))

  (declaration-specifiers-opt
   ()
   declaration-specifiers)

  (init-declarator-list
   (init-declarator)
   (init-declarator-list |,| init-declarator #'rcons3)
   )

  (init-declarator
   declarator
   (declarator |=| initializer #'(lambda (a b c) (declare (ignore b)) (list a c))))

  (storage-class-specifier
   typedef
   extern
   static
   auto
   register)

  (type-specifier
   void
   char
   short
   int
   long
   float
   double
   signed
   unsigned
   |_Bool|
   |_Complex|
   |_Imaginary|
   struct-or-union-specifier
   enum-specifier
   typedef-name)

  (struct-or-union-specifier
   (struct-or-union identifier-opt |{| struct-declaration-list |}|
                    (extract 0 1 3))
   (struct-or-union typedef-name |{| struct-declaration-list |}|
                    (extract 0 1 3))
   (struct-or-union identifier)
   (struct-or-union typedef-name)
   )

  (identifier-opt
   ()
   identifier)

  (struct-or-union
   struct
   union)

  (struct-declaration-list
   (struct-declaration)
   (struct-declaration-list struct-declaration #'rcons))

  (struct-declaration
   (specifier-qualifier-list struct-declarator-list-opt |;|  (extract 0 1))
   )

  (specifier-qualifier-list
   (type-specifier specifier-qualifier-list-opt #'cons)
   (type-qualifier specifier-qualifier-list-opt #'cons))

  (specifier-qualifier-list-opt
   ()
   specifier-qualifier-list)

  (struct-declarator-list-opt
   ()
   struct-declarator-list)

  (struct-declarator-list
   (struct-declarator)
   (struct-declarator-list |,| struct-declarator #'rcons3)
   )


  (struct-declarator
   declarator
   (declarator-opt |:| constant-expression  (extract 'bitfield 0 2)))

  (enum-specifier
   (enum identifier-opt |{| enumerator-list |}|
         (extract 0 1 3))
   (enum identifier-opt |{| enumerator-list |,| |}|
         (extract 0 1 3))
   (enum identifier))

  (enumerator-list
   (enumerator)
   (enumerator-list |,| enumerator #'rcons3))

  (enumerator
   (enumeration-constant)
   (enumeration-constant |=| constant-expression
                         #'(lambda (a b c) (declare (ignore b)) (list a c))))

  (enumeration-constant
   identifier)

  (type-qualifier
   const
   __const
   restrict
   __restrict                             ; GNU extension
   volatile)


  (function-specifier
   inline
   __inline                               ; GNU extension
   __inline__                             ; GNU extension
   )

  (declarator
   (pointer-opt direct-declarator asm-label-opt ; GNU extension fn() asm ("label")
                (lambda (a b c)
                  (list* a b c))))

  (declarator-opt
   ()
   declarator)

  (num-opt
   ()
   number)

  (fn-qualifier-list
   (type-qualifier fn-qualifier-list-opt #'cons)
   (static fn-qualifier-list-opt #'cons))

  (fn-qualifier-list-opt
   ()
   fn-qualifier-list)

  (direct-declarator
   identifier
   (|(| declarator |)| (lambda (a b c) (declare (ignore a c)) b))
   (direct-declarator |[| fn-qualifier-list-opt assignment-expression-opt |]| ; a[3] primary exp
                      (extract 'array 3 2 0))
   (direct-declarator |[| fn-qualifier-list-opt |*| |]|  (extract 'array '* 2 0))
   (direct-declarator |(| parameter-type-list |)|
                      (lambda (a b c d)
                        (declare (ignore b d))
                        (list* 'function a c)))
   )

  (pointer
   (|*| type-qualifier-list-opt #'cons)
   (|*| type-qualifier-list-opt pointer
        #'(lambda (a b c) (append (cons a b) c))))

  (pointer-opt
   ()
   pointer)

  (asm-label
   (asm |(| string |)| (extract 'asm 2))
   (__asm__ |(| string |)| (extract 'asm 2)))

  (asm-label-opt
   ()
   asm-label)

  (type-qualifier-list-opt
   ()
   type-qualifier-list)

  (type-qualifier-list
   (type-qualifier)
   (type-qualifier-list type-qualifier #'rcons)
   )

  (parameter-type-list
   parameter-list
   (parameter-list |,| |...| #'rcons3)
   )


  (parameter-type-list-opt
   ()
   parameter-type-list)

  (parameter-list
   (parameter-declaration)
   (parameter-list |,| parameter-declaration #'rcons3))

  (parameter-declaration
   (declaration-specifiers declarator)
   (declaration-specifiers abstract-declarator-opt))

  (identifier-list
   (identifier)
   (identifier-list |,| identifier #'rcons3))

  (identifier-list-opt
   ()
   identifier-list)

  (type-name
   (specifier-qualifier-list abstract-declarator-opt))

  (abstract-declarator ; function parameter decls
   (pointer (extract 0 'nil))
   (pointer-opt direct-abstract-declarator))

  (abstract-declarator-opt
   ()
   abstract-declarator)

  (direct-abstract-declarator
   (|(| abstract-declarator |)| (lambda (a b c) (declare (ignore a c)) b))
   (direct-abstract-declarator-opt |[| fn-qualifier-list-opt assignment-expression-opt |]|
                                   (extract 'array 3 2 0))
   (direct-abstract-declarator-opt |[| fn-qualifier-list-opt |*| |]|
                                   (extract 'array '* 2 0))
   (direct-abstract-declarator-opt |(| parameter-type-list-opt |)| ; abstract fn decl
                                   (lambda (a b c d)
                                     (declare (ignore b d))
                                     (list* 'function a c)))
   )

  (direct-abstract-declarator-opt
   ()
   direct-abstract-declarator)

  (initializer
   (assignment-expression #'(lambda (x) (list 'expression x)))
   (|{| initializer-list |}|  (extract 'initializer-list 1))
   (|{| initializer-list |,| |}| (extract 'initializer-list 1))
   )

  (initializer-list
   (designation-opt initializer #'(lambda (a b) (list a b)))
   (initializer-list |,| designation-opt initializer
                     #'(lambda (a b c d) (declare (ignore b)) (append a (list (list c d)))))
   )

  (designation
   (designator-list |=| #'(lambda (a b) (list b a))))

  (designation-opt
   ()
   designation)

  (designator-list
   designator
   (designator-list designator))

  (designator
   (|[| constant-expression |]|  (extract 'aref 1))
   (|.| identifier  (extract '|.| 1))
   )

  (statement-no-head
   labeled-statement
   compound-statement
   expression-statement
   selection-statement
   iteration-statement
   jump-statement)

  (statement
   (statement-no-head  (extract 'statement 0))
   )

  (labeled-statement
   (identifier |:| statement  (extract 'label 0 2))
   (case constant-expression |:| statement  (extract 'case 1 3))
   (default |:| statement  (extract 'default 2))
   )

  (compound-statement
   (|{| block-item-list-opt |}|
        #'(lambda (a b c) (declare (ignore a c)) (cons 'block b))))

  (block-item-list
   (block-item)
   (block-item-list block-item #'rcons))

  (block-item-list-opt
   ()
   block-item-list)

  (block-item
   declaration
   statement
   )

  (expression-statement
   (expression-opt |;|  (extract 'expression 0))
   )

  (selection-statement
   (if |(| expression |)| statement  (extract 0 2 4))
   (if |(| expression |)| statement else statement  (extract 0 2 4 6))
   (switch |(| expression |)| statement  (extract 0 2 4)))

  (iteration-statement
   (while |(| expression |)| statement  (extract 0 2 4))
   (do statement while |(| expression |)| |;|  (extract 'do-while 1 4))
   (for |(| expression-opt |;| expression-opt |;| expression-opt |)| statement
        (extract 0 2 4 6 8))
   (for |(| declaration expression-opt |;| expression-opt |)| statement
        #'(lambda (a b c d e f g h) (declare (ignore b e g h))
            (list a (list 'declaration c) d f)))
   )

  (jump-statement
   (goto identifier |;| (extract 0 1))
   (continue |;| (constantly (list 'continue)))
   (break |;| (constantly (list 'break)))
   (return expression-opt |;|  (extract 0 1))
   )

  (translation-unit
   (external-declaration)
   (translation-unit external-declaration #'rcons)
   )

  (external-declaration ; TODO checkpoint file-position here
   function-definition
   declaration)

  (function-definition
   (declaration-specifiers declarator ;declaration-list-opt k&r?
                           compound-statement
                           (extract 'definition 0 1 2))
   )

  (declaration-list
   (declaration)
   (declaration-list declaration #'rcons))

  (declaration-list-opt
   ()
   declaration-list))
