(asdf:defsystem #:weave
  :author "tianlin qu"
  :description "A semantic lisp editor"
  :license "BSD 3-clause license"
  :depends-on (#:alexandria
               :bordeaux-threads
			   :cl-environments
               #:closer-mop
			   #:eclector
			   :slynk
               #:trivial-features
               #:trivia
               #:uncursed
               #+sbcl #:sb-concurrency
               #+sbcl #:sb-cltl2
               #+sbcl #:sb-sprof
               )
  :pathname "src"
  :components ((:file "util")
               (:file "ast" :depends-on ("util"))
               (:file "indentation" :depends-on ("ast"))
               (:file "serialization" :depends-on ("indentation"))
               (:file "tui" :depends-on ("util" "ast" "indentation"))
               (:file "commands" :depends-on ("util" "ast" "tui" "serialization"))
               (:file "selection" :depends-on ("tui" "commands"))
               (:file "entry-point" :depends-on ("selection"))
               )
  :in-order-to ((asdf:test-op (asdf:test-op #:weave/tests))))

(asdf:defsystem #:weave/tests
  :description "Ad-hoc tests for weave's parser and frontend"
  :depends-on (#:weave #:parachute)
  :pathname "tests"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "parser" :depends-on ("util"))
               (:file "editing" :depends-on ("util")))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call '#:parachute '#:test '#:weave-tests)))
