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
               )
  :pathname "src"
  :components ((:file "util")
               (:file "ast" :depends-on ("util"))
               (:file "tui" :depends-on ("util" "ast"))
               ))
