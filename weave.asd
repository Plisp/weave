(asdf:defsystem :weave
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A semantic lisp editor"
  :license "BSD 3-clause license"
  :depends-on (#:alexandria
               ;;:bordeaux-threads
			   :cl-environments
			   #:eclector
			   :slynk
               #:trivial-features
               :trivia
               ;;:uncursed
               )
  :pathname "src"
  :components ((:file "ast")
			   ;(:file "nodes")
               ;(:file "draw" :depends-on ("nodes"))
               ;(:file "main" :depends-on ("draw" "nodes"))
               ))
