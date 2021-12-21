(asdf:defsystem :kira
  :build-pathname "kira"
  :entry-point "kira:main"
  ;;:author "tianlin qu <tianlinqu@gmail.com>"
  :description "A SDL2 frontend for Kira"
  :license "BSD 3-clause license"
  :depends-on (#:alexandria
               #:trivial-features
               #:yacc
               ;;
               #:cffi-libffi ;; for sdl bindings
               #:cl-environments
               #:font-discovery
               #:raw-bindings-sdl2 #:raw-bindings-sdl2-ttf ; *not on quicklisp*
               ;;#:trivial-clipboard
               )
  :pathname "src"
  :serial t
  :components ((:file "nodes")
               (:file "c")
               (:file "c-parse")
               (:file "main")
               ))
