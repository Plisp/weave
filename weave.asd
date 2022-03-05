(asdf:defsystem #:weave
  :author "tianlin qu <tianlinqu@gmail.com>"
  :description "A semantic C editor"
  :license "BSD 3-clause license"
  :depends-on (#:alexandria
               #:trivial-features
               ;;
               #:cffi-libffi ; for sdl bindings
               #:font-discovery
               #:raw-bindings-sdl2 #:raw-bindings-sdl2-ttf ; *not on quicklisp*
               )
  :pathname "src"
  :components ((:file "nodes")
               (:file "draw" :depends-on ("nodes"))
               (:file "main" :depends-on ("draw" "nodes"))
               ))
