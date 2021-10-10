(asdf:defsystem :kira
  :build-pathname "kira"
  :entry-point "kira:main"
  ;;:author "tianlin qu <tianlinqu@gmail.com>"
  :description "A SDL2 frontend for Kira"
  :license "BSD 3-clause license"
  :depends-on (#:alexandria
               #:trivial-features
               #:cl-environments
               #:cffi-libffi
               #:font-discovery
               #:trivial-clipboard
               ;; *not on quicklisp*
               #:raw-bindings-sdl2 #:raw-bindings-sdl2-ttf)
  :pathname "src"
  :serial t
  :components ((:file "impl")))
