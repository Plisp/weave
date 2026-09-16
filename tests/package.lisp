(defpackage #:weave-tests
  (:use :cl #:alexandria-2)
  (:import-from #:parachute #:define-test #:is #:fail #:test)
  (:local-nicknames (#:w #:weave-tui)
                    (#:parse #:weave-parser)
                    (#:tui #:uncursed)
                    (#:tui-sys #:uncursed-sys)))
