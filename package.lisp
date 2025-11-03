(defpackage #:lgj-fall25
  (:use #:cl);;#:cl-fast-ecs #:float-features
  (:import-from #:alexandria #:clamp #:define-constant #:make-keyword)
  ;;(:import-from #:float-features #:single-float-nan #:float-nan-p)
  (:export #:start #:stop #:stop-thread #:stop-thread-by-name))
