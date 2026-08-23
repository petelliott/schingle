(define-module (schingle static)
  #:use-module (schingle context)
  #:export (*schingle-static-routes*))

(define *schingle-static-routes* (make-schingle-parameter '()))
