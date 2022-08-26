;;; utilities that are not schingle specific
(define-module (schingle util)
  #:export (to-string))

(define (to-string obj)
  "converts an object to a string via display.\
  not reversable"
  (call-with-output-string
    (lambda (port)
      (display obj port))))
