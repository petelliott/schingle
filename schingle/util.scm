;;; utilities that are not schingle specific
(define-module (schingle util)
  #:export (to-string
            mcons))

(define (to-string obj)
  "converts an object to a string via display.\
  not reversable"
  (call-with-output-string
    (lambda (port)
      (display obj port))))

(define (mcons . rest)
  (if (null? (cdr rest))
      (car rest)
      (cons (car rest)
            (apply mcons (cdr rest)))))
