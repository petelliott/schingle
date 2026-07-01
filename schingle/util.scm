;;; utilities that are not schingle specific
(define-module (schingle util)
  #:use-module (sxml simple)
  #:export (to-string
            mcons
            xml-escape))

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

(define (xml-escape str)
  (with-output-to-string
    (lambda () (sxml->xml str))))
