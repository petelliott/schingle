;;; utilities that are not schingle specific
(define-module (schingle util)
  #:use-module (sxml simple)
  #:export (mcons
            xml-escape))

(define (mcons . rest)
  (if (null? (cdr rest))
      (car rest)
      (cons (car rest)
            (apply mcons (cdr rest)))))

(define (xml-escape str)
  (with-output-to-string
    (lambda () (sxml->xml str))))
