(use-modules (guest test)
             (schingle util))

(define-test (schingle util multi-map)
  (equal? (multi-map '()) '())
  (equal? (multi-map '(1 2 3)) '(1 2 3))
  (equal? (multi-map '() 1+ 1+) '())
  (equal? (multi-map '(1 2 3) 1+ 1+) '(3 4 5)))

(define-test (schingle util intersperse)
  (equal? (intersperse '* '()) '())
  (equal? (intersperse '* '(a)) '(a))
  (equal? (intersperse '* '(a b c)) '(a * b * c))
  (equal? (intersperse '* '(a b c d)) '(a * b * c * d)))

(define-test (schingle util alist->args)
  (equal? (alist->args '()) '())
  (equal? (alist->args '((a . b))) '(#:a b))
  (equal? (alist->args '((a . b) (c . d))) '(#:a b #:c d)))
