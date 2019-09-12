;;; utilities that are not schingle specific
(define-module (schingle util)
  #:export (multi-map
            intersperse
            enlist
            to-string
            alist->args
            assoc-del))

(define (multi-map lst . procs)
  "maps lst through procs, starting with the first function as the innermost"
  (if (null? procs)
    lst
    (apply multi-map
      (map (car procs) lst)
      (cdr procs))))

(define (intersperse item lst)
  "intersperse item in list. (intersperse * (a b c)) => (a * b * c)"
  (if (or (null? lst)
          (null? (cdr lst)))
    lst
    (cons
      (car lst)
      (cons
        item
        (intersperse item (cdr lst))))))

(define (to-string obj)
  "converts an object to a string via display.\
  not reversable"
  (call-with-output-string
    (lambda (port)
      (display obj port))))

(define (alist->args alist)
  "converts alist like '((a . b) (c . d)) to '(#:a b #:c d)"
  (if (null? alist)
    '()
    (cons
      (symbol->keyword (caar alist))
      (cons
        (cdar alist)
        (alist->args (cdr alist))))))

(define (assoc-del alist key)
  "deletes all items associated with key from alist"
  (filter
    (lambda (pair)
      (not (equal? (car pair) key)))
    alist))
