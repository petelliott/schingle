;;; utilities that are not schingle specific
(define-module (schingle util)
  #:export (multi-map
            intersperse
            enlist))

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

#|
;TODO unused methods queued for deletion
(define (enlist item)
  "wrap an item in a list if it is not a list"
  (if (list? item)
    item
    (list item)))

(define (alist-app key alist fn)
  "return a new alist where (assoc key list) has fn applied to it.\
  supports duplicates"
  (display alist)
  (newline)
  (if (null? alist)
      alist
      (cons
        (if (equal? (caar alist) key)
          (cons (caar alist) (fn (cdar alist)))
          (car alist))
        (alist-app key (cdr alist) fn))))

(define (collate-keys alist #:optional (res '()))
  "converts an alist keyed by lists to an alist keyed by the first elements of\
  the list"
  (cond
    ((null? alist) res)
    ((assoc (caar alist) res)
     (collate-keys (cdr alist) (alist-app (caar alist)
|#
