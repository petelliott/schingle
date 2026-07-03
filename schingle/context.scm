(define-module (schingle context)
  #:export (make-schingle-parameter
            with-schingle-context
            with-clean-schingle))

(define *schingle-parameters* (make-parameter '()))

(define (make-schingle-parameter dflt)
  (define parameter (make-parameter dflt))
  (*schingle-parameters* (acons parameter dflt (*schingle-parameters*)))
  parameter)

(define (with-schingle-context thunk)
  (define (inner params)
    (if (null? params)
        (thunk)
        (parameterize (( (caar params) ((caar params)) ))
          (inner (cdr params)))))
  (inner (*schingle-parameters*)))

(define (with-clean-schingle thunk)
  (define (inner params)
    (if (null? params)
        (thunk)
        (parameterize (( (caar params) (cdar params) ))
          (inner (cdr params)))))
  (inner (*schingle-parameters*)))
