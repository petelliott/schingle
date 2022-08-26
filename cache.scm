(define-module (schingle cache)
  #:export (schingle-cache-enable
            cached))

(define schingle-cache-enable (make-parameter #f))

(define cache (make-hash-table))

(define (cached site key proc)
  (define k (cons site key))
  (if (schingle-cache-enable)
      (or (hash-ref cache k)
          (hash-set! cache k (proc)))
      (proc)))
