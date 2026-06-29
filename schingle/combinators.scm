(define-module (schingle combinators)
  #:use-module (web server)
  #:use-module (web response)
  #:export (apply-combinators
            add-headers))

(define (apply-combinators combinators)
  (if (null? combinators)
      #f
      ((car combinators) (apply-combinators (cdr combinators)))))

(define (add-headers request response body . headers)
  (define sr (sanitize-response request response body))
  (build-response #:version (response-version sr)
                  #:code (response-code sr)
                  #:reason-phrase (response-reason-phrase sr)
                  #:headers (append headers (response-headers sr))
                  #:port (response-port sr)))
