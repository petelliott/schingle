(define-module (schingle middleware)
  #:use-module (srfi srfi-11)
  #:use-module (web request)
  #:use-module (web server)
  #:use-module (web response)
  #:export (apply-middleware
            add-headers
            make-cors-middleware))

(define (apply-middleware handler middleware)
  "create a new guile http handler by calling each proc in the list middleware \
   on it"
  (if (null? middleware)
      handler
      (apply-middleware ((car middleware) handler)
                        (cdr middleware))))

(define (add-headers request response body . headers)
  (define sr (sanitize-response request response body))
  (build-response #:version (response-version sr)
                  #:code (response-code sr)
                  #:reason-phrase (response-reason-phrase sr)
                  #:headers (append headers (response-headers sr))
                  #:port (response-port sr)))


(define* (make-cors-middleware #:optional (allow-origin "*")
                               (allow-methods "*"))
  (lambda (handler)
    (lambda (request request-body)
      (let-values (((response response-body)
                    (handler request request-body)))
        (if (assoc 'origin (request-headers request))
            (values (add-headers request response response-body
                                 `(Access-Control-Allow-Origin . ,allow-origin)
                                 `(Access-Control-Allow-Methods . ,allow-methods))
                    response-body)
            (values response
                    response-body))))))
