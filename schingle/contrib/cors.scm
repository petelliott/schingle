(define-module (schingle contrib cors)
  #:use-module (web request)
  #:use-module (srfi srfi-11)
  #:use-module (schingle combinators)
  #:export (make-cors-middleware))

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
