(use-modules (guest test)
             (schingle handler)
             (srfi srfi-11)
             (web request)
             (web response)
             (web uri))

(define-syntax-rule
  (values-equal? valuesa valuesb)
  (let-values ((va valuesa)
               (vb valuesb))
    (equal? va vb)))

(define-test (schingle handler 400handler)
  (assert (values-equal?
           (values
            (build-response
             #:code 400
             #:headers '((content-type . (text/plain))))
            "400 Bad Request")
           ((400handler) #f #f))))

(define-test (schingle handler 404handler)
  (assert (values-equal?
           (values
            (build-response
             #:code 404
             #:headers '((content-type . (text/plain))))
            "404 Not Found")
           ((404handler) #f #f))))

(define-test (schingle handler 500handler)
  (assert (values-equal?
           (values
            (build-response
             #:code 500
             #:headers '((content-type . (text/plain))))
            "500 Internal Server Error")
           ((500handler) #f #f))))

(define (test-handler)
  (routes->handler (make-routes)))

(define-test (schingle handler routes->handler)
  (assert (values-equal?
           ((test-handler)
            (build-request (string->uri "http://localhost"))
            #f)
           ((404handler) #f #f))))
