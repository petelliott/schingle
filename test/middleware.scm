(use-modules (guest test)
             (schingle middleware)
             (srfi srfi-11)
             (web response)
             (web request)
             (web uri))


(define-syntax-rule
  (values-equal? valuesa valuesb)
  (let-values ((va valuesa)
               (vb valuesb))
    (equal? va vb)))

(define-test (schingle middleware apply-middleware)
  (assert-equal? (apply-middleware 4 '()) 4)
  (assert-equal? (apply-middleware 4 (list (lambda (x) (1+ x))
                                           (lambda (x) (* 2 x))))
                 10))

(define-test (schingle middleware make-cors-middleware)
  (assert-equal? (response-headers
                  (((make-cors-middleware)
                    (lambda (request request-body)
                      (values '((content-type . (text/plain)))
                              "")))
                   (build-request (string->uri "http://localhost")
                                  #:headers '((origin . "whatever")))
                   ""))
                 '((Access-Control-Allow-Origin . "*")
                   (Access-Control-Allow-Methods . "*")
                   (content-length . 0)
                   (content-type text/plain (charset . "utf-8")))))
