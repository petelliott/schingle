(use-modules (guest test)
             (schingle schingle)
             (schingle content-type)
             (schingle static)
             (schingle handler)
             (schingle query)

             (web uri)
             (web request)
             (web response)
             (ice-9 iconv))

(define (make-handlers)
  (make-handler (list
    (GET /hello
         (lambda (request body)
           (plain "Hello World")))

    (GET /hello/:name
         (lambda* (request body #:key :name)
           (plain (format #f "Hello, ~a!" :name))))

    (GET /error
         (lambda (request body)
           (car '()) ; cause an error to invoke 500 handler
           (plain "this shouldn't happen")))

    ; html form support

    (GET /form
         (lambda (request body)
           (let ((query (req-query request)))
             (plain (format #f "Hello, ~a ~a!"
                            (assoc-ref query "firstname")
                            (assoc-ref query "lastname"))))))

    (POST /form
          (lambda (request body)
            (plain (format #f "Hello, ~a ~a!"
                           (assoc-ref body "firstname")
                           (assoc-ref body "lastname")))))
    ; simple return types

    (GET /json/:value
         (lambda* (request body #:key :value)
           (json `((value . ,:value)))))

    (GET /xml/:value
         (lambda* (request body #:key :value)
           (xml `(value ,:value))))

    (GET /html/:value
         (lambda* (request body #:key :value)
           (html `((html (p ,:value))))))

    (GET /sexp/:value
         (lambda* (request body #:key :value)
           (sexp `((value . ,:value)))))

    (GET /urlencoded/:value
         (lambda* (request body #:key :value)
           (urlencoded `((value . ,:value)))))

    ; static files

    (GET /schingle/:file
         (lambda* (request body #:key :file)
           (static 'text/plain :file))))))

(define-test (schingle integration sanity)
  (make-handlers))

(define handler (make-handlers))

(define* (check-response response #:key version code reason-phrase headers)
  (and
    (or (not version) (equal? version (response-version response)))
    (or (not code) (equal? code (response-code response)))
    (or (not reason-phrase) (equal? reason-phrase (response-reason-phrase response)))
    (or (not headers) (equal? headers (response-headers response)))))

(define-test (schingle integration basic)
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/hello")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type text/plain)))
         (equal? b "Hello World")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/hello/peter")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type text/plain)))
         (equal? b "Hello, peter!")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/error")) #f)))
    (and (check-response r #:code 500 #:headers '((content-type text/plain)))
         (equal? b "500 Internal Server Error"))))

(define-test (schingle integration form)
  (let-values (((r b)
                (handler
                  (build-request
                    (string->uri
                      "http://localhost/form?firstname=peter&lastname=elliott")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type text/plain)))
         (equal? b "Hello, peter elliott!")))
  (let-values (((r b)
                (handler
                  (call-with-input-string
                    "string"
                    (lambda (port)
                      (build-request
                        (string->uri "http://localhost/form")
                        #:method 'POST #:port port
                        #:headers '((content-type application/x-www-form-urlencoded)))))
                  (string->bytevector "firstname=peter&lastname=elliott" "utf-8"))))
    (and (check-response r #:code 200 #:headers '((content-type text/plain)))
         (equal? b "Hello, peter elliott!"))))

