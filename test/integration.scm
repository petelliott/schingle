(use-modules (guest test)
             (schingle schingle)
             (schingle content-type)
             (schingle static)
             (schingle handler)
             (schingle query)

             (web uri)
             (web request)
             (web response)
             (ice-9 iconv)
             (ice-9 binary-ports))

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

(define-test (schingle integration return)
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/json/blah")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type application/json)))
         (equal? b "{\"value\":\"blah\"}")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/xml/blah")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type application/xml)))
         (equal? b "<value>blah</value>")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/html/blah")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type text/html)))
         (equal? b "<html><p>blah</p></html>")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/sexp/blah")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type application/x.s-expression)))
         (equal? b "((value . \"blah\"))")))
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/urlencoded/blah")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type application/x-www-form-urlencoded)))
         (equal? b "value=blah"))))

(define (read-file fname)
  "read a file to a byte-vector"
  (call-with-input-file
    fname
    (lambda (port)
      (get-bytevector-all port))))

(define-test (schingle integration static)
  (let-values (((r b)
                (handler (build-request (string->uri "http://localhost/schingle/static.scm")) #f)))
    (and (check-response r #:code 200 #:headers '((content-type text/plain)))
         (equal? b (read-file "static.scm")))))
