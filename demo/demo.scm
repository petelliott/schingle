(use-modules (schingle)
             (schingle combinators))

(GET "/hello"
     (lambda (request body)
       (plain "Hello World")))

(GET "/hello/:name"
     (lambda (request body :name)
       (plain (format #f "Hello, ~a!" :name))))

(GET "/error"
     (lambda (request body)
       (car '()) ; cause an error to invoke 500 handler
       (plain "this shouldn't happen")))

; html form support

(GET "/form"
     (lambda (request body)
         (plain (format #f "Hello, ~a ~a!"
                        (query "firstname")
                        (query "lastname")))))

(POST "/form"
      (lambda (request body)
        (plain (format #f "Hello, ~a ~a!"
                       (assoc-ref body "firstname")
                       (assoc-ref body "lastname")))))
; simple return types

(GET "/json/:value"
     (lambda (request body :value)
       (json `((value . ,:value)))))

(GET "/xml/:value"
     (lambda (request body :value)
       (xml `(value ,:value))))

(GET "/html/:value"
     (lambda (request body :value)
       (html `((html (p ,:value))))))

(GET "/sexp/:value"
     (lambda (request body :value)
       (sexp `((value . ,:value)))))

(GET "/urlencoded/:value"
     (lambda (request body :value)
       (urlencoded `((value . ,:value)))))

; static files

(GET "/schingle/(.*.scm)"
     (lambda (request body filename)
       (static filename 'text/plain)))

; templates

(GET "/template/:name"
     (lambda (request body :name)
       (template "template.mustache" `((name . ,:name)
                                       (listitems ((text . "hello"))
                                                  ((text . "world")))))))

(schingle-static-folder "../")


(run-schingle
 #:port 8080 ; optional setting of port
 )
