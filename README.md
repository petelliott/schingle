# schingle

schingle (pronounced shingle) is a tiny web framework for guile inspired by
[ningle](https://github.com/fukamachi/ningle) (hence "SCHeme nINGLE") and
[sinatra](https://github.com/sinatra/sinatra). It is licensed under the LGPLv3.

```scheme
(GET /hello/:name
     (lambda (request body :name)
       (plain (format #f "Hello, ~a!" :name))))
```

## installation

1) install [guile-json](https://github.com/aconchillo/guile-json)

2) clone this repository into somewhere on guile's `%load-path`

## a more fleshed out example

```scheme
(use-modules (schingle schingle)
             (schingle middleware))

; paths in schingle may be strings or symbols:
(GET "/hello"
     (lambda (request body)
       (plain "Hello World")))

(GET /hello/:name
     (lambda (request body :name)
       (plain (format #f "Hello, ~a!" :name))))

(GET /error
     (lambda (request body)
       (car '()) ; cause an error to invoke 500 handler
       (plain "this shouldn't happen")))

; html form support

(GET /form
     (lambda (request body)
         (plain (format #f "Hello, ~a ~a!"
                        (query "firstname")
                        (query "lastname")))))

(POST /form
      (lambda (request body)
        (plain (format #f "Hello, ~a ~a!"
                       (assoc-ref body "firstname")
                       (assoc-ref body "lastname")))))
; simple return types

(GET /json/:value
     (lambda (request body :value)
       (json `((value . ,:value)))))

(GET /xml/:value
     (lambda (request body :value)
       (xml `(value ,:value))))

(GET /html/:value
     (lambda (request body :value)
       (html `((html (p ,:value))))))

(GET /sexp/:value
     (lambda (request body :value)
       (sexp `((value . ,:value)))))

(GET /urlencoded/:value
     (lambda (request body :value)
       (urlencoded `((value . ,:value)))))

; static files

(GET "/schingle/(.*.scm)"
     (lambda (request body filename)
       (static filename 'text/plain)))

; templates

(GET /template/:name
     (lambda (request body :name)
       (template "template.mustache" `((name . ,:name)
                                       (listitems ((text . "hello"))
                                                  ((text . "world")))))))

(schingle-static-folder "../")

(define (custom404 request body)
  "a custom 404 handler. custom 500 and 400 handlers can also be defined"
  (plain "oopsiedoo" #:code 404))

(run-schingle #:middleware (list (make-cors-middleware))
              #:port 8080 ; optional setting of port
              #:use-cache #t ; optional cache for static files and templates
              #:h404 custom404)
```
