# schingle

schingle (pronounced shingle) is a tiny web framework for guile inspired by
[ningle](https://github.com/fukamachi/ningle) (hence "SCHeme nINGLE") and
[sinatra](https://github.com/sinatra/sinatra). It is licensed under the LGPLv3.

## installation

1) install [guile-json](https://github.com/aconchillo/guile-json)

2) clone this repository into somewhere on guile's `%load-path`

## schingle interface

```scheme
(use-modules (schingle schingle)
             (schingle content-type)
             (schingle static)
             (schingle query))

(define-handlers handlers

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
       (static 'text/plain :file)))

)

(run-schingle handlers)
```

## sinatra-like interface

schingle also has a sinatra-like interface for simple web services.

### example

```scheme
(use-modules (schingle sinatra)
             (schingle content-type))

(GET "/hello" (params request body) do
  (plain "Hello World!"))

(GET "/hello/:name" (params request body) do
  (plain (format #f "Hello, ~a!" (cdr (assoc ':name params)))))

(GET "/error" (params request body) do
  (car '()) ; cause an error to invoke 500 handler
  (plain "this shouldn't happen"))

(run-sinatra)
```
