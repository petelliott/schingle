# schingle

[![Build Status](https://travis-ci.com/Petelliott/schingle.svg?branch=master)](https://travis-ci.com/Petelliott/schingle)
[![Coverage Status](https://coveralls.io/repos/github/Petelliott/schingle/badge.svg?branch=master)](https://coveralls.io/github/Petelliott/schingle?branch=master)

schingle (pronounced shingle) is a tiny web framework for guile inspired by
[ningle](https://github.com/fukamachi/ningle) (hence "SCHeme nINGLE") and
[sinatra](https://github.com/sinatra/sinatra). It is licensed under the LGPLv3.

```scheme
(GET /hello/:name
     (lambda* (request body #:key :name)
       (plain (format #f "Hello, ~a!" :name))))
```

## installation

1) install [guile-json](https://github.com/aconchillo/guile-json)

2) clone this repository into somewhere on guile's `%load-path`

## testing

1) install [guest](https://github.com/Petelliott/guest)

2) run `guest test/`

## a more fleshed out example

```scheme
(use-modules (schingle schingle)
             (schingle content-type)
             (schingle static)
             (schingle handler)
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

(GET /schingle/*.scm
     (lambda* (request body #:optional :file)
       (static 'text/plain (string-append :file ".scm"))))

)

(define (custom404 request body)
  "a custom 404 handler. custom 500 and 400 handlers can also be defined"
  (plain "oopsie" #:code 404))

(parameterize ((404handler custom404))
  (run-schingle handlers))
```
