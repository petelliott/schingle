# schingle

schingle (pronounced shingle) is a tiny web framework for guile inspired by
[ningle](https://github.com/fukamachi/ningle) (hence "SCHeme nINGLE") and
[sinatra](https://github.com/sinatra/sinatra). It is licensed under the LGPLv3.

## schingle interface

```scheme
(use-modules (schingle schingle))

(define-handlers handlers

(GET /hello
     (lambda (request body)
       (values '((content-type . (text/plain)))
               "Hello World!")))

(GET /hello/:name
     (lambda* (request body #:key :name)
       (values '((content-type . (text/plain)))
                (format #f "Hello, ~a!" :name))))

(GET /error
     (lambda (request body)
       (car '()) ; cause an error to invoke 500 handler
       (values '((content-type . (text/plain)))
               "this shouldn't happen")))

)

(run-schingle handlers)
```

## sinatra-like interface

schingle also has a sinatra-like interface for simple web services.

### example

```scheme
(use-modules (schingle sinatra))

(GET "/hello" (params request body) do
  (values '((content-type . (text/plain)))
          "Hello World!"))

(GET "/hello/:name" (params request body) do
  (values '((content-type . (text/plain)))
          (format #f "Hello, ~a!" (cdr (assoc ':name params)))))

(GET "/error" (params request body) do
  (car '()) ; cause an error to invoke 500 handler
  (values '((content-type . (text/plain)))
          "this shouldn't happen"))

(run-sinatra)
```
