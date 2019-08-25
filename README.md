# schingle

schingle (pronounced shingle) is a tiny web framework for guile inspired by
[ningle](https://github.com/fukamachi/ningle) (hence "SCHeme nINGLE") and
[sinatra](https://github.com/sinatra/sinatra). It is licensed under the LGPLv3.

## schingle interface

the main interface does not exist yet.

## sinatra-like interface

schingle has a sinatra-like interface for simple web services.

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
