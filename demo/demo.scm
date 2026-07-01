(use-modules (schingle)
             (schingle contrib cors)
             (schingle template mustache))

(GET "/hello"
     (lambda (request body)
       (plain "Hello World\n")))

(GET-static "/hello-static"
            (plain "Hello World, statically\n"))

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

(schingle-include-path "../schingle")

(GET "/schingle/(.*.scm)"
     (lambda (request body filename)
       (static filename 'text/plain)))

; templates

; templates are rendered from schingle's include path
(schingle-include-path "./")

(define template (mustache-compile "template.html.mustache"))

(GET "/template/:name"
     (lambda (request body :name)
       (content '(text/html)
                (template `((name . ,:name)
                            (var . "variables")
                            (lambdavar . ,(lambda () "lambda variables"))
                            (varsection . ((str . "variable sections")))
                            (listsection . #(((text . "list sections 1/2"))
                                             ((text . "list sections 2/2"))))
                            (lambdasection . ,(lambda (text) (string-append text "sections")))
                            (falsesection . #f)
                            )))))

;; middleware

(use-middleware
 (make-cors-middleware))


(run-schingle
 #:port 8080) ; optional setting of port
