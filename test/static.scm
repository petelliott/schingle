(use-modules (guest test)
             (schingle static)
             (ice-9 iconv)
             (web response))

(define-syntax-rule
  (with-debug-fns body* ...)
  (let ((last-call-wif call-with-input-file))
    (set! call-with-input-file
      (lambda (file proc)
        (call-with-input-string
          (cond
            ((equal? file "a.json") "{\"a\": 5}")
            ((equal? file "a.xml")  "<a>5</a>")
            ((throw 'system-error '("call-with-input-file" "file-not-found" '()))))
          proc)))
    body* ...
    (set! call-with-input-file last-call-wif)))

(define-syntax-rule
  (values-equal? valuesa valuesb)
  (let-values ((va valuesa)
               (vb valuesb))
    (equal? va vb)))

(define-test (schingle static static)
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/json))))
        (string->bytevector "{\"a\": 5}" "utf-8"))
      (static "a.json" 'application/json)))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/json))))
        (string->bytevector "{\"a\": 5}" "utf-8"))
      (static "a.json" 'application/json)))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/xml))))
        (string->bytevector "<a>5</a>" "utf-8"))
      (static "a.xml" 'application/xml)))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/xml))))
        (string->bytevector "<a>5</a>" "utf-8"))
      (static "a.xml" 'application/xml)))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 404)
          "404 Not Found")
      (static "a.x" 'application/xml))))

(define-test (schingle static static-guess-content)
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/json))))
        (string->bytevector "{\"a\": 5}" "utf-8"))
      (static "a.json")))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 200
          #:headers '((content-type . (application/xml))))
        (string->bytevector "<a>5</a>" "utf-8"))
      (static "a.xml")))
  (with-debug-fns
    (values-equal?
      (values
        (build-response
          #:code 404)
          "404 Not Found")
      (static "a.x"))))
