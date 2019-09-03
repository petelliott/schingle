(define-module (schingle content-type)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (json)
  #:use-module (sxml simple)
  #:export (handle-content
            transform-body
            plain))

(define (handle-content proc)
  "produces a new handler that transforms the body based on the content-type"
  (lambda (request body)
    (proc request (transform-body request body))))

(define (transform-body request body)
  "transforms body into a suitable scheme object based on request's \
  content-type"
  (let* ((ctype (request-content-type request))
         (type (car ctype))
         (args (cdr ctype)))
    (case type
      ((text/plain)
       (bytevector->string
         body (or (assoc-ref args 'charset) "utf-8")))
      ((application/json)
       (json-string->scm
         (bytevector->string
           body (or (assoc-ref args 'charset) "utf-8"))))
      ((application/xml application/html)
       (xml->sxml
         (bytevector->string
           body (or (assoc-ref args 'charset) "utf-8"))))
      ((application/x.s-expression)
       (call-with-input-string
         (bytevector->string
           body (or (assoc-ref args 'charset) "utf-8"))
         (lambda (port)
           (read port))))
      (else body))))

(define (dcons a b rest)
  (cons a (cons b rest)))

(define* (build-content-response content #:key (version '(1 . 1))
                                               (code 200)
                                               (reason-phrase #f)
                                               (headers '())
                                               (port #f)
                                               (validate-headers? #t))
  (build-response #:version version
                  #:code code
                  #:reason-phrase reason-phrase
                  #:headers (acons 'content-type content headers)
                  #:port port
                  #:validate-headers? validate-headers?))

(define (plain body . rest)
  (values
    (apply build-content-response '(text/plain) rest)
    body))
