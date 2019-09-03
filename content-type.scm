(define-module (schingle content-type)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (json)
  #:use-module (sxml simple)
  #:export (handle-content
            transform-body
            plain
            json
            xml
            html
            sexp))

(define (handle-content proc)
  "produces a new handler that transforms the body based on the content-type"
  (lambda (request body . rest)
    (apply proc request (transform-body request body) rest)))

(define (transform-body request body)
  "transforms body into a suitable scheme object based on request's \
  content-type"
  (let* ((ctype (request-content-type request))
         (type (and ctype (car ctype)))
         (args (and ctype (cdr ctype))))
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

(define (json body . rest)
  (values
    (apply build-content-response '(application/json) rest)
    (scm->json-string body)))

(define (xml body . rest)
  (values
    (apply build-content-response '(application/xml) rest)
    (call-with-output-string
      (lambda (port)
        (sxml->xml body port)))))

(define (html body . rest)
  (apply xml body rest))

(define (sexp body . rest)
  (values
    (apply build-content-response '(application/x.s-expression) rest)
    (call-with-output-string
      (lambda (port)
        (write body port)))))
