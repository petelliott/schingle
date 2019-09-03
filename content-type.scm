(define-module (schingle content-type)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (ice-9 iconv)
  #:use-module (json)
  #:use-module (sxml simple)
  #:export (handle-content
            transform-body))


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
