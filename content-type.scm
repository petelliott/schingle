(define-module (schingle content-type)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 iconv)
  #:use-module (json)
  #:use-module (sxml simple)
  #:use-module (schingle query)
  #:use-module (schingle handler)
  #:export (handle-content
            transform-body
            plain
            json
            xml
            html
            sexp
            urlencoded))

(define (handle-content proc)
  "produces a new handler that transforms the body based on the content-type"
  (lambda (request body . rest)
    (let* ((failure (gensym))
           (nbody (catch #t
                    (lambda ()
                      (transform-body request body))
                    (lambda (key . args)
                      (format #t "malformed body in request to ~A: "
                              (uri-path (request-uri request)))
                      (safe-display-error key args)
                      failure))))
      (if (not (equal? nbody failure))
        (apply proc request (transform-body request body) rest)
        ((400handler) request body)))))

(define (transform-body request body)
  "transforms body into a suitable scheme object based on request's \
  content-type"
  (let* ((ctype (request-content-type request))
         (type (and ctype (car ctype)))
         (args (and ctype (cdr ctype)))
         (sbody (and body
                     (bytevector->string
                       body (or (assoc-ref args 'charset) "utf-8")))))
    (case type
      ((text/plain) sbody)
      ((application/json) (json-string->scm sbody))
      ((application/xml application/html) (xml->sxml sbody))
      ((application/x.s-expression)
       (call-with-input-string
         sbody
         (lambda (port)
           (read port))))
      ((application/x-www-form-urlencoded) (query->alist sbody))
      (else body))))

(define* (build-content-response content #:key (version '(1 . 1))
                                               (code 200)
                                               (reason-phrase #f)
                                               (headers '())
                                               (port #f)
                                               (validate-headers? #t))
  "takes the same arguments as build-response but has a seperate argument for \
  content-type and other headers"
  (build-response #:version version
                  #:code code
                  #:reason-phrase reason-phrase
                  #:headers (acons 'content-type content headers)
                  #:port port
                  #:validate-headers? validate-headers?))

(define (plain body . rest)
  "returns a plain text response/body, takes the same keyword args as \
  build-response"
  (values
    (apply build-content-response '(text/plain) rest)
    body))

(define (json body . rest)
  "returns a json response/body. converts s-expression body to json. takes \
  the same keyword args as build-response"
  (values
    (apply build-content-response '(application/json) rest)
    (scm->json-string body)))

(define (xml body . rest)
  "returns an xml response/body. converts SXML body to xml. takes \
  the same keyword args as build-response"
  (values
    (apply build-content-response '(application/xml) rest)
    (call-with-output-string
      (lambda (port)
        (sxml->xml body port)))))

(define (html body . rest)
  "returns an html response/body. converts SXML body to html. takes \
  the same keyword args as build-response"
  (values
    (apply build-content-response '(text/html) rest)
    (call-with-output-string
      (lambda (port)
        (sxml->xml body port)))))

(define (sexp body . rest)
  "returns an s-expression response/body. seriallizes s-expression body.
  takes the same keyword args as build-response"
  (values
    (apply build-content-response '(application/x.s-expression) rest)
    (call-with-output-string
      (lambda (port)
        (write body port)))))

(define (urlencoded body . rest)
  "returns a urlencoded response/body. converts alist body to url encoded. \
  takes the same keyword args as build-response"
  (values
    (apply build-content-response '(application/x-www-form-urlencoded) rest)
    (alist->query body)))
