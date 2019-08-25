(define-module (schingle handler)
  #:use-module (schingle route)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (404handler
            500handler
            routes->handler))

(define (404handler request body)
  (values
    (build-response
      #:code 404
      #:headers '((content-type . (text/plain))))
    "404 Not Found"))

(define (500handler request body)
  (values
    (build-response
      #:code 500
      #:headers '((content-type . (text/plain))))
    "500 Internal Server Error"))

(define* (routes->handler routefn #:key (h404 404handler) (h500 500handler))
  "produces a handler compatible with run-server from a compiled route table\
  with (params reqeust body) args"
  (lambda (request body)
    (let ((handler (routefn (cons (request-method request)
                                  (uri-path (request-uri request))))))
      (catch #t
        (lambda ()
          (if handler
            ((cdr handler) (car handler) request body)
            (h404 request body)))
        ; post-unwind handler
        (lambda (key . args)
          (h500 request body))
        ; pre-unwind handler
        (lambda (key . args)
          (format #t "error in request to ~A: " (uri-path (request-uri request)))
          (display-error #f (current-output-port)
                         (car args) (cadr args) (caddr args) #f))))))
