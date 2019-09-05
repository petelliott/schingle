(define-module (schingle handler)
  #:use-module (schingle route)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:export (400handler
            404handler
            500handler
            routes->handler
            safe-display-error))

(define (400handler request body)
  (values
    (build-response
      #:code 400
      #:headers '((content-type . (text/plain))))
    "400 Bad Request"))

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
          (safe-display-error key args))))))

(define (safe-display-error key args)
  "displays the key and args of the values of a catch"
  (match args
    ((subr msg args . rest)
     (display-error #f (current-output-port) subr msg args rest))
    (else
      (format #t "Throw to key `~a' with args `~s'\n"
                 key args))))
