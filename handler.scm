(define-module (schingle handler)
  #:use-module (schingle route)
  #:use-module (schingle content-type)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (ice-9 match)
  #:export (400handler
            404handler
            500handler
            router->handler
            safe-display-error))

(define 400handler
  (make-parameter
    (lambda (request body)
      (plain "400 Bad Request" #:code 400))))

(define 404handler
  (make-parameter
    (lambda (request body)
      (plain "404 Not Found" #:code 404))))

(define 500handler
  (make-parameter
    (lambda (request body)
      (plain "500 Internal Server Error" #:code 500))))

(define (router->handler router)
  "produces a handler compatible with run-server from a route table\
  with (params reqeust body) args"
  (lambda (request body)
    (let ((handler (match-route router
                                (request-method request)
                                (uri-path (request-uri request)))))
      (catch #t
        (lambda ()
          (if handler
            (apply (route-match-value handler) request body (route-match-captures handler))
            ((404handler) request body)))
        ; post-unwind handler
        (lambda (key . args)
          ((500handler) request body))
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
