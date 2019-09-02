(define-module (schingle schingle)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle handler)
  #:export (GET
            HEAD
            POST
            PUT
            DELETE
            TRACE
            OPTIONS
            CONNECT
            PATCH
            make-handler
            run-schingle))

(define (alist-to-args alist)
  (if (null? alist)
    '()
    (cons
      (symbol->keyword (caar alist))
      (cons
        (cdar alist)
        (alist-to-args (cdr alist))))))

(define (schingle-route method route proc)
  (cons
    (cons method route)
    (lambda (params request body)
      (apply proc request body
             (alist-to-args params)))))

(define (GET route proc)
  (schingle-route 'GET route proc))

(define (HEAD route proc)
  (schingle-route 'HEAD route proc))

(define (POST route proc)
  (schingle-route 'POST route proc))

(define (PUT route proc)
  (schingle-route 'PUT route proc))

(define (DELETE route proc)
  (schingle-route 'DELETE route proc))

(define (TRACE route proc)
  (schingle-route 'TRACE route proc))

(define (OPTIONS route proc)
  (schingle-route 'OPTIONS route proc))

(define (CONNECT route proc)
  (schingle-route 'CONNECT route proc))

(define (PATCH route proc)
  (schingle-route 'PATCH route proc))

(define* (make-handler routes #:key (h404 404handler) (h500 500handler))
  (routes->handler (compile-routes routes) #:h404 h404 #:h500 h500))

(define* (run-schingle routes
                       #:optional (impl 'http) (open-params '())
                       #:key (h404 404handler) (h500 500handler))
  (run-server
    (make-handler routes #:h404 h404 #:h500 h500)
    impl open-params))
