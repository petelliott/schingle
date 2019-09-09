(define-module (schingle schingle)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle handler)
  #:use-module (schingle content-type)
  #:export (GET HEAD POST PUT DELETE TRACE
            OPTIONS CONNECT PATCH
            GETs HEADs POSTs PUTs DELETEs TRACEs
            OPTIONSs CONNECTs PATCHs
            make-handler
            run-schingle
            define-handlers))

(define (alist-to-args alist)
  "converts alist like '((a . b) (c . d)) to '(#:a b #:c d)"
  (if (null? alist)
    '()
    (cons
      (symbol->keyword (caar alist))
      (cons
        (cdar alist)
        (alist-to-args (cdr alist))))))

(define (schingle-route method route proc)
  "create a route-entry like '((METHOD . /route) . #<proc>)"
  (let ((nproc (handle-content proc)))
    (cons
      (cons method route)
      (lambda (params request body)
        (apply nproc request body
               (alist-to-args params))))))

(define (GETs route proc)
  (schingle-route 'GET route proc))

(define-syntax-rule
  (GET route proc)
  (GETs (symbol->string (quote route)) proc))

(define (HEADs route proc)
  (schingle-route 'HEAD route proc))

(define-syntax-rule
  (HEAD route proc)
  (HEADs (symbol->string (quote route)) proc))

(define (POSTs route proc)
  (schingle-route 'POST route proc))

(define-syntax-rule
  (POST route proc)
  (POSTs (symbol->string (quote route)) proc))

(define (PUTs route proc)
  (schingle-route 'PUT route proc))

(define-syntax-rule
  (PUT route proc)
  (PUTs (symbol->string (quote route)) proc))

(define (DELETEs route proc)
  (schingle-route 'DELETE route proc))

(define-syntax-rule
  (DELETE route proc)
  (DELETEs (symbol->string (quote route)) proc))

(define (TRACEs route proc)
  (schingle-route 'TRACE route proc))

(define-syntax-rule
  (TRACE route proc)
  (TRACEs (symbol->string (quote route)) proc))

(define (OPTIONSs route proc)
  (schingle-route 'OPTIONS route proc))

(define-syntax-rule
  (OPTIONS route proc)
  (OPTIONSs (symbol->string (quote route)) proc))

(define (CONNECTs route proc)
  (schingle-route 'CONNECT route proc))

(define-syntax-rule
  (CONNECT route proc)
  (CONNECTs (symbol->string (quote route)) proc))

(define (PATCHs route proc)
  (schingle-route 'PATCH route proc))

(define-syntax-rule
  (PATCH route proc)
  (PATCHs (symbol->string (quote route)) proc))

(define* (make-handler routes #:key (h404 404handler) (h500 500handler))
  "construct a guile web compatible handler with a list of route/handler \
  pairs and optional 404 and 500 error handlers"
  (routes->handler (compile-routes routes) #:h404 h404 #:h500 h500))

(define* (run-schingle routes
                       #:optional (impl 'http) (open-params '())
                       #:key (h404 404handler) (h500 500handler))
  "convinience function that combines making the handler and starting the server."
  (run-server
    (make-handler routes #:h404 h404 #:h500 h500)
    impl open-params))

(define-syntax-rule
  (define-handlers name body* ...)
  (define name (list body* ...)))
