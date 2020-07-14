(define-module (schingle schingle)
  #:use-module (schingle util)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle handler)
  #:use-module (schingle content-type)
  #:use-module (schingle middleware)
  #:export (GET HEAD POST PUT DELETE TRACE
            OPTIONS CONNECT PATCH
            GETs HEADs POSTs PUTs DELETEs TRACEs
            OPTIONSs CONNECTs PATCHs
            run-schingle
            schingle-handler
            routes))

(define routes (make-parameter (make-routes)))

(define (schingle-route method route proc)
  "create a route-entry like '((METHOD . /route) . #<proc>)"
  (let ((nproc (handle-content proc)))
    (cons
      (cons method route)
      (lambda (params request body)
        (let ((optional (or (assoc-ref params '*) '()))
              (keyword  (assoc-del params '*)))
          (apply nproc request body
                 (append
                   optional
                   (alist->args keyword))))))))

(define (maybe-symbol->string val)
  (if (string? val)
      val
      (symbol->string val)))

(define (GETs route proc)
  (add-route (routes) (schingle-route 'GET route proc)))

(define-syntax-rule
  (GET route proc)
  (GETs (maybe-symbol->string (quote route)) proc))

(define (HEADs route proc)
  (add-route (routes) (schingle-route 'HEAD route proc)))

(define-syntax-rule
  (HEAD route proc)
  (HEADs (maybe-symbol->string (quote route)) proc))

(define (POSTs route proc)
  (add-route (routes) (schingle-route 'POST route proc)))

(define-syntax-rule
  (POST route proc)
  (POSTs (maybe-symbol->string (quote route)) proc))

(define (PUTs route proc)
  (add-route (routes) (schingle-route 'PUT route proc)))

(define-syntax-rule
  (PUT route proc)
  (PUTs (maybe-symbol->string (quote route)) proc))

(define (DELETEs route proc)
  (add-route (routes) (schingle-route 'DELETE route proc)))

(define-syntax-rule
  (DELETE route proc)
  (DELETEs (maybe-symbol->string (quote route)) proc))

(define (TRACEs route proc)
  (add-route (routes) (schingle-route 'TRACE route proc)))

(define-syntax-rule
  (TRACE route proc)
  (TRACEs (maybe-symbol->string (quote route)) proc))

(define (OPTIONSs route proc)
  (add-route (routes) (schingle-route 'OPTIONS route proc)))

(define-syntax-rule
  (OPTIONS route proc)
  (OPTIONSs (maybe-symbol->string (quote route)) proc))

(define (CONNECTs route proc)
  (add-route (routes) (schingle-route 'CONNECT route proc)))

(define-syntax-rule
  (CONNECT route proc)
  (CONNECTs (maybe-symbol->string (quote route)) proc))

(define (PATCHs route proc)
  (add-route (routes) (schingle-route 'PATCH route proc)))

(define-syntax-rule
  (PATCH route proc)
  (PATCHs (maybe-symbol->string (quote route)) proc))

(define (schingle-handler)
  (routes->handler (routes)))

(define* (run-schingle #:key (impl 'http) (open-params '())
                       (middleware '()))
  "convinience function that combines making the handler and starting the server."
  (run-server (apply-middleware (schingle-handler) middleware)
              impl open-params))
