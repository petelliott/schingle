(define-module (schingle schingle)
  #:use-module (schingle util)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle handler)
  #:use-module (schingle content-type)
  #:use-module (schingle middleware)
  #:use-module (schingle cache)
  #:use-module (schingle template)
  #:use-module (schingle static)
  #:use-module (schingle query)
  #:export (GET HEAD POST PUT DELETE TRACE
            OPTIONS CONNECT PATCH
            GETs HEADs POSTs PUTs DELETEs TRACEs
            OPTIONSs CONNECTs PATCHs
            run-schingle
            schingle-handler
            router)
  #:re-export (plain json xml html sexp urlencoded
               template static req-query query
               schingle-static-folder
               schingle-template-folder))

(define router (make-parameter (make-router)))

(define (maybe-symbol->string val)
  (if (string? val)
      val
      (symbol->string val)))

(define (GETs route proc)
  (add-route! (router) 'GET route (handle-content proc)))

(define-syntax-rule
  (GET route proc)
  (GETs (maybe-symbol->string (quote route)) proc))

(define (HEADs route proc)
  (add-route! (router) 'HEAD route (handle-content proc)))

(define-syntax-rule
  (HEAD route proc)
  (HEADs (maybe-symbol->string (quote route)) proc))

(define (POSTs route proc)
  (add-route! (router) 'POST route (handle-content proc)))

(define-syntax-rule
  (POST route proc)
  (POSTs (maybe-symbol->string (quote route)) proc))

(define (PUTs route proc)
  (add-route! (router) 'PUT route (handle-content proc)))

(define-syntax-rule
  (PUT route proc)
  (PUTs (maybe-symbol->string (quote route)) proc))

(define (DELETEs route proc)
  (add-route! (router) 'DELETE route (handle-content proc)))

(define-syntax-rule
  (DELETE route proc)
  (DELETEs (maybe-symbol->string (quote route)) proc))

(define (TRACEs route proc)
  (add-route! (router) 'TRACE route (handle-content proc)))

(define-syntax-rule
  (TRACE route proc)
  (TRACEs (maybe-symbol->string (quote route)) proc))

(define (OPTIONSs route proc)
  (add-route! (router) 'OPTIONS route (handle-content proc)))

(define-syntax-rule
  (OPTIONS route proc)
  (OPTIONSs (maybe-symbol->string (quote route)) proc))

(define (CONNECTs route proc)
  (add-route! (router) 'CONNECT route (handle-content proc)))

(define-syntax-rule
  (CONNECT route proc)
  (CONNECTs (maybe-symbol->string (quote route)) proc))

(define (PATCHs route proc)
  (add-route! (router) 'PATCH route (handle-content proc)))

(define-syntax-rule
  (PATCH route proc)
  (PATCHs (maybe-symbol->string (quote route)) proc))

(define (schingle-handler)
  (router->handler (router)))

(define* (run-schingle #:key (impl 'http)
                       (port 8080)
                       (addr INADDR_ANY)
                       (open-params `(#:port ,port #:addr ,addr))
                       (middleware '())
                       (use-cache #f)
                       (h404 (404handler)) (h500 (500handler)) (h400 (400handler)))
  "convinience function that combines making the handler and starting the server."
  (parameterize ((404handler h404) (500handler h500) (400handler h400)
                 (schingle-cache-enable use-cache))
    (run-server (apply-middleware (schingle-handler) middleware)
                impl open-params)))
