;;; sintra like interface for schingle.
;;; this interface was made for fun.
;;; it is recomened that you use the regular schingle interface.
(define-module (schingle sinatra)
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
            run-sinatra))

(display "WARNING: schingle's sinatra interface is deprecated\n")

(define *sinatra-routes* '())

(define (add-sinatra-route route proc)
  "adds route/proc to the sinatra routes"
  (set! *sinatra-routes* (acons route proc *sinatra-routes*)))

(define-syntax sinatra-request
  (syntax-rules ()
    ((_ method path (params request body) body* ...)
     (add-sinatra-route
       (cons (quote method) path)
       (lambda (params request body)
         body* ...)))))

(define-syntax GET
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request GET path (params request body) body* ...))))

(define-syntax HEAD
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request HEAD path (params request body) body* ...))))

(define-syntax POST
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request POST path (params request body) body* ...))))

(define-syntax PUT
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request PUT path (params request body) body* ...))))

(define-syntax DELETE
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request DELETE path (params request body) body* ...))))

(define-syntax TRACE
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request TRACE path (params request body) body* ...))))

(define-syntax OPTIONS
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request OPTIONS path (params request body) body* ...))))

(define-syntax CONNECT
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request CONNECT path (params request body) body* ...))))

(define-syntax PATCH
  (syntax-rules (do)
    ((_ path (params request body) do body* ...)
     (sinatra-request PATCH path (params request body) body* ...))))

(define* (run-sinatra #:optional (impl 'http) (open-params '()))
  (run-server
    (routes->handler (compile-routes *sinatra-routes*))
  impl open-params))
