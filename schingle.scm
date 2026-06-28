(define-module (schingle)
  #:use-module (schingle util)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle handler)
  #:use-module (schingle content-type)
  #:use-module (schingle combinators)
  #:use-module (schingle cache)
  #:use-module (schingle template)
  #:use-module (schingle static)
  #:use-module (schingle query)
  #:export (GET HEAD POST PUT DELETE TRACE
            OPTIONS CONNECT PATCH
            run-schingle
            schingle-handler
            *schingle-routes*
            route)
  #:re-export (plain json xml html sexp urlencoded redirect
               template static req-query query
               schingle-static-folder
               schingle-template-folder))

(define *schingle-routes* (make-parameter '()))

(define (route methods path proc)
  (*schingle-routes* (add-route (*schingle-routes*) methods path proc)))

(define-syntax make-method-route
  (syntax-rules ()
    ((_ sym)
     (define (sym path proc)
       (route '(sym) path proc)))))

(make-method-route GET)
(make-method-route HEAD)
(make-method-route POST)
(make-method-route PUT)
(make-method-route DELETE)
(make-method-route TRACE)
(make-method-route OPTIONS)
(make-method-route CONNECT)
(make-method-route PATCH)

(define *schingle-lowerware* (make-parameter '()))
(define *schingle-middleware* (make-parameter '()))
(define *schingle-upperware* (make-parameter '()))

(define (use-lowerware . combinators)
  (*schingle-lowerware* (append (reverse combinators) (*schingle-lowerware*))))

(define (use-middleware . combinators)
  (*schingle-middleware* (append (reverse combinators) (*schingle-middleware*))))

(define (use-upperware . combinators)
  (*schingle-upperware* (append (reverse combinators) (*schingle-upperware*))))

(use-lowerware
 (routes->combinator *schingle-routes*))


(define* (run-schingle #:key (impl 'http)
                       (port 8080)
                       (addr INADDR_ANY)
                       (open-params `(#:port ,port #:addr ,addr)))
  "convinience function that combines making the handler and starting the server."
  (run-server (apply-combinators (append (*schingle-upperware*)
                                         (*schingle-middleware*)
                                         (*schingle-lowerware*)))
              impl open-params))
