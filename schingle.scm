(define-module (schingle)
  #:use-module (srfi srfi-11)
  #:use-module (schingle util)
  #:use-module (web server)
  #:use-module (schingle route)
  #:use-module (schingle content-type)
  #:use-module (schingle combinators)
  #:use-module (schingle status)
  #:use-module (schingle query)
  #:use-module (schingle error)
  #:use-module (schingle load)
  #:use-module (schingle render)
  #:use-module (schingle context)
  #:use-module (schingle logging)
  #:use-module (schingle static)
  #:export (GET HEAD POST PUT DELETE TRACE
            OPTIONS CONNECT PATCH
            GET-static
            run-schingle
            *schingle-routes*
            route
            use-middleware)
  #:re-export (plain json xml html sexp urlencoded redirect content
               serve-directory add-renderer render
               request-query query
               schingle-include-path schingle-include-relative
               schingle-logging))

(define *schingle-routes* (make-schingle-parameter '()))

(define (route methods path proc)
  (*schingle-routes* (add-route (*schingle-routes*) methods path proc))
  (car (*schingle-routes*)))

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

(define *schingle-serve-static* (make-schingle-parameter #t))

(define-syntax GET-static
  (syntax-rules ()
    ((_ path result)
     (let-values (((response body) result))
       (route '(GET) path (lambda (r b) (values response body)))
       (*schingle-static-routes*
        (acons path (list response body)
         (*schingle-static-routes*)))))))

(define *schingle-lowerware* (make-schingle-parameter '()))
(define *schingle-middleware* (make-schingle-parameter '()))
(define *schingle-upperware* (make-schingle-parameter '()))

(define (use-lowerware . combinators)
  (*schingle-lowerware* (append (reverse combinators) (*schingle-lowerware*))))

(define (use-middleware . combinators)
  (*schingle-middleware* (append (reverse combinators) (*schingle-middleware*))))

(define (use-upperware . combinators)
  (*schingle-upperware* (append (reverse combinators) (*schingle-upperware*))))

(use-lowerware
 (lambda (next) (lambda (request body) (not-found)))
 serve-directory-combinator
 (routes->combinator *schingle-routes*))

(use-upperware
 transform-body-combinator
 query-string-combinator
 500combinator
 logging-combinator)

(define (open-addr-params str)
  (define addrinfo (car (getaddrinfo str "http")))
  `(#:family ,(addrinfo:fam addrinfo)
             #:addr ,(sockaddr:addr (addrinfo:addr addrinfo))))

(define* (run-schingle #:key (impl 'http)
                       (port 8000)
                       (addr "127.0.0.1")
                       (open-params `(#:port ,(or (maybe string->number (getenv "SCHINGLE_PORT")) port)
                                      ,@(open-addr-params (or (getenv "SCHINGLE_ADDR") addr)))))
  (run-server (apply-combinators (append (*schingle-upperware*)
                                         (*schingle-middleware*)
                                         (*schingle-lowerware*)))
              impl open-params)
  (newline))
