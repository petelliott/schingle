(define-module (schingle logging)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:use-module (schingle context)
  #:use-module (srfi srfi-19)
  #:export (schingle-logging
            logging-combinator))

(define schingle-logging (make-schingle-parameter #t))

(define (sockaddr->string addr)
  (inet-ntop (sockaddr:fam addr)
             (sockaddr:addr addr)))


(define (logging-combinator next)
  (lambda (request body)
    (define-values (response response-body) (next request body))
    (format #t "[~a] ~a -- ~a ~a ~a ~a\n"
            (date->string (current-date) "~5")
            (sockaddr->string (getpeername (request-port request)))
            (request-method request)
            (uri->string (request-uri request))
            (response-code response)
            (response-reason-phrase response))
    (values response response-body)))
