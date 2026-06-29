(define-module (schingle error)
  #:use-module (schingle status)
  #:use-module (ice-9 match)
  #:use-module (web request)
  #:use-module (web uri)
  #:export (500combinator
            safe-display-error))

(define (500combinator next)
  (lambda (request body)
    (catch #t
      (lambda ()
        (next request body))
      ;; post-unwind handler
      (lambda (key . args)
        (internal-server-error))
      ;; pre-unwind handler
      (lambda (key . args)
        (format #t "error in request to ~A: " (uri-path (request-uri request)))
        (safe-display-error key args)))))


(define (safe-display-error key args)
  "displays the key and args of the values of a catch"
  (match args
    ((subr msg args . rest)
     (display-error #f (current-output-port) subr msg args rest))
    (else
      (format #t "Throw to key `~a' with args `~s'\n"
                 key args))))
