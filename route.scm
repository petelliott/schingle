(define-module (schingle route)
  #:use-module (web uri)
  #:use-module (schingle util)
  #:export (parse-route
            compile-routmap
            pathmap-ref    ;TODO: maybe remove
            make-pathmap)) ;TODO: remove

(define (parse-route routespec)
  "parse a routespec, returning a list that represents it.\
  a routespec is a method-string pair."
  (cons (car routespec) (parse-path (cdr routespec))))

(define (parse-path path)
  "parse the path of a route, extracting names and splats,\
  and splitting by '/'"
  (multi-map
    (split-and-decode-uri-path path)

    ; parse args like "/whatever/:path"
    (lambda (elem)
      (if (and (> (string-length elem) 0)
               (eqv? (string-ref elem 0) #\:))
        (string->symbol elem)
        elem))

    ; parse args like "/whatever/*.*"
    (lambda (elem)
      (if (and (string? elem)
               (string-contains elem "*"))
        (delete "" (intersperse '* (string-split elem #\*)))
        elem))))


(define* (make-pathmap path item #:optional (onto (make-hash-table)))
  (if (null? path)
    (hash-set! onto 'terminal item)
    (make-pathmap (cdr path) item
                  (or (hash-ref onto (pm-key (car path)))
                      (hash-set! onto (pm-key (car path))
                                 (make-hash-table)))))
  onto)

(define* (pathmap-ref table path #:optional dflt)
  (cond
    ((equal? table #f) dflt)
    ((null? path) (hash-ref table 'terminal dflt))
    (#t (pathmap-ref (or (hash-ref table (car path))
                         (hash-ref table 'default))
                     (cdr path) dflt))))


(define (pm-key elem)
  (if (symbol? elem)
    'default
    elem))
