(define-module (schingle route)
  #:use-module (web uri)
  #:use-module (schingle util)
  #:use-module (ice-9 regex)
  #:export (parse-route
            compile-routes
            parse-params
            compile-routemap))

(define (parse-route routespec)
  "parse a routespec, returning a list that represents it.\
  a routespec is a method-string pair. if path is already not\
  a string, returns routespec"
  (if (string? (cdr routespec))
    (cons (car routespec) (parse-path (cdr routespec)))
    routespec))

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

(define* (compile-routes routelist #:optional dflt (onto (make-hash-table)))
  "like compile-routemap, but returns an alist of args and value pair"
  (let ((rm (compile-routemap routelist #f onto)))
    (lambda (mpath)
      (let ((ret (rm mpath)))
        (if ret
          (cons
            (parse-params (car ret) mpath)
            (cdr ret))
          dflt)))))

(define (parse-params routespec mpath)
  "produces an alist of args from the path and the spec"
  (parse-inner-params (cdr (parse-route routespec))
                      (cdr (parse-mpath mpath))))

(define (parse-inner-params route path)
  (cond
    ((null? path) (and (null? route) '()))
    ((null? route) (and (null? path) '()))
    ((symbol? (car route))
     (acons (car route) (car path)
       (parse-inner-params (cdr route) (cdr path))))
    ((and (string? (car route))
          (not (equal? (car route) (car path))))
     #f)
    ((list? (car route))
       (parse-splat-param (splat-regex (car route))
                          (cdr route) path))
    (#t (parse-inner-params (cdr route) (cdr path)))))

(define (splat-acons vals alist)
  (if (assoc-ref alist '*)
    (map
      (lambda (elem)
        (if (equal? (car elem) '*)
          (cons '* (append vals (cdr elem)))
          elem))
      alist)
    (acons '* vals alist)))

(define* (parse-splat-param splat-reg route-rest path #:optional (splat-data '()))
  (let ((match (regexp-exec splat-reg (string-join splat-data "/")))
        (rest  (parse-inner-params route-rest path)))
    (if (and match rest)
      (splat-acons
        (map
          (lambda (i)
            (match:substring match i))
          (cdr (iota (match:count match))))
        rest)
      (parse-splat-param splat-reg route-rest (cdr path) (append splat-data
                                                                 (list (car path)))))))

(define* (compile-routemap routelist #:optional dflt (onto (make-hash-table)))
  "compiles an alist keyed by routes int a function that takes a method/path pair\
  and returns a routespec/value pair. has optional default argument"
  (for-each (lambda (routeval)
              (let ((mpath (parse-route (car routeval))))
                (make-routemap mpath (cons mpath (cdr routeval)) onto)))
            routelist)
  (lambda (mpath)
    (routemap-ref onto (parse-mpath mpath) dflt)))

(define (parse-mpath mpath)
  (if (string? (cdr mpath))
      (cons (car mpath)
            (split-and-decode-uri-path (cdr mpath)))
      mpath))

(define* (make-routemap mpath item #:optional (onto (make-hash-table)))
  (make-pathmap (cdr mpath) item (or (hash-ref onto (car mpath))
                                     (hash-set! onto (car mpath)
                                                (make-hash-table))))
  onto)

(define* (routemap-ref table mpath #:optional dflt)
  (pathmap-ref (hash-ref table (car mpath))
               (cdr mpath) dflt))

(define* (make-pathmap path item #:optional (onto (make-hash-table)))
  (if (null? path)
    (hash-set! onto 'terminal item)
    (make-pathmap (cdr path) item (insert-or-get (car path) onto)))
  onto)

(define* (pathmap-ref table path #:optional dflt)
  (cond
    ((equal? table #f) dflt)
    ((null? path) (hash-ref table 'terminal dflt))
    (#t (or (pathmap-ref (or (hash-ref table (car path))
                             (hash-ref table 'default))
                         (cdr path) dflt)
            (splat-multi (hash-ref table 'splat) path dflt)))))

(define (splat-multi alist path dflt)
  (if (or (null? alist) (not alist))
    dflt
    (or (pathmap-splat (cdar alist) (caar alist) path dflt)
        (splat-multi (cdr alist) path dflt))))

(define (pathmap-splat table splat path dflt)
  (pathmap-splat-reg table (splat-regex splat) path dflt))

(define* (pathmap-splat-reg table splat-reg path #:optional dflt (splat-data '()))
  (or (and (regexp-exec splat-reg (string-join splat-data "/"))
           (pathmap-ref table path dflt))
      (if (null? path)
        dflt
        (pathmap-splat-reg table splat-reg (cdr path) dflt
                       (append splat-data (list (car path)))))))

(define (splat-regex splat)
  "returns a regex for a splat"
  (make-regexp
    (string-append
      "^"
      (string-join
        (map (lambda (elem)
               (if (equal? elem '*)
                 "(.*)"
                 elem))
             splat)
        "")
      "$")))

(define (insert-or-get key onto)
  (if (list? key)
    (or (assoc-ref (hash-ref onto 'splat) key)
        (let ((table (make-hash-table)))
          (hash-set! onto 'splat
                     (acons key table (hash-ref onto 'splat '())))
          table))
    (or (hash-ref onto (pm-key key))
        (hash-set! onto (pm-key key)
                   (make-hash-table)))))

(define (pm-key elem)
  (if (symbol? elem)
    'default
    elem))
