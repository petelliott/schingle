(define-module (schingle query)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (schingle util)
  #:use-module (schingle combinators)
  #:export (query->alist
            alist->query
            request-query
            *query-string*
            query
            query-string-combinator))

(define (empty-split str char)
  "like string-split, but empty strings and #f become '()"
  (if (or (not str) (string-null? str))
    '()
    (string-split str char)))

(define (query->alist qstring)
  "converts an application/x-www-form-urlencoded query string to an alist"
  (map (lambda (kv)
         (let ((split (string-split kv #\=)))
           (cons
             (uri-decode (first split))
             (uri-decode (second split)))))
       (empty-split qstring #\&)))

(define (alist->query alist)
  "converts an alist to an application/x-www-form-urlencoded query string"
  (string-join
    (map (lambda (pair)
           (string-append
             (uri-encode (to-string (car pair)))
             "="
             (uri-encode (to-string (cdr pair)))))
         alist)
    "&"))

(define (request-query request)
  "returns the request's query string as an alist"
  (query->alist (uri-query (request-uri request))))

(define *query-string* (make-parameter #f))

(define (query-string-combinator next)
  (lambda (request body)
    ;; we don't  want to parse the query string unless we use it. if so, only parse once
    (parameterize ((*query-string* request))
      (next request body))))

(define (query key)
  (if (request? (*query-string*))
      (*query-string* (request-query (*query-string*))))
  (assoc-ref (*query-string*) key))
