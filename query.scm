(define-module (schingle query)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:export (query->alist
            alist->query))


(define (query->alist qstring)
  "converts an application/x-www-form-urlencoded query string to an alist"
  (map (lambda (kv)
         (let ((split (string-split kv #\=)))
           (cons
             (uri-decode (first split))
             (uri-decode (second split)))))
       (string-split qstring #\&)))

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

(define (to-string obj)
  (call-with-output-string
    (lambda (port)
      (display obj port))))

