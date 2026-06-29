(define-module (schingle route)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (schingle util)
  #:use-module (schingle status)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:export (add-route
            match-route
            route-match-route
            route-match-captures
            route-match-value
            routes->combinator))

(define-record-type route
  (make-route methods regex value)
  route?
  (methods route-methods)
  (regex route-regex)
  (value route-value))

(define-record-type route-match
  (make-route-match route captures value)
  route-match?
  (route route-match-route)
  (captures route-match-captures)
  (value route-match-value))

(define (add-route routes methods path value)
  (cons (path->route methods path value) routes))

(define (match-captures m)
  (define (inner n)
    (if (= n (match:count m))
        '()
        (cons (match:substring m n)
              (inner (+ n 1)))))
  (inner 1))

(define (match-route routes method path mna)
  (if (null? routes)
      mna
      (let ((path-match (regexp-exec (route-regex (car routes)) path))
            (method-match (member method (route-methods (car routes)))))
        (cond
         ((and path-match method-match)
          (make-route-match (car routes) (match-captures path-match) (route-value (car routes))))
         (path-match
          (match-route (cdr routes) method path 'method-not-allowed))
         (else
          (match-route (cdr routes) method path mna))))))

(define (path->route method path value)
  (define parts (split-and-decode-uri-path path))
  (make-route method
              (make-regexp (string-append "^/*"
                                          (string-join
                                           (map (lambda (part)
                                                  (if (equal? #\: (string-ref part 0))
                                                      "([^/]+)"
                                                      part))
                                                parts)
                                           "/+")
                                          "/*$"))
              value))

(define (routes->combinator routes-fn)
  (lambda (next)
    (lambda (request body)
      (let ((route (match-route (routes-fn) (request-method request) (uri-path (request-uri request)) #f)))
        (cond
         ((not route) (next request body))
         ((eq? route 'method-not-allowed)
          (method-not-allowed))
         (else
          (apply (route-match-value route) request body (route-match-captures route))))))))
