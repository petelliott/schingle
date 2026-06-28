(define-module (schingle route)
  #:use-module (web uri)
  #:use-module (web request)
  #:use-module (schingle util)
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

(define (match-route routes path)
  (if (null? routes)
      #f
      (let ((hit (regexp-exec (route-regex (car routes)) path)))
        (if hit
            (make-route-match (car routes) (match-captures hit) (route-value (car routes)))
            (match-route (cdr routes) path)))))

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
      (let ((route (match-route (routes-fn) (uri-path (request-uri request)))))
        (cond
         ((not route) (next request body))
         ((not (member (request-method request) (route-methods (route-match-route route))))
          (error "TODO: 405"))
         (else
          (apply (route-match-value route) request body (route-match-captures route))))))))
