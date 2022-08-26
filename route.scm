(define-module (schingle route)
  #:use-module (web uri)
  #:use-module (schingle util)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:export (make-router
            add-route!
            match-route
            route-match-route
            route-match-captures
            route-match-value))

(define-record-type route
  (make-route method regex value)
  route?
  (method route-method)
  (regex route-regex)
  (value route-value))

(define-record-type router
  (make-router-inner routes)
  router?
  (routes router-routes router-set-routes!))

(define-record-type route-match
  (make-route-match route captures value)
  route-match?
  (route route-match-route)
  (captures route-match-captures)
  (value route-match-value))

(define (make-router)
  (make-router-inner '()))

(define (add-route! router method path value)
  (router-set-routes! router (cons (path->route method path value)
                                   (router-routes router))))

(define (match-captures m)
  (define (inner n)
    (if (= n (match:count m))
        '()
        (cons (match:substring m n)
              (inner (+ n 1)))))
  (inner 1))

(define (match-route router method path)
  (define (inner routes)
    (if (null? routes)
        #f
        (let ((hit (and (eq? method (route-method (car routes)))
                        (regexp-exec (route-regex (car routes)) path))))
          (if hit
              (make-route-match (car routes) (match-captures hit) (route-value (car routes)))
              (inner (cdr routes))))))
  (inner (router-routes router)))

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
