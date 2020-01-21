(define-module (schingle template)
  #:use-module (sxml simple)
  #:export (tag-ref
            tag-let
            bind-tag
            define-tag))


(define local-tags (make-parameter '()))
(define global-tags (make-hash-table))

(define (bind-tag sym val)
  (hash-set! global-tags sym val))

(define (tag-ref sym)
  (or (assoc-ref (local-tags) sym)
      (hash-ref global-tags sym)))

(define (pcons alist . rest)
  (if (null? rest)
      alist
      (apply pcons (acons (caar rest) (cdar rest) alist)
             (cdr rest))))

(define-syntax-rule (tag-let ((key val) ...) body* ...)
  (parameterize ((tags (pcons (tags) (cons 'key val) ...))) body* ...))

(define-syntax define-tag
  (syntax-rules ()
    ((_ (name args* ...) body* ...)
     (bind-tag 'name (lambda (args* ...)
                       body* ...)))
    ((_ name val)
     (bind-tag 'name val))))
