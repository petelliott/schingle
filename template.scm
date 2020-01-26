(define-module (schingle template)
  #:use-module (sxml simple)
  #:export (tag-ref
            tag-let
            define-tag
            apply-template
            apply-template-file))


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
  (parameterize ((local-tags (pcons (local-tags) (cons 'key val) ...)))
                 body* ...))

(define-syntax define-tag
  (syntax-rules ()
    ((_ (name args* ...) body* ...)
     (bind-tag 'name (lambda* (args* ...)
                       body* ...)))
    ((_ (name args* ... . rest) body* ...)
     (bind-tag 'name (lambda* (args* ... . rest)
                       body* ...)))
    ((_ name val)
     (bind-tag 'name val))))

(define (has-attribs elem)
  (and (not (null? (cdr elem)))
       (pair? (cadr elem))
       (eq? '@ (caadr elem))))

(define (attribs->args attribs)
  (cond
   ((not attribs) '())
   ((null? attribs) '())
   ((eq? (car attribs) '@)
    (attribs->args (cdr attribs)))
   (else
    (cons (symbol->keyword (caar attribs))
          (cons (cadar attribs)
                (attribs->args (cdr attribs)))))))

(define (node-attribs node)
  (and (has-attribs node)
       (cadr node)))

(define (node-body node)
  (if (has-attribs node)
      (cddr node)
      (cdr node)))

(define (make-node tag attribs body)
  (if attribs
      (cons tag (cons attribs body))
      (cons tag body)))

(define (apply-template template)
  (cond
   ((not (pair? template)) template)
   ((tag-ref (car template))
    (let ((val (tag-ref (car template))))
      (if (procedure? val)
          (apply (tag-ref (car template))
                 (map apply-template (node-body template))
                 (attribs->args (node-attribs template)))
          val)))
   (else (make-node (car template)
                    (node-attribs template)
                    (map apply-template (node-body template))))))

;; TODO: cache files
(define (apply-template-file fname)
  (call-with-input-file fname
    (lambda (port)
      (apply-template (xml->sxml port)))))
