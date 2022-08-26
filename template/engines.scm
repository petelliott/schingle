(define-module (schingle template engines)
  #:use-module (srfi srfi-9)
  #:export (template-engine?
            te-regex
            te-compile
            te-render
            register-template-engine
            template-engines))

(define-record-type template-engine
  (make-template-engine regex compile render)
  template-engine?
  (regex te-regex)
  (compile te-compile)
  (render te-render))

(define template-engines (make-parameter '()))

(define (register-template-engine regex compile render)
  (template-engines
   (cons (make-template-engine regex compile render)
         template-engines)))
