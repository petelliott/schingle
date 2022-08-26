(define-module (schingle template)
  #:use-module (srfi srfi-1)
  #:use-module (schingle cache)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (schingle template engines)
  #:use-module (schingle template mustache)
  #:use-module (schingle content-type)
  #:export (schingle-template-folder
            register-template-engine
            compile-template
            render-template
            template))

(define schingle-template-folder (make-parameter ""))

(define (path filename)
  (string-append (schingle-template-folder) filename))

(define (compile-template filename engine)
  ((te-compile engine)
   (call-with-input-file (path filename)
     (lambda (port)
       (get-string-all port)))))

(define (render-template filename data)
  (define compiled
    (cached 'template filename
            (lambda ()
              (define engine (or (find (lambda (engine)
                                         (string-match (te-regex engine) filename))
                                       (template-engines))
                                 (error "could not find template engine for file" filename)))
              (define compiled (compile-template filename engine))
              (lambda (data)
                (call-with-output-string
                 (lambda (port)
                   (parameterize ((current-output-port port))
                     ((te-render engine) compiled data))))))))
  (compiled data))

(define (template filename data . rest)
  (values
   (apply build-content-response '(text/html) rest)
   (render-template filename data)))
