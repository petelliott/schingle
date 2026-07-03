(define-module (schingle build)
  #:use-module (schingle util)
  #:use-module (schingle ftypes)
  #:use-module (schingle render)
  #:use-module (schingle context)
  #:use-module (web response)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-11)
  #:export (*schingle-static-routes*
            build-static-directory))

(define *schingle-static-routes* (make-schingle-parameter '()))

(define (build-static-directory target)
  (delete-directory target)
  (mkdir target)
  (for-each (lambda (dirspec)
              (build-served-directory dirspec target))
            (*schingle-served-directories*))
  (for-each (lambda (route) (build-static-routes route target)) (*schingle-static-routes*)))


(define (do-render-file src-dir target-dir src-filename)
  (define-values (content target-filename) (render-file src-dir src-filename))
  (call-with-output-file (in-vicinity target-dir target-filename)
    (lambda (port)
      (display content port))))

(define (build-served-directory dirspec target)
  (define prefix (car dirspec))
  (define path (cdr dirspec))
  (define path-len (string-length path))
  (ftw path
       (lambda (filename statinfo flag)
         (define rel (substring filename path-len))
         (define dest (in-vicinity target (in-vicinity prefix rel)))
         (case flag
           ((regular)
            (do-render-file (dirname filename) (dirname dest) (basename filename)))
           ((directory)
            (mkdir-if-not-exists dest))
           (else
            (error "can't walk file type" flag)))
         #t)))

(define (content-to-file content port)
  (cond
   ((string? content) (display content port))
   ((bytevector? content) (put-bytevector port content))
   ((procedure? content) (content port))))

(define (build-static-routes route target)
  (define filename (in-vicinity target (car route)))
  (define response (cadr route))
  (define content (caddr route))
  (define content-type (car (response-content-type response)))

  (call-with-output-file
      (if (eq? (extension->content-type filename) content-type)
          filename
          (begin
            (mkdir-p filename)
            (in-vicinity filename (string-append "index" (or (content-type->extension content-type)
                                                             ".txt")))))
    (lambda (port)
      (content-to-file content port))))
