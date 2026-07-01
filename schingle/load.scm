(define-module (schingle load)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 binary-ports)
  #:export (*schingle-load-path*
            schingle-include-path
            schingle-resolve-path
            call-with-schingle-input-file
            schingle-read-file
            schingle-read-file-bv))

(define *schingle-load-path* (make-parameter '()))

(define (schingle-include-path path)
  (*schingle-load-path* (cons path (*schingle-load-path*))))

(define (schingle-resolve-path path)
  (define (first-exists dirs)
    (if (null? dirs)
        #f
        (let ((f  (in-vicinity (car dirs)  path)))
          (if (file-exists? f)
              f
              (first-exists (cdr dirs))))))
  (if (absolute-file-name? path)
      path
      (first-exists (*schingle-load-path*))))

(define (call-with-schingle-input-file path proc)
  (define true-path (schingle-resolve-path  path))
  (if true-path
      (call-with-input-file true-path proc)
      (error "path could not be found in schingle's load paths" path (*schingle-load-path*))))

(define (schingle-read-file-bv path)
  (define true-path (schingle-resolve-path  path))
  (and true-path
       (call-with-input-file true-path
         get-bytevector-all)))

(define (schingle-read-file path)
  (define true-path (schingle-resolve-path  path))
  (and true-path
       (call-with-input-file true-path
         get-string-all)))
