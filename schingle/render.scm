(define-module (schingle render)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (schingle load)
  #:use-module (schingle content-type)
  #:use-module (schingle ftypes)
  #:use-module (schingle status)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 textual-ports)
  #:use-module (web uri)
  #:use-module (web request)
  #:export (*schingle-renderers*
            *schingle-served-directories*
            add-renderer render-file render
            serve-directory
            serve-directory-combinator))

(define *schingle-renderers* (make-parameter '()))

(define (add-renderer ext renderer)
  (*schingle-renderers* (acons ext renderer (*schingle-renderers*))))

(define (get-backing-file file)
  (cond
   ((file-exists? file) file)
   ((find (lambda (f) (string-prefix? (basename file) f)) (scandir (dirname file))) =>
    (lambda (f)
      (in-vicinity (dirname file) f)))
   (else #f)))

(define (do-render content filename)
  (define next-renderer (find (lambda (renderer)
                                (string-suffix? (car renderer) filename))
                              (*schingle-renderers*)))
  (if next-renderer
      (do-render ((cdr next-renderer) content)
                 (string-drop-right filename
                                    (string-length (car next-renderer))))
      (values content filename)))

(define (render-file searchdir path)
  (define file (in-vicinity searchdir path))
  (define backing-file (get-backing-file
                        (if (and (file-exists? file) (file-is-directory? file))
                            (in-vicinity file "index")
                            file)))
  (if backing-file
      (do-render (call-with-input-file backing-file get-string-all)
                 (basename backing-file))
      (values #f #f)))

(define* (render path #:optional (content-type #f))
  (define (get-content dirs)
    (if (null? dirs)
        (not-found)
        (let-values (((body filename) (render-file (car dirs) path)))
          (if body
              (content (list (or content-type
                                 (extension->content-type filename)
                                 'text/plain)) body)
              (get-content (cdr dirs))))))
  (get-content (*schingle-load-path*)))

;; serving whole directories

(define *schingle-served-directories* (make-parameter '()))

(define (serve-directory path prefix)
  (*schingle-served-directories*
   (acons prefix path
          (*schingle-served-directories*))))

(define (do-serve-directory path dirs)
  (cond
   ((null? dirs) (values #f #f))
   ((string-prefix? (caar dirs) path)
    (let-values (((body filename) (render-file (cdar dirs) (string-drop path (string-length (caar dirs))))))
      (if body
          (content (list (or (extension->content-type filename)
                       'text/plain)) body)
          (do-serve-directory path (cdr dirs)))))
   (else
    (do-serve-directory path (cdr dirs)))))

(define (serve-directory-combinator next)
  (lambda (request body)
    (define-values (response response-body)
      (do-serve-directory (uri-path (request-uri request)) (*schingle-served-directories*)))
    (cond
     ((not response)
      (next request body))
     ((not (eq? (request-method request) 'GET))
      (method-not-allowed))
     (else
      (values response response-body)))))
