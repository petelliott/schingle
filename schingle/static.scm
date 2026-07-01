(define-module (schingle static)
  #:use-module (web response)
  #:use-module (web request)
  #:use-module (web uri)
  #:use-module (ice-9 binary-ports)
  #:use-module (schingle ftypes)
  #:use-module (schingle status)
  #:use-module (schingle load)

  #:export (static
            *schingle-served-directories*
            serve-directory
            get-directory-file
            serve-directory-combinator))

(define* (static file #:optional (content-type (or (extension->content-type file) 'text/plain))
                 #:key (transf identity))
  "load and serve a static file from the cache with content-type content-type.\
  if content-type is not provided, it will be determined from the file extention"
  (define data (transf (schingle-read-file-bv file)))
  (if data
      (values
       (build-response
        #:code 200
        #:headers `((content-type . (,content-type))))
       data)
      (not-found)))

(define *schingle-served-directories* (make-parameter '()))

(define* (serve-directory path prefix #:optional (transformers '()))
  (*schingle-served-directories*
   (cons (list prefix path transformers)
         (*schingle-served-directories*))))

(define (get-file-for-path path dirs)
  (cond
   ((null? dirs) (values  #f #f))
   ((string-prefix? (caar dirs) path)
    (values
     (in-vicinity (cadar dirs) (substring path (string-length (caar dirs))))
     (caddar dirs)))
   (else
    (get-file-for-path path (cdr dirs)))))

(define (default-transformer file)
  (values
   (build-response
    #:code 200
    #:headers `((content-type . (,(or (extension->content-type file) 'text/plain)))))
   (call-with-input-file file get-bytevector-all)))

(define (extension-assoc-ref alist file)
  (cond
   ((null? alist) #f)
   ((string-suffix? (caar alist) file)
    (cdar alist))
   (else (extension-assoc-ref (cdr alist) file))))

(define (get-directory-file path)
  (define-values (file transformers)
    (get-file-for-path path (*schingle-served-directories*)))
  (if (file-exists? file)
      ((or (extension-assoc-ref transformers file)
           default-transformer)
       file)
      #f))

(define (serve-directory-combinator next)
  (lambda (request body)
    (define-values (file transformers)
      (get-file-for-path (uri-path (request-uri request)) (*schingle-served-directories*)))
    (cond
     ((not (file-exists? file))
      (next))
     ((not (eq? (request-method request) 'GET))
      (method-not-allowed))
     (else
      ((or (extension-assoc-ref transformers file)
           default-transformer)
       file)))))
