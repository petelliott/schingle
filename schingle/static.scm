(define-module (schingle static)
  #:use-module (ice-9 regex)
  #:use-module (web response)
  #:use-module (schingle ftypes)
  #:use-module (schingle status)
  #:use-module (schingle load)
  #:export (static
            file-content-type))

(define* (static file #:optional (content-type (file-content-type file))
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

(define ext-reg (make-regexp "\\.[^\\.]*$"))

(define (file-content-type file)
  "determines the content-type of a file from extension to mime type"
  (hash-ref extension-table (match:substring (regexp-exec ext-reg file))))
