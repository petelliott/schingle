(define-module (schingle static)
  #:use-module (web response)
  #:use-module (schingle ftypes)
  #:use-module (schingle status)
  #:use-module (schingle load)
  #:export (static
            file-content-type))

(define* (static file #:optional (content-type (extension->content-type file))
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
