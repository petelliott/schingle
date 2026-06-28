(define-module (schingle static)
  #:use-module (schingle handler)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 regex)
  #:use-module (web response)
  #:use-module (schingle ftypes)
  #:export (schingle-static-folder
            static
            file-content-type))

(define schingle-static-folder (make-parameter ""))

(define* (static file #:optional (content-type (file-content-type file))
                 #:key (transf identity))
  "load and serve a static file from the cache with content-type content-type.\
  if content-type is not provided, it will be determined from the file extention"
  (define data (catch
                 'system-error
                 (lambda ()
                   (transf
                    (call-with-input-file (string-append (schingle-static-folder) file)
                      (lambda (port)
                        (get-bytevector-all port)))))
                 (lambda (key . args)
                   #f)))
    (if data
        (values
         (build-response
          #:code 200
          #:headers `((content-type . (,content-type))))
         data)
        ((404handler) #f #f)))

(define ext-reg (make-regexp "\\.[^\\.]*$"))

(define (file-content-type file)
  "determines the content-type of a file from extension to mime type"
  (hash-ref extension-table (match:substring (regexp-exec ext-reg file))))
