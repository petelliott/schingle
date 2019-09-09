(define-module (schingle static)
  #:use-module (schingle handler)
  #:use-module (ice-9 binary-ports)
  #:use-module (web response)
  #:export (schingle-static-cache
            static))

(define schingle-static-cache (make-parameter
                                (make-hash-table)))

(define* (static content-type file
                 #:key (transf identity) (cache (schingle-static-cache)))
  (let ((data
          (or
            (hash-ref cache (cons file transf))
            (catch
              'system-error
              (lambda ()
                (hash-set! cache (cons file transf)
                           (transf
                             (call-with-input-file
                             file
                             (lambda (port)
                               (get-bytevector-all port))))))
              (lambda (key . args)
                #f)))))
    (if data
      (values
        (build-response
          #:code 200
          #:headers `((content-type . (,content-type))))
        data)
      (404handler #f #f))))

