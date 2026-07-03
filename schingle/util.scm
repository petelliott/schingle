;;; utilities that are not schingle specific
(define-module (schingle util)
  #:use-module (sxml simple)
  #:use-module (ice-9 ftw)
  #:export (mcons
            xml-escape
            delete-directory
            mkdir-if-not-exists
            mkdir-p))

(define (mcons . rest)
  (if (null? (cdr rest))
      (car rest)
      (cons (car rest)
            (apply mcons (cdr rest)))))

(define (xml-escape str)
  (with-output-to-string
    (lambda () (sxml->xml str))))

(define (delete-directory dir)
  (if (file-exists? dir)
      (nftw dir
            (lambda (filename statino flag base level)
              (case flag
                ((directory-processed)
                 (rmdir filename))
                (else
                 (delete-file filename)))
              #t)
            'depth)))

(define (mkdir-if-not-exists dir)
  (unless (file-exists? dir)
    (mkdir dir)))

(define (mkdir-p dir)
  (unless (file-exists? dir)
    (mkdir-p (dirname dir))
    (mkdir dir)))
