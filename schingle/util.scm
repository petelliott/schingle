;;; utilities that are not schingle specific
(define-module (schingle util)
  #:use-module (rnrs bytevectors)
  #:use-module (sxml simple)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 iconv)
  #:export (mcons
            xml-escape
            delete-directory
            mkdir-if-not-exists
            mkdir-p
            copy-file-p
            string/bytevector->string
            string/bytevector->bytevector
            maybe))

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
                ((symlink)
                 (delete-file filename))
                (else
                 (delete-file filename)))
              #t)
            'depth 'physical)))

(define (mkdir-if-not-exists dir)
  (unless (file-exists? dir)
    (mkdir dir)))

(define (mkdir-p dir)
  (unless (file-exists? dir)
    (mkdir-p (dirname dir))
    (mkdir dir)))

(define (copy-file-p src dst)
  (mkdir-p (dirname dst))
  (copy-file src dst))

(define (string/bytevector->string string-or-bytevector)
  (if (string? string-or-bytevector)
      string-or-bytevector
      (bytevector->string string-or-bytevector "UTF-8")))

(define (string/bytevector->bytevector string-or-bytevector)
  (if (bytevector? string-or-bytevector)
      string-or-bytevector
      (string->bytevector string-or-bytevector "UTF-8")))

(define (maybe proc value)
  (if value
      (proc value)
      value))
