(define-module (schingle build cachebust)
  #:use-module (schingle context)
  #:use-module (schingle ftypes)
  #:use-module (schingle util)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 iconv)
  #:use-module (gcrypt hash)
  #:export (*schingle-cachebusting-regexes*
            cachebust-static-dir))

(define *schingle-cachebusting-regexes*
  (make-schingle-parameter
   '((".html"
      "(<link [^>]*href=\")([^\"]*)(\"[^>]*>)"
      "(<script [^>]*src=\")([^\"]*)(\"[^>]*>)")
     (".css"
      "(url\\(\"?'?)([^\"'\\)]*)('?\"?\\))"
      "(@import \")([^\"]*)(\")"
      "(@import ')([^']*)(')"))))

(define* (string-match-all pattern str #:optional (start 0))
  (define m (string-match pattern str start))
  (if m
      (cons m (string-match-all pattern str (match:end m)))
      '()))

(define (match-cb-regexes filename content)
  #;(define str (call-with-input-file filename
                (lambda (port)
                  (get-string-all port))))
  (define regexes (assoc-ref (*schingle-cachebusting-regexes*) (file-extension filename)))
  (if regexes
      (fold append '()
            (map (lambda (r) (string-match-all r content)) regexes))
      '()))

(define (replace-cb-regexes filename content table regexes)
  (define (inner content regexes)
    (if (null? regexes)
        content
        (inner (regexp-substitute/global #f (car regexes) content
                                         'pre 1 (lambda (m)
                                                (hash-ref table (match:substring m 2)
                                                          (match:substring m 2)))
                                         3 'post)
               (cdr regexes))))
  (inner content regexes))


(define (file-dependencies filename content)
  (map (cut match:substring <> 2)
       (match-cb-regexes filename content)))

(define (hash->tag hash)
  (number->string
   (bytevector-u64-ref hash 0 (endianness big))
   16))


(define (tag content)
  (hash->tag (sha256 (string/bytevector->bytevector content))))

(define (tag-file filename)
  (hash->tag (file-hash (hash-algorithm sha256) filename)))


(define (cachebust-1 root prefix filename table)
  (define src (in-vicinity root filename))
  (define precompiled (hash-ref table filename))
  (define regexes (assoc-ref (*schingle-cachebusting-regexes*) (file-extension filename)))
  (cond
   (precompiled precompiled)
   ((or (not (absolute-file-name? filename)) ; TODO: figure out relative paths correctly
        (not (file-exists? src)))
    #f)
   ((not regexes)
    (let* ((hash (tag-file src))
           (target (string-append prefix (tag-filename filename hash)))
           (dst (in-vicinity root target)))
      (copy-file-p src dst)
      (hash-set! table filename target)))
   (else
    ;; TODO: don't read files we can't parse
    (let* ((content (call-with-input-file src
                      (lambda (port)
                        (get-string-all port)))))
      (for-each (cut cachebust-1 root prefix <> table)
                (file-dependencies filename content))
      (let* ((compiled-content (replace-cb-regexes filename content table regexes))
             (hash (tag content))
             (target (string-append prefix (tag-filename filename hash)))
             (dst (in-vicinity root target)))
        (mkdir-p (dirname dst))
        (call-with-output-file dst
          (cut display compiled-content <>))
        (call-with-output-file src
          (cut display compiled-content <>))
        (hash-set! table filename target))))))

(define (get-static-files-tree tree rel)
  (define name (car tree))
  (define stat (cadr tree))
  (define children (cddr tree))
  (define rel-path (if rel (in-vicinity rel name) "/"))
  (if (not (null? children))
      (fold append '()
            (map (cut get-static-files-tree <> rel-path)
                 children))
      (list (in-vicinity rel name))))

(define (get-static-files root)
  (get-static-files-tree (file-system-tree root) #f))

(define* (cachebust-static-dir dir #:optional (prefix "/_immutable"))
  (define table (make-hash-table))
  (for-each (cut cachebust-1 dir prefix <> table)
            (get-static-files dir))
  table)
