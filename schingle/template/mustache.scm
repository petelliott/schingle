(define-module (schingle template mustache)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 regex)
  #:use-module (schingle util)
  #:use-module (schingle load)
  #:export (mustache-compile
            mustache-compile-string))

(define tagre
  (make-regexp "\\{\\{(\\{?) *([&#^/>$<]?) *([a-zA-Z0-9]+) *\\}\\}\\}?"))

(define (tag-assoc alist key)
  (define sym (string->symbol key))
  (define (inner alist)
    (cond
     ((null? alist) #f)
     ((or (equal? (caar alist) key)
          (eq? (caar alist) sym))
      (cdar alist))
     (else
      (tag-assoc (cdr alist) key))))
  (inner alist))

(define text (make-parameter #f))
(define text-i (make-parameter #f))

(define (tag-noescape? match)
  (equal? (match:substring match 1) "{"))

(define (tag-type match)
  (match:substring match 2))

(define (tag-key match)
  (match:substring match 3))

(define (parse-prefix nexttag)
  (define pre (substring (text) (text-i) (match:start nexttag)))
  (text-i (match:end nexttag))
  pre)


(define (parse-body close-tag)
  (define nexttag (regexp-exec tagre (text) (text-i)))
  (cond
   ((and (not nexttag) close-tag)
    (error "mustache: expected closing tag, but got eof" close-tag))
   ((not nexttag)
    (let ((rest (substring (text) (text-i))))
      (lambda (data blocks) rest)))
   ((equal? (tag-type nexttag) "/")
    (unless (equal? (tag-key nexttag) close-tag)
      (error "mustache: unexpected closing tag" (tag-key nexttag)))
    (let ((pre (parse-prefix nexttag)))
      (lambda (data blocks) pre)))
   (else
    (let ((pre (parse-prefix nexttag))
          (t (parse-tag nexttag))
          (post (parse-body close-tag)))
      (lambda (data blocks)
        (string-append pre (t data blocks) (post data blocks)))))))

(define (render-datum datum)
  ;; TODO: escape html
  (cond
   ((not datum) "")
   ((procedure? datum) (datum))
   (else (object->string datum display))))

(define set-block (make-parameter #f))

(define (parse-tag nexttag)
  (define type (tag-type nexttag))
  (define key (tag-key nexttag))
  (cond
   ;; variables
   ((equal? type "")
    (lambda (data blocks)
      ;; TODO: escape html entities
      (render-datum (tag-assoc data key))))
   ;; sections
   ((equal? type "#")
    (let ((body (parse-body key)))
      (lambda (data blocks)
        (define datum (tag-assoc data key))
        (cond
         ((vector? datum)
          (apply string-append (map (lambda (d) (body d blocks)) (vector->list datum))))
         ((procedure? datum)
          ;; TODO: pass literal block unredered
          (datum (body data blocks)))
         ((not datum) "")
         (else (body datum blocks))))))
   ;; inverted sections
   ((equal? type "^")
    (let ((body (parse-body key)))
      (lambda (data blocks)
        (define datum (tag-assoc data key))
        (if (or (not datum) (eq? datum '()))
            (body data blocks)
            ""))))
   ;; comments
   ((equal? type "!")
    ;; TODO: comments are broken
    (lambda (data body) ""))
   ;; partials
   ((equal? type ">")
    ;; TODO: maybe this has to be expanded at runtime? idk
    (mustache-compile-blocks (string-append key ".mustache")))
   ;; blocks
   ((equal? type "$")
    (let ((body (parse-body key)))
      (if (set-block)
          (set-block (cons (cons key body) (set-block))))
      (lambda (data blocks)
        (define override (tag-assoc blocks key))
          (if override
              (override data blocks)
              (body data blocks)))))
   ;; parents
   ((equal? type "<")
    (let ((parent (mustache-compile-blocks (string-append key ".mustache")))
          (blocks2 (parameterize ((set-block (or (set-block) '())))
                     (parse-body key)
                     (set-block))))
      (lambda (data blocks)
        (parent data blocks2))))
   (else
    (error "mustache: unrecognized tag type" type))))

(define (mustache-compile-string-blocks str)
  (parameterize ((text str)
                 (text-i 0))
    (parse-body #f)))

(define (mustache-compile-blocks filename)
  (define text (schingle-read-file filename))
  (unless text
    (error "mustache: file not found on schingle's load path" filename))
  (mustache-compile-string-blocks text))

(define (mustache-compile-string str)
  (define compiled (mustache-compile-string-blocks str))
  (lambda (data) (compiled data '())))

(define (mustache-compile filename)
  (define compiled (mustache-compile-blocks filename))
  (lambda (data) (compiled data '())))
