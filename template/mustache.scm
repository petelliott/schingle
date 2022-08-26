(define-module (schingle template mustache)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (ice-9 regex)
  #:use-module (schingle util)
  #:export (mustache-compile
            mustache-render))

(define-record-type tag
  (make-tag key escaped inverted children)
  tag?
  (key tag-key)
  (escaped tag-escaped)
  (inverted tag-inverted)
  (children tag-children))

(define tagre
  (make-regexp "\\{\\{(\\{?) *([&#^/!>]?) *([a-zA-Z0-9]+) *\\}\\}\\}?"))

(define (parse text i close-tag)
  (define nexttag (regexp-exec tagre text i))
  (if nexttag
      (let ((extram (match:substring nexttag 1))
            (marker (match:substring nexttag 2))
            (key    (match:substring nexttag 3))
            (nexti  (match:end nexttag))
            (pre    (substring text i (match:start nexttag))))
        (cond
         ((or (equal? marker "#") (equal? marker "^"))
          (let*-values (((child end) (parse text nexti key))
                        ((rest end2) (parse text end close-tag)))
            (values
             (mcons pre
                    (make-tag key #f (equal? marker "^") child)
                    rest)
             end2)))
         ((equal? marker "/")
          (unless (equal? key close-tag)
            (error "mustache: unexpected closing tag" key))
          (values (list pre) nexti))
         ((equal? marker "!")
          (let-values (((rest end) (parse text nexti close-tag)))
            (values (cons pre rest) end)))
         ((equal? marker ">")
          (error "TODO: partial support"))
         (else
          (let-values (((rest end) (parse text nexti close-tag)))
            (values (mcons pre
                           (make-tag key (not (or (equal? extram "{") (equal? marker "&"))) #f #f)
                           rest)
                    end)))))
      (if close-tag
          (error "mustache: expected closing tag, but got eof" close-tag)
          (values (list (substring text i)) #f))))

(define (mustache-compile text)
  (parse text 0 #f))

(define (tag-assoc alist key)
  (define sym (string->symbol key))
  (define (inner alist)
    (cond
     ((null? alist) #f)
     ((or (equal? (caar alist) key)
          (eq? (caar alist) sym))
      (car alist))
     (else
      (tag-assoc (cdr alist) key))))
  (inner alist))

(define (render-tag tag data)
  (define match (tag-assoc data (tag-key tag)))
  (cond
   ((not match))
   ((tag-children tag)
    (cond
     ((list? (cdr match))
      (for-each (lambda (datum)
                  (mustache-render (tag-children tag) datum))
                (cdr match)))
     ((eq? (cdr match) (not (tag-inverted tag)))
      (mustache-render (tag-children tag) data))))
   ((procedure? (cdr match))
    (display ((cdr match))))
   (else (display (cdr match)))))

(define (mustache-render template data)
  (cond
   ((null? template))
   ((tag? (car template))
    (render-tag (car template) data)
    (mustache-render (cdr template) data))
   ((string? (car template))
    (display (car template))
    (mustache-render (cdr template) data))))
