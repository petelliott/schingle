(define-module (schingle ftypes)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 regex)
  #:export (extension->content-type
            content-type->extension))

; extracted from:
; https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types
(define extension-alist
  '((".aac" . audio/aac)
    (".abw" . application/x-abiword)
    (".arc" . application/x-freearc)
    (".avi" . video/x-msvideo)
    (".azw" . application/vnd.amazon.ebook)
    (".bin" . application/octet-stream)
    (".bmp" . image/bmp)
    (".bz" . application/x-bzip)
    (".bz2" . application/x-bzip2)
    (".csh" . application/x-csh)
    (".css" . text/css)
    (".csv" . text/csv)
    (".doc" . application/msword)
    (".docx"
     .
     application/vnd.openxmlformats-officedocument.wordprocessingml.document)
    (".eot" . application/vnd.ms-fontobject)
    (".epub" . application/epub+zip)
    (".gz" . application/gzip)
    (".gif" . image/gif)
    (".html" . text/html)
    (".htm" . text/html)
    (".ico" . image/vnd.microsoft.icon)
    (".ics" . text/calendar)
    (".jar" . application/java-archive)
    (".jpeg" . image/jpeg)
    (".jpg" . image/jpeg)
    (".js" . text/javascript)
    (".json" . application/json)
    (".jsonld" . application/ld+json)
    (".midi" . audio/midi)
    (".mid" . audio/midi)
    (".mjs" . text/javascript)
    (".mp3" . audio/mpeg)
    (".mpeg" . video/mpeg)
    (".mpkg" . application/vnd.apple.installer+xml)
    (".odp"
     .
     application/vnd.oasis.opendocument.presentation)
    (".ods"
     .
     application/vnd.oasis.opendocument.spreadsheet)
    (".odt"
     .
     application/vnd.oasis.opendocument.text)
    (".oga" . audio/ogg)
    (".ogv" . video/ogg)
    (".ogx" . application/ogg)
    (".otf" . font/otf)
    (".png" . image/png)
    (".pdf" . application/pdf)
    (".php" . appliction/php)
    (".ppt" . application/vnd.ms-powerpoint)
    (".pptx"
     .
     application/vnd.openxmlformats-officedocument.presentationml.presentation)
    (".rar" . application/x-rar-compressed)
    (".rtf" . application/rtf)
    (".sh" . application/x-sh)
    (".svg" . image/svg+xml)
    (".swf" . application/x-shockwave-flash)
    (".tar" . application/x-tar)
    (".tiff" . image/tiff)
    (".tif" . image/tiff)
    (".ts" . video/mp2t)
    (".ttf" . font/ttf)
    (".txt" . text/plain)
    (".vsd" . application/vnd.visio)
    (".wav" . audio/wav)
    (".weba" . audio/webm)
    (".webm" . video/webm)
    (".webp" . image/webp)
    (".woff" . font/woff)
    (".woff2" . font/woff2)
    (".xhtml" . application/xhtml+xml)
    (".xls" . application/vnd.ms-excel)
    (".xlsx"
     .
     application/vnd.openxmlformats-officedocument.spreadsheetml.sheet)
    (".xml" . application/xml)
    (".xul" . application/vnd.mozilla.xul+xml)
    (".zip" . application/zip)
    (".3gp" . video/3gpp)
    (".3g2" . video/3gpp2)
    (".7z" . application/x-7z-compressed)))

(define (reverse-alist alist)
  (map (lambda (pair)
         (cons (cdr pair) (car pair)))
       alist))

(define extension-type-table
  (alist->hash-table extension-alist))

(define type-extension-table
  (alist->hash-table (reverse-alist extension-alist)))

(define ext-reg (make-regexp "\\.[^\\.]*$"))

(define (extension->content-type filename-or-extension)
  (hash-ref extension-type-table
            (match:substring (regexp-exec ext-reg filename-or-extension))))

(define (content-type->extension content-type)
  (hash-ref type-extension-table content-type))
