#!/usr/bin/env racket
#lang racket/base

(require
 racket/cmdline
 racket/string
 racket/file
 json
 setup/dirs
 basedir
 net/sendurl
 )

(define browser "firefox")
(define same-terminal? #f)

(define search-term
  (command-line
   #:once-each
   [("--browser") b "which browser to use" (set! browser b)]
   [("--in-terminal") "launch the browser in the same terminal?"
                      (set! same-terminal? #t)]
   #:args (term)
   term
   ))


(struct sdata
  (term url from)
  #:transparent)

(define (doc-dir->search-data docdir)
  (define path (build-path docdir "search/plt-index.js"))
  (define indexf
    (if (file-exists? path)
        (open-input-file path)
        (open-input-string "var plt_search_data = []")))
  (void (regexp-match #px"var plt_search_data =" indexf))

  ;; this array has an entry of four items for each index link:
  ;; - text is a string holding the indexed text
  ;; - url holds the link (">" prefix means relative to plt_main_url)
  ;; - html holds either a string, or [idx, html] where idx is an
  ;;   index into plt_span_classes (note: this is recursive)
  ;; - from_lib is an array of module names for bound identifiers,
  ;;   or the string "module" for a module entry
  (define search-data (read-json indexf))
  (close-input-port indexf)
  search-data)

(define (doc-dir->matches doc-dir)
  (define search-data (doc-dir->search-data doc-dir))

  (define search-matches (filter (λ (item) (regexp-match search-term (car item)))
                                 search-data))
  (define sorted-matches (sort search-matches <
                               #:key (λ (item) (string-length (car item)))
                               #:cache-keys? #t))
  ;; TODO - multi search term
  ;; TODO - maybe match approximate results (add things that don't match but are close)

  (define (prefixed-url->url u)
    (if (string-prefix? u ">")
        (string-replace u ">" (string-append (path->string doc-dir) "/") #:all? #f)
        u))


  (define matches-with-urls
    (map (λ (item) (sdata (car item)
                          (prefixed-url->url (cadr item))
                          (cadddr item)))
         sorted-matches))

  matches-with-urls)



(define (format-html matches)
  (printf "<h1>matches:</h1>\n")
  (map (λ (m) (printf "<p><a href=\"file://~a\">~a</a> from lib: ~a</p>\n"
                      (sdata-url m) (sdata-term m) (sdata-from m)))
       matches))

(define outfile (writable-runtime-file
                 (string-append "racket-doc-search_"
                                search-term
                                "_"
                                (number->string (current-seconds))
                                ".html")
                 #:program "racket-docs"))

(make-parent-directory* outfile)
(define outport (open-output-file outfile))
(void
 (parameterize ([current-output-port outport])
   (format-html (append (doc-dir->matches (find-doc-dir))
                        (doc-dir->matches (find-user-doc-dir))))))
(close-output-port outport)

(define (schedule-file-deletion)
      (define-values (atproc out in err)
        (subprocess #f
                    #f
                    #f
                    (find-executable-path "at")
                    "now" "+" "30" "minutes"))
      (fprintf in "rm ~a" outfile))

(define (launch-terminal-browser b)
  (define-values (sproc out in err)
    (subprocess (current-output-port)
                (current-input-port)
                (current-error-port)
                (find-executable-path b)
                outfile))
  (subprocess-wait sproc))

(if same-terminal?
    (launch-terminal-browser browser)
    (let ()
      (define browser-setting (cons (string-append browser " ") ""))
      (external-browser browser-setting)
      (send-url (string-append "file://" (path->string outfile)))
      (schedule-file-deletion)))
