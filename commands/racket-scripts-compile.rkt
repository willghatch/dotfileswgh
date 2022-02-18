#!/usr/bin/env racket
#lang rash
(require racket/string racket/list)

(define (racket-script? f)
  (and (file-exists? f)
       #{head -n 1 $f |>> string-contains? _ "racket"}))

(define user #{whoami})

(define path-dirs (remove-duplicates (string-split (getenv "PATH") ":")))

(for ([d path-dirs]
      #:when (and (directory-exists? d)
                  (equal? user #{stat -L --format=%U $d})))
  (define scripts (filter racket-script?
                          (map (λ (f) (build-path d f))
                               (directory-list d))))
  (when (not (null? scripts))
    {
     echo raco make $scripts
     raco make $scripts
     }))
