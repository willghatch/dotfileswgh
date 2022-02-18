#lang racket/base

;; TODO - Ultimately this should provide a nice suite of exec functions and be polished and published on the racket package repo.  But I just wanted to use it quickly so I ripped it out of my rackterm package and plopped it here.  Some time when I want to read the man pages and FFI docs I'll get execvpe working, add docs, etc.

(provide execv)

(require ffi/unsafe)

(define (execv command . args)
  (define execv
    (get-ffi-obj "execv" #f
                 (_fun (file : _string)
                       (argv : (_array/list _string (+ 2 (length args))))
                       -> (ret : _int)
                       -> (error (format "execv failed (returned ~a)" ret)))))
  (define argv (append (list command) args (list #f)))
  (execv command argv))

