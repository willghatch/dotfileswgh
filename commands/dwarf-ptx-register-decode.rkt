#!/usr/bin/env racket
#lang racket/base

(require
 racket/cmdline
 racket/format
 )

(define (main)
  (define number-strings
    (command-line
     #:usage-help "converts a decimal dwarf register number into PTX register name"
     #:usage-help "Can take a single number, or a list of numbers (as separate args)"
     #:usage-help "that each represent one byte of a ULEB, for convenience in looking at"
     #:usage-help "LLVM PTX output directly."
     ;; Format here: https://github.com/NVIDIA/cuda-gdb/blob/e5cf3bddae520ffb326f95b4d98ce5c7474b828b/gdb/cuda/cuda-tdep.c#L353
     #:args regx-numbers
     regx-numbers))
  (when (null? number-strings)
    (error "no register number provided"))
  (define numbers (map string->number number-strings))

  (define (register-number-to-name-string register-number)
    (let loop ([number register-number]
               [accum '()])
      (if (eq? number 0)
          (apply string accum)
          (let ([byte (bitwise-and number #xFF)]
                [rest (arithmetic-shift number -8)])
            (loop rest
                  (cons (integer->char byte) accum))))))

  (define (uleb-bytes-to-number byte-list)
    (define (high-bit-set? byte)
      (not (equal? 0 (bitwise-and #x80 byte))))
    (define (validate-bytes byte-list)
      (cond [(null? byte-list) #f]
            [(null? (cdr byte-list)) (not (high-bit-set? (car byte-list)))]
            [else (and (high-bit-set? (car byte-list))
                       (validate-bytes (cdr byte-list)))]))
    (when (not (validate-bytes byte-list))
      (error "Bad uleb128 byte list, high bits not set properly"))

    (let loop ([bytes (reverse byte-list)]
               [accum 0])
      (if (null? bytes)
          accum
          (loop (cdr bytes)
                (bitwise-ior (arithmetic-shift accum 7)
                             (bitwise-and (car bytes) #x7f))))))

  (printf "~a\n"
          (register-number-to-name-string (if (equal? 1 (length numbers))
                                              (car numbers)
                                              (uleb-bytes-to-number numbers))))
  )

(main)
