#!/usr/bin/env racket
#lang racket/base

(require
 racket/cmdline
 )

(define (main)
  (define number-string
    (command-line
     #:usage-help "converts a decimal dwarf register number into PTX register name"
     ;; Format here: https://github.com/NVIDIA/cuda-gdb/blob/e5cf3bddae520ffb326f95b4d98ce5c7474b828b/gdb/cuda/cuda-tdep.c#L353
     #:args (regx-number)
     regx-number))
  (define number-number (string->number number-string))
  (define register-name-string
    (let loop ([number number-number]
               [accum '()])
      (if (eq? number 0)
          (apply string accum)
          (let ([byte (bitwise-and number #xFF)]
                [rest (arithmetic-shift number -8)])
            (loop rest
                  (cons (integer->char byte) accum))))))
  (printf "~a\n" register-name-string)
  )

(main)
