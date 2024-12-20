#!/usr/bin/env racket
#lang rash
(require racket/cmdline)

;;; Script containing my knowledge from experiments recording and compressing old family videos on VHS.

; CRF values range from 0 (lossless) to 51 (most lossy), defaulting to 23
; After looking at several, 25 seems the best for VHS rips - it is practically indistinguishable, and high compression can be achieved.
; Though, for a barely noticeable downgrade in quality I can go to 27 with significant gains on compression
(define crf-vhs-default 25)
(define crf crf-vhs-default)
(define speed "veryslow")
(define tune-flag '())

(define-values (input output)
  (command-line
   #:once-each
   ["--quality-mod"
    qmod
    "positive for higher compression, negative for higher quality"
    (set! crf (+ crf-vhs-default (string->number qmod)))]
   ["--speed" s "veryslow, slow, medium, fast, veryfast" (set! speed s)]
   ["--tune" t "film, animation, grain (the default is to leave the flag off)"
             (set! tune-flag (list "-tune" t))]
   #:help-labels "Quality mod -2 should be the exact quality I get from my vhs-rip script."
   #:help-labels "Quality mod +2 is still pretty good quality with significant gains on compression."
   #:args (in out)
   (values in out)))

; ffmpeg [global_ops] [input-file-ops] -i input-file [out-file-ops] out-file
; The -map 0 tells it that the following options are for input 0 (I think)
; The -c:v/a/s selects which kind of stream the following options work on.  So -c:a copy and -c:s copy mean copy all audio and subtitle streams unmodified.
ffmpeg -i $input -map 0 -c:v libx264 -crf $crf $tune-flag -preset $speed -c:a copy -c:s copy $output

; TODO - get times for conversion on slow, slower, and very slow
; For example file, crf 25:
; fast - 17:17 total, 922M
; medium - 20:26 total, 860M
; slow - 31:00 total, 858M
; slower - 49:42 total, 842M
; veryslow - 1:34:16 total, 743M

; another comparison:
; crf 25
; medium: 25:42, 1.8G
; slow: 38:35, 1.6G
; slower: 1:07:45, 1.6G

; bluray notes
; original file: 27G, ~150m
; crf 19, veryslow: 3.2G
; crf 21, veryslow: 2.4G
; crf 23, veryslow: 1.9G
; crf 25, veryslow: 1.5G
; crf 27, veryslow: 1.3G
;; All of these up to 27 frankly had good enough quality for my taste.  I didn't try anything past 27.  It seems that if the original input is high quality, ffmpeg can make a high quality compressed version at even very high compression rates.
