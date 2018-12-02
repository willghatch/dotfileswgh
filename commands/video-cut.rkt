#!/usr/bin/env racket
#lang rash
(require racket/cmdline)

(define-values
  (start end input output)
  ;; Time is hh:mm:ss.<ms>
  (command-line #:args (start-time end-time input-file output-file)
                (values start-time end-time input-file output-file)))

ffmpeg -ss $start -i $input -t $end -acodec copy -vcodec copy $output
