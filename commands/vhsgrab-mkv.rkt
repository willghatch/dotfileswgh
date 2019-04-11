#!/usr/bin/env racket
#lang rash
(require racket/cmdline)

(define record-hours 2.5)
(define title
  (command-line
   #:once-each
   ["--hours" hours
              "how long in hours to record (default 2.5)"
              (set! record-hours (string->number hours))]
   #:args (title)
   title))
(define record-seconds (inexact->exact (round (* 60 60 record-hours))))

cd $HOME
mkdir -p "vhsgrabs"
cd "vhsgrabs"

(define pline {dvgrab -format dv1 - | ffmpeg -f dv -i - (string-append title ".mkv") &bg})

sleep $record-seconds

(pipeline-kill pline)

