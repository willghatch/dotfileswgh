#!/usr/bin/env racket
#lang racket/base

; A certain phone camera names files MMDDYYHHMM.jpg or .mp4, or MMDDYYHHMMa.mp4, etc.  This is stupid.  For jpeg files I already have a rename script that uses metadata, but I don't already know how to get the metadata for mp4 files, and I want to solve this problem now.  So here is a quick script using regex matching...

(require racket/cmdline
         racket/match
         racket/system
         )

(define file (command-line #:args (file) (values file)))

(define r-matched
  (regexp-match (pregexp "(\\d\\d)(\\d\\d)(\\d\\d)(\\d\\d\\d\\d)(.?).mp4")
                file))

(define new-file
  (match r-matched
    [(list full-match mm dd yy hhmm maybe-extra)
     (define extra-part
       (match maybe-extra
         ["" ""]
         [else (string-append "_" maybe-extra)]))
     (string-append "20" yy "-" mm "-" dd "_" hhmm extra-part ".mp4")]
    [else #f]))

(void (if new-file
          (begin
            ;; print the command as well as doing it...
            (system*
             (find-executable-path "echo")
             (find-executable-path "mv") "-i" file new-file)
            (system* (find-executable-path "mv") "-i" file new-file)
            )
          (printf "not moving ~a, not matched.\n" file)))

