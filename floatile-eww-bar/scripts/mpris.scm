#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; mpris.scm -- the playing media, as Waybar's mpris module showed it, as
;;; JSON lines {"icon", "text", "paused", "shown"}: icon "▶" while playing and
;;; "⏸" while paused, and text "ARTIST - TITLE", 40 characters at most.
(use-modules (ice-9 popen) (ice-9 rdelim) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define (show line)
  (let* ((tab (string-index line #\tab))
         (status (if tab (substring line 0 tab) ""))
         (text (truncate-text (if tab (substring line (+ tab 1)) "") 40)))
    (print-json
     (cond ((equal? status "Playing") `((icon . "▶") (text . ,text) (paused . #f) (shown . #t)))
           ((equal? status "Paused") `((icon . "⏸") (text . ,text) (paused . #t) (shown . #t)))
           (else '((icon . "") (text . "") (paused . #f) (shown . #f)))))))

(show "")
(let ((port (open-input-pipe
             "playerctl --follow metadata --format '{{status}}\t{{artist}} - {{title}}' 2>/dev/null")))
  (let loop ()
    (let ((line (read-line port)))
      (unless (eof-object? line)
        (show line)
        (loop)))))
