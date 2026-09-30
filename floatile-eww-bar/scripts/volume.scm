#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; volume.scm -- the default sink and source, as Waybar's pulseaudio module
;;; showed them, as JSON lines {"markup", "muted"}, printed again whenever
;;; `pactl subscribe' reports a change.
;;;   VOLUME% ICON SOURCE-VOLUME%MIC, or MUTED-ICON for a muted sink or source.
(use-modules (ice-9 popen) (ice-9 rdelim) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define muted-icon "🔇")
(define microphone-icon "")
;; By the active port's type, as Waybar picked them.
(define port-icons
  '(("headphone" . "") ("headset" . "") ("hands-free" . "")
    ("phone" . "") ("portable" . "") ("car" . "")))
;; Sinks with their own icon.
(define sink-icons
  '(("alsa_output.usb-Focusrite_Scarlett_2i2_USB_Y8JRKG723DEF0B-00.HiFi__Line1__sink"
     . "<span color='#00cc66'>℗</span>")
    ("alsa_output.usb-Focusrite_Scarlett_2i2_USB_Y8JRKG723DEF0B-00.HiFi__Line__sink"
     . "<span color='#00cc66'>℗</span>")))
;; Otherwise a speaker by volume: low, middle, high.
(define speaker-icons
  '("<span color='#00aaff'></span>" "<span color='#00aaff'></span>"
    "<span color='#00aaff'></span>"))

(define (volume-of kind)
  "The first channel's volume percentage of the default KIND, sink or source."
  (let ((line (command-output (format #f "pactl get-~a-volume @DEFAULT_~a@" kind (string-upcase kind)))))
    (and line
         (let ((i (string-index line #\%)))
           (and i (let ((start (let loop ((j i)) (if (and (> j 0) (char-numeric? (string-ref line (- j 1)))) (loop (- j 1)) j))))
                    (string->number (substring line start i))))))))

(define (muted? kind)
  (let ((line (command-output (format #f "pactl get-~a-mute @DEFAULT_~a@" kind (string-upcase kind)))))
    (and line (string-contains line "yes") #t)))

(define (active-port sink)
  "The active port of SINK, from `pactl list sinks'."
  (let loop ((lines (command-lines "pactl list sinks")) (in-sink #f))
    (cond ((null? lines) #f)
          ((string-prefix? "Name: " (string-trim (car lines)))
           (loop (cdr lines) (equal? (substring (string-trim (car lines)) 6) sink)))
          ((and in-sink (string-prefix? "Active Port: " (string-trim (car lines))))
           (substring (string-trim (car lines)) 13))
          (else (loop (cdr lines) in-sink)))))

(define (sink-icon sink volume)
  (or (assoc-ref sink-icons sink)
      (let ((port (or (and sink (active-port sink)) "")))
        (any (lambda (p) (and (string-contains-ci port (car p)) (cdr p))) port-icons))
      (list-ref speaker-icons (min 2 (quotient (or volume 0) 34)))))

(define (show)
  (let* ((sink (command-output "pactl get-default-sink"))
         (volume (volume-of "sink"))
         (source-volume (volume-of "source"))
         (source (if (muted? "source")
                     muted-icon
                     (string-append (number->string (or source-volume 0)) "%" microphone-icon))))
    (print-json
     (if (muted? "sink")
         `((markup . ,(string-append muted-icon " " source)) (muted . #t))
         `((markup . ,(string-append (number->string (or volume 0)) "% " (sink-icon sink volume) " " source))
           (muted . #f))))))

(show)
(let ((port (open-input-pipe "pactl subscribe 2>/dev/null")))
  (let loop ()
    (let ((line (read-line port)))
      (unless (eof-object? line)
        (when (or (string-contains line "sink") (string-contains line "source") (string-contains line "server"))
          (show))
        (loop)))))
