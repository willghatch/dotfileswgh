#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; battery.scm -- the batteries, as JSON {"text", "class", "batteries"}.
;;;
;;; The text shows the system batteries as Waybar's battery module did:
;;; CAPACITY%ICON, "+" while charging, "≈" when plugged in and not charging;
;;; the class is warning at 30% and critical at 15%.  With no system battery,
;;; as on a desktop, the text is "NO_BAT" and the class empty.  Peripheral
;;; batteries (a mouse, keyboard, or headset: scope "Device") never set the
;;; class; the text only counts them, as " +N dev".
;;;
;;; batteries lists every battery, system ones first, for the popup:
;;;   [{"name", "capacity", "status", "detail", "class"}]
;;; where capacity is "85%", or the kernel's level ("Low", "Full", ...) when
;;; the device gives no percentage, and detail is the time left or "".
;;;
;;; BATTERY_SYSFS_DIR overrides /sys/class/power_supply, for trying it out.
(use-modules (ice-9 rdelim) (ice-9 ftw) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define dir (or (getenv "BATTERY_SYSFS_DIR") "/sys/class/power_supply"))
(define icons '("" "" "" "" ""))

(define (read-value battery key)
  (let ((path (string-append dir "/" battery "/" key)))
    (and (file-exists? path)
         (let ((line (false-if-exception (call-with-input-file path read-line))))
           (and (string? line) (string-trim-both line))))))

(define (read-number battery key)
  (let ((value (read-value battery key)))
    (and value (string->number value))))

(define batteries
  (filter (lambda (name) (equal? (read-value name "type") "Battery"))
          (or (scandir dir (lambda (name) (not (string-prefix? "." name)))) '())))

(define (system? battery)
  (member (or (read-value battery "scope") "System") '("System" "Unknown")))

(define system-batteries (filter system? batteries))
(define peripheral-batteries (remove system? batteries))

(define (capacity battery) (read-number battery "capacity"))
(define (status battery) (or (read-value battery "status") ""))

(define (battery-class battery)
  (let ((c (or (capacity battery) 100)))
    (cond ((and (<= c 15) (not (equal? (status battery) "Charging"))) "critical")
          ((<= c 30) "warning")
          (else ""))))

(define (bar-text battery)
  (let ((c (or (capacity battery) 0))
        (s (status battery)))
    (string-append (number->string c) "%"
                   (cond ((equal? s "Charging") "+")
                         ((member s '("Not charging" "Full")) "≈")
                         (else (list-ref icons (min 4 (quotient c 20))))))))

(define (name battery)
  "A friendly name: the manufacturer and model, else the sysfs name."
  (let* ((parts (filter (lambda (s) (and s (not (string-null? s))))
                        (list (read-value battery "manufacturer")
                              (read-value battery "model_name"))))
         (model (if (null? parts) battery (string-join parts " "))))
    (if (system? battery)
        (string-append "System battery (" model ")")
        model)))

(define (hours-text hours)
  (let* ((minutes (inexact->exact (round (* hours 60)))))
    (format #f "~d:~2,'0d" (quotient minutes 60) (remainder minutes 60))))

(define (time-left battery)
  "\"H:MM left\" or \"H:MM to full\" from energy or charge and its rate, or \"\"."
  (let* ((now (or (read-number battery "energy_now") (read-number battery "charge_now")))
         (full (or (read-number battery "energy_full") (read-number battery "charge_full")))
         (rate (let ((r (or (read-number battery "power_now") (read-number battery "current_now"))))
                 (and r (abs r))))
         (s (status battery)))
    (cond ((not (and now full rate (> rate 0))) "")
          ((equal? s "Discharging") (string-append (hours-text (/ now rate)) " left"))
          ((equal? s "Charging") (string-append (hours-text (/ (max 0 (- full now)) rate)) " to full"))
          (else ""))))

(define (describe battery)
  `((name . ,(name battery))
    (capacity . ,(let ((c (capacity battery)))
                   (if c
                       (string-append (number->string c) "%")
                       (or (read-value battery "capacity_level") "?"))))
    (status . ,(status battery))
    (detail . ,(time-left battery))
    (class . ,(if (capacity battery) (battery-class battery) ""))))

(let ((count (length peripheral-batteries)))
  (print-json
   `((text . ,(string-append
               (if (null? system-batteries)
                   "NO_BAT"
                   (string-join (map bar-text system-batteries) " "))
               (if (zero? count) "" (string-append " +" (number->string count) " dev"))))
     (class . ,(or (find (lambda (c) (not (string-null? c)))
                         (map battery-class system-batteries))
                   ""))
     (batteries . ,(map describe (append system-batteries peripheral-batteries))))))
