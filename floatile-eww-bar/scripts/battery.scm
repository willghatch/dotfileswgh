#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; battery.scm -- the batteries, as Waybar's battery module showed them, as
;;; JSON {"shown", "text", "class"}: CAPACITY%ICON, "+" while charging, "≈"
;;; when plugged in and not charging; warning at 30%, critical at 15%.
(use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 ftw) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define dir "/sys/class/power_supply")
(define icons '("" "" "" "" ""))

(define (read-value battery key)
  (let ((path (string-append dir "/" battery "/" key)))
    (and (file-exists? path) (call-with-input-file path read-line))))

(define batteries
  (filter (lambda (name) (equal? (read-value name "type") "Battery"))
          (or (scandir dir (lambda (name) (not (string-prefix? "." name)))) '())))

(define (describe battery)
  (let* ((capacity (or (string->number (or (read-value battery "capacity") "")) 0))
         (status (or (read-value battery "status") "")))
    (cons (string-append (number->string capacity) "%"
                         (cond ((equal? status "Charging") "+")
                               ((member status '("Not charging" "Full")) "≈")
                               (else (list-ref icons (min 4 (quotient capacity 20))))))
          (cond ((and (<= capacity 15) (not (equal? status "Charging"))) "critical")
                ((<= capacity 30) "warning")
                (else "")))))

(if (null? batteries)
    (print-json '((shown . #f) (text . "") (class . "")))
    (let ((described (map describe batteries)))
      (print-json `((shown . #t)
                    (text . ,(string-join (map car described) " "))
                    (class . ,(or (find (lambda (c) (not (string-null? c))) (map cdr described)) ""))))))
