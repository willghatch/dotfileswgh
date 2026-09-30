#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; network.scm -- the network status Waybar's network module showed, as JSON:
;;;   {"text", "tooltip", "class"}
;;; wifi "ESSID(SIGNAL%)", ethernet "ADDRESS/CIDR", "IFNAME (No IP)" when
;;; linked without an address, and "Disconnected ⚠".
(use-modules (ice-9 popen) (ice-9 rdelim) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define wifi-icon "")
(define wired-icon "")

(define (words line) (filter (negate string-null?) (string-split line #\space)))

(define (after key ws)
  (let ((tail (member key ws))) (and tail (pair? (cdr tail)) (cadr tail))))

;; "default via GATEWAY dev IFNAME ..."
(define route (let ((line (command-output "ip route show default"))) (and line (words line))))
(define ifname (and route (after "dev" route)))
(define gateway (and route (after "via" route)))

;; "N: IFNAME inet ADDRESS/CIDR ..."
(define address
  (and ifname
       (let ((line (command-output (string-append "ip -o -4 addr show dev " ifname))))
         (and line (after "inet" (words line))))))

(define (wifi-status)
  "(ESSID . SIGNAL-PERCENT) of IFNAME's wifi link, or #f."
  (or
   ;; nmcli: "yes:ESSID:SIGNAL" for the connected network.
   (let ((line (find (lambda (l) (string-prefix? "yes:" l))
                     (command-lines "nmcli -t -f active,ssid,signal dev wifi"))))
     (and line
          (let ((parts (string-split line #\:)))
            (cons (string-join (drop-right (cdr parts) 1) ":") (last parts)))))
   ;; iw: "SSID: NAME" and "signal: -52 dBm"
   (and ifname
        (let* ((lines (map string-trim (command-lines (string-append "iw dev " ifname " link"))))
               (ssid (find (lambda (l) (string-prefix? "SSID: " l)) lines))
               (signal (find (lambda (l) (string-prefix? "signal: " l)) lines)))
          (and ssid
               (cons (substring ssid 6)
                     (let ((dbm (and signal (string->number (car (words (substring signal 8)))))))
                       (if dbm (number->string (max 0 (min 100 (* 2 (+ dbm 100))))) "?"))))))))

(define wifi (and ifname (wifi-status)))

(print-json
 (cond ((not ifname) `((text . "Disconnected ⚠") (tooltip . "") (class . "disconnected")))
       ((not address) `((text . ,(string-append ifname " (No IP) " wired-icon)) (tooltip . ,ifname) (class . "linked")))
       (wifi `((text . ,(string-append (car wifi) "(" (cdr wifi) "%)" wifi-icon))
               (tooltip . ,(string-append ifname " via " (or gateway "?") " " wired-icon))
               (class . "wifi")))
       (else `((text . ,(string-append address wired-icon))
               (tooltip . ,(string-append ifname " via " (or gateway "?") " " wired-icon))
               (class . "ethernet")))))
