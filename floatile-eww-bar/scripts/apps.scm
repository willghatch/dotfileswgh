#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; apps.scm -- the applications a desktop menu lists, by category, as JSON.
;;;
;;; Reads freedesktop desktop entries from $XDG_DATA_HOME/applications and
;;; each $XDG_DATA_DIRS/applications, earlier directories winning for each
;;; desktop-file ID, and leaves out NoDisplay, Hidden, OnlyShowIn/NotShowIn
;;; mismatches, and entries whose TryExec is not installed, as GNOME does.
;;; Prints [{"index", "category", "label",
;;;          "apps": [{"id", "name", "icon", "icon_path"}]}],
;;; where index is the category's position in the list, and icon_path the
;;; icon's image file, or "" when none is found.  The menu draws icons from
;;; their files, since eww cannot resize a theme icon drawn by name.
(use-modules (ice-9 popen) (ice-9 rdelim) (ice-9 ftw) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/desktop.scm"))

;; Freedesktop main categories, in menu order, with Audio and Video under
;; AudioVideo.
(define main-categories
  '(("AudioVideo" . "Sound & Video") ("Development" . "Programming") ("Education" . "Education")
    ("Game" . "Games") ("Graphics" . "Graphics") ("Network" . "Internet") ("Office" . "Office")
    ("Science" . "Science") ("Settings" . "Settings") ("System" . "System") ("Utility" . "Accessories")))

(define (category-of e)
  (let ((cats (map (lambda (c) (if (member c '("Audio" "Video")) "AudioVideo" c))
                   (list-value (entry "Categories" e)))))
    (or (find (lambda (c) (member c cats)) (map car main-categories)) "Other")))

(define entries
  (filter (lambda (pair) (shown? (cdr pair))) all-entries))

(define (numbered categories)
  (map (lambda (category index) (acons 'index index category))
       categories (iota (length categories))))

(print-json
 (numbered
  (filter-map
  (lambda (category)
    (let ((apps (sort (filter (lambda (pair) (equal? (category-of (cdr pair)) (car category))) entries)
                      (lambda (a b) (string-ci<? (entry "Name" (cdr a)) (entry "Name" (cdr b)))))))
      (and (pair? apps)
           `((category . ,(car category))
             (label . ,(cdr category))
             (apps . ,(map (lambda (pair)
                             (let ((icon (or (entry "Icon" (cdr pair)) "application-x-executable")))
                               `((id . ,(car pair))
                                 (name . ,(entry "Name" (cdr pair)))
                                 (icon . ,icon)
                                 (icon_path . ,(icon-path icon)))))
                           apps))))))
  (append main-categories '(("Other" . "Other"))))))
