#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; floatile-bars.scm -- what each vmonitor's bar shows, as JSON lines.
;;;
;;; Follows `floatilectl subscribe' and prints, on every change,
;;;   {"vmonitors": {"VMONITOR": {"workspaces": [{"id", "name", "shown", "active", "urgent"}],
;;;                               "layout": "LAYOUT",
;;;                               "windows": [{"id", "label", "tooltip", "icon_path",
;;;                                            "class", "marks", "tags"}]}, ...},
;;;    "night_light": {"on", "temperature"}}
;;; where a vmonitor's layout and windows are those of the workspace it displays.
;;; A window's label is a short name: its application's name, else the last
;;; part of its app id, else its title, shortened.  Its class is its tags
;;; joined by spaces, for styling; the bar knows `focused', `minimized',
;;; `floating', `maximized', `fullscreen', and `urgent', and ignores others.
;;; Its marks are symbols for floating, maximized, and fullscreen.
;;; Its icon_path is an image file: the icon the window gave, else the icon
;;; of its application, else "".
(use-modules (ice-9 popen) (ice-9 rdelim) (srfi srfi-1))
(define here (canonicalize-path (dirname (car (command-line)))))
(load (string-append here "/json.scm"))
(load (string-append here "/desktop.scm"))

(define (field record key) (assq-ref record key))

;; The longest label shown before it is cut short.
(define label-length 18)

(define (shorten text)
  (if (> (string-length text) label-length)
      (string-append (string-take text (- label-length 1)) "…")
      text))

(define (last-part app-id)
  "The part of APP-ID after its last dot, as in org.gnome.Nautilus."
  (let ((dot (string-rindex app-id #\.)))
    (if dot (substring app-id (+ dot 1)) app-id)))

;; Desktop entries by lower-case ID without .desktop, then, where no ID
;; matches, by lower-case StartupWMClass, for finding a window's application
;; from its app id.
(define entries-by-key
  (let ((table (make-hash-table)))
    (for-each (lambda (pair)
                (let ((id (string-downcase (if (string-suffix? ".desktop" (car pair))
                                               (string-drop-right (car pair) 8)
                                               (car pair)))))
                  (unless (hash-ref table id) (hash-set! table id (cdr pair)))))
              all-entries)
    (for-each (lambda (pair)
                (let ((class (entry "StartupWMClass" (cdr pair))))
                  (when (and class (not (hash-ref table (string-downcase class))))
                    (hash-set! table (string-downcase class) (cdr pair)))))
              all-entries)
    table))

(define (application-of app-id)
  "The desktop entry of the application with APP-ID, or #f."
  (and app-id (not (string-null? app-id))
       (or (hash-ref entries-by-key (string-downcase app-id))
           (hash-ref entries-by-key (string-downcase (last-part app-id))))))

(define (capitalized text)
  (if (string-null? text)
      text
      (string-append (string (char-upcase (string-ref text 0))) (substring text 1))))

(define (window-label w)
  (let* ((app-id (or (field w 'app-id) ""))
         (title (or (field w 'title) ""))
         (application (application-of app-id)))
    (shorten (cond ((and application (entry "Name" application)))
                   ((not (string-null? app-id)) (capitalized (last-part app-id)))
                   (else title)))))

(define (window-tooltip w)
  (let ((app (or (field w 'app-id) "")) (title (or (field w 'title) "")))
    (cond ((string-null? app) title)
          ((string-null? title) app)
          (else (string-append app " • " title)))))

(define (named-icon name)
  "The image file of the icon theme's icon NAME, or #f."
  (and name (not (string-null? name))
       (or (and (string-prefix? "/" name) (file-exists? name) name)
           (hash-ref icon-files name)
           (hash-ref icon-files (string-downcase name)))))

(define (window-icon-path w)
  (let* ((icon (field w 'icon))
         (images (if icon (or (field icon 'images) '()) '()))
         ;; The largest image the window gave, since the bar scales it.
         (largest (fold (lambda (image best)
                          (if (or (not best) (> (field image 'size) (field best 'size))) image best))
                        #f images))
         (app-id (field w 'app-id))
         (application (application-of app-id)))
    (or (and largest (field largest 'path))
        (named-icon (and icon (field icon 'name)))
        (named-icon (and application (entry "Icon" application)))
        (named-icon app-id)
        (named-icon (and app-id (last-part app-id)))
        "")))

(define window-marks
  '((floating . "◇") (maximized . "□") (fullscreen . "■")))

(define (marks tags)
  (string-concatenate (filter-map (lambda (mark) (and (memq (car mark) tags) (cdr mark))) window-marks)))

(define (window-urgent? w) (memq 'urgent (or (field w 'tags) '())))

(define (bars state)
  (let ((workspaces (field state 'workspaces))
        (windows (field state 'windows)))
    (map (lambda (vm)
           (let* ((shown-id (field vm 'workspace-id))
                  (shown (find (lambda (ws) (eqv? (field ws 'id) shown-id)) workspaces))
                  (ids (if shown
                           (append (field shown 'window-ids) (field shown 'minimized-window-ids))
                           '())))
             (cons (string->symbol (field vm 'name))
                   `((workspaces
                      . ,(filter-map
                          (lambda (ws)
                            (and (eqv? (field ws 'vmonitor-id) (field vm 'id))
                                 `((id . ,(field ws 'id))
                                   (name . ,(field ws 'name))
                                   (shown . ,(field ws 'displayed))
                                   (active . ,(field ws 'active))
                                   (urgent . ,(and (any (lambda (w) (and (eqv? (field w 'workspace-id) (field ws 'id))
                                                                         (window-urgent? w)))
                                                        windows)
                                                   #t)))))
                          workspaces))
                     (layout . ,(if shown (symbol->string (field shown 'layout)) ""))
                     (windows
                      . ,(filter-map
                          (lambda (w)
                            (and (memv (field w 'id) ids)
                                 (let ((tags (map symbol->string (or (field w 'tags) '()))))
                                   `((id . ,(field w 'id))
                                     (label . ,(window-label w))
                                     (tooltip . ,(window-tooltip w))
                                     (icon_path . ,(window-icon-path w))
                                     (class . ,(string-join tags " "))
                                     (marks . ,(marks (or (field w 'tags) '())))
                                     (tags . ,tags)))))
                          windows))))))
         (field state 'vmonitors))))

(define (night-light state)
  (let ((status (or (field state 'night-light) '())))
    `((on . ,(and (field status 'on) #t))
      (temperature . ,(or (field status 'temperature) 0)))))

;; json.scm writes symbol-keyed alists as objects; an empty vmonitor list
;; would be an array, so a session with no vmonitors gets an empty object.
(define (print-state state)
  (let ((vmonitors (bars state)))
    (display "{\"vmonitors\":")
    (if (null? vmonitors) (display "{}") (write-json vmonitors (current-output-port)))
    (display ",\"night_light\":")
    (write-json (night-light state) (current-output-port))
    (display "}")
    (newline)
    (force-output)))

(let ((port (open-input-pipe "floatilectl subscribe")))
  (let loop ()
    (let ((line (read-line port)))
      (unless (eof-object? line)
        (let ((state (false-if-exception (call-with-input-string line read))))
          (when (pair? state)
            (print-state state)))
        (loop)))))
