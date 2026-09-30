;;; desktop.scm -- freedesktop desktop entries and icon theme lookup, shared
;;; by the application menu (apps.scm) and the window list (floatile-bars.scm).
;;; Loaded by those scripts; not run on its own.
(use-modules (ice-9 rdelim) (ice-9 ftw) (srfi srfi-1))

(define (env name default)
  (let ((v (getenv name))) (if (and v (not (string-null? v))) v default)))

(define application-dirs
  (map (lambda (d) (string-append d "/applications"))
       (cons (env "XDG_DATA_HOME" (string-append (getenv "HOME") "/.local/share"))
             (string-split (env "XDG_DATA_DIRS" "/usr/local/share:/usr/share") #\:))))

(define current-desktops
  (string-split (env "XDG_CURRENT_DESKTOP" "Floatile") #\:))

(define (desktop-files dir)
  "Pairs of desktop-file ID and path under DIR, IDs made from the relative
path with / as -."
  (if (not (file-exists? dir))
      '()
      (let ((prefix (+ 1 (string-length dir))))
        (file-system-fold
         (const #t)
         (lambda (path stat result)
           (if (string-suffix? ".desktop" path)
               (cons (cons (string-map (lambda (c) (if (char=? c #\/) #\- c))
                                       (substring path prefix))
                           path)
                     result)
               result))
         (lambda (path stat result) result)
         (lambda (path stat result) result)
         (lambda (path stat result) result)
         (lambda (path stat errno result) result)
         '() dir))))

(define (read-entry path)
  "The keys of the [Desktop Entry] group of PATH, as an alist of strings."
  (call-with-input-file path
    (lambda (port)
      (let loop ((group #f) (keys '()))
        (let ((line (read-line port)))
          (cond ((eof-object? line) keys)
                ((string-prefix? "[" line) (loop (string-trim-both line) keys))
                ((and (equal? group "[Desktop Entry]") (string-index line #\=))
                 => (lambda (i)
                      (let ((key (string-trim-both (substring line 0 i))))
                        (loop group (if (assoc key keys) keys
                                        (acons key (string-trim-both (substring line (+ i 1))) keys))))))
                (else (loop group keys))))))))

(define (entry key entry) (assoc-ref entry key))
(define (list-value s) (if s (filter (negate string-null?) (string-split s #\;)) '()))
(define (true? s) (equal? s "true"))

(define (on-path? program)
  (or (and (string-prefix? "/" program) (access? program X_OK))
      (any (lambda (dir) (access? (string-append dir "/" program) X_OK))
           (string-split (env "PATH" "/usr/bin:/bin") #\:))))

(define (shown? e)
  (and (equal? (entry "Type" e) "Application")
       (entry "Name" e)
       (not (true? (entry "NoDisplay" e)))
       (not (true? (entry "Hidden" e)))
       (let ((only (list-value (entry "OnlyShowIn" e))))
         (or (null? only) (any (lambda (d) (member d current-desktops)) only)))
       (not (any (lambda (d) (member d current-desktops)) (list-value (entry "NotShowIn" e))))
       (let ((try (entry "TryExec" e))) (or (not try) (on-path? try)))))

;; ---------------------------------------------------------------------------
;; Icons: a simplified freedesktop icon theme lookup
;; ---------------------------------------------------------------------------

(define (directory? path)
  (false-if-exception (file-is-directory? path)))

(define data-dirs
  (filter (negate string-null?)
          (cons (env "XDG_DATA_HOME" (string-append (getenv "HOME") "/.local/share"))
                (string-split (env "XDG_DATA_DIRS" "/usr/local/share:/usr/share") #\:))))

(define icon-base-dirs
  (cons (string-append (getenv "HOME") "/.icons")
        (map (lambda (d) (string-append d "/icons")) data-dirs)))

(define (ini-value path key)
  "The value of the first KEY= line of the file at PATH, or #f."
  (and (file-exists? path)
       (false-if-exception
        (call-with-input-file path
          (lambda (port)
            (let loop ()
              (let ((line (read-line port)))
                (cond ((eof-object? line) #f)
                      ((string-prefix? (string-append key "=") line)
                       (string-trim-both (substring line (+ 1 (string-length key)))))
                      (else (loop))))))))))

(define (theme-dirs theme)
  (filter directory? (map (lambda (base) (string-append base "/" theme)) icon-base-dirs)))

(define (theme-inherits theme)
  (let ((value (any (lambda (dir) (ini-value (string-append dir "/index.theme") "Inherits"))
                    (theme-dirs theme))))
    (if value (filter (negate string-null?) (string-split value #\,)) '())))

(define icon-themes
  ;; The configured theme, what it inherits, and hicolor last, as the
  ;; specification asks.
  (let* ((configured (or (ini-value (string-append (env "XDG_CONFIG_HOME" (string-append (getenv "HOME") "/.config"))
                                                   "/gtk-3.0/settings.ini")
                                    "gtk-icon-theme-name")
                         "Adwaita"))
         (themes (let loop ((pending (list configured)) (seen '()))
                   (cond ((null? pending) (reverse seen))
                         ((member (car pending) seen) (loop (cdr pending) seen))
                         (else (loop (append (cdr pending) (theme-inherits (car pending)))
                                     (cons (car pending) seen)))))))
    (append (delete "hicolor" themes) '("hicolor"))))

;; The size menu icons are drawn at, which bitmaps are chosen to be nearest.
(define wanted-icon-size 48)

(define (size-rank dir-name)
  "How well icons under DIR-NAME suit, lower being better: scalable first,
then bitmap sizes by distance from the wanted size, larger ones before
smaller."
  (cond ((string=? dir-name "scalable") 0)
        ((string-index dir-name #\@) #f)
        ((string->number (car (string-split dir-name #\x)))
         => (lambda (size)
              (+ 1 (* 2 (abs (- size wanted-icon-size))) (if (< size wanted-icon-size) 1 0))))
        (else #f)))

(define (subdirs dir)
  (or (false-if-exception
       (filter (lambda (name) (directory? (string-append dir "/" name)))
               (scandir dir (lambda (name) (not (member name '("." "..")))))))
      '()))

(define (image-name file)
  "FILE without its extension, if it is an image an icon may be, or #f."
  (any (lambda (ext) (and (string-suffix? ext file) (string-drop-right file (string-length ext))))
       '(".svg" ".png" ".xpm")))

(define (index-theme theme table)
  "Record in TABLE, for each icon name THEME has and TABLE lacks, its best file."
  (let ((found (make-hash-table)))
    (for-each
     (lambda (dir)
       (for-each
        (lambda (size-dir)
          (let ((rank (size-rank size-dir)))
            (when rank
              (for-each
               (lambda (context)
                 (let ((path (string-append dir "/" size-dir "/" context)))
                   (for-each
                    (lambda (file)
                      (let ((name (image-name file)))
                        (when name
                          (let ((best (hash-ref found name)))
                            (when (or (not best) (< rank (car best)))
                              (hash-set! found name (cons rank (string-append path "/" file))))))))
                    (or (scandir path) '()))))
               (subdirs (string-append dir "/" size-dir))))))
        (subdirs dir)))
     (theme-dirs theme))
    (hash-for-each (lambda (name best)
                     (unless (hash-ref table name) (hash-set! table name (cdr best))))
                   found)))

(define icon-files
  (let ((table (make-hash-table)))
    (for-each (lambda (theme) (index-theme theme table)) icon-themes)
    ;; Unthemed icons, in the pixmaps directories.
    (for-each (lambda (base)
                (let ((dir (string-append base "/pixmaps")))
                  (for-each (lambda (file)
                              (let ((name (image-name file)))
                                (when (and name (not (hash-ref table name)))
                                  (hash-set! table name (string-append dir "/" file)))))
                            (or (scandir dir) '()))))
              data-dirs)
    table))

(define (icon-path icon)
  "The image file for the desktop entry Icon value ICON, or \"\"."
  (cond ((and (string-prefix? "/" icon) (file-exists? icon)) icon)
        ((hash-ref icon-files icon))
        ((hash-ref icon-files "application-x-executable"))
        (else "")))


;; ---------------------------------------------------------------------------
;; Desktop entries by ID
;; ---------------------------------------------------------------------------

(define all-entries
  ;; Every application's entry, as (ID . ENTRY), earlier directories winning
  ;; for each ID, including those a menu does not show.
  (let loop ((dirs application-dirs) (seen '()) (result '()))
    (if (null? dirs)
        (reverse result)
        (let* ((files (desktop-files (car dirs)))
               (new (filter (lambda (f) (not (member (car f) seen))) files)))
          (loop (cdr dirs)
                (append (map car new) seen)
                (append (reverse
                         (filter-map (lambda (f)
                                       (let ((e (false-if-exception (read-entry (cdr f)))))
                                         (and e (equal? (entry "Type" e) "Application") (cons (car f) e))))
                                     new))
                        result))))))
