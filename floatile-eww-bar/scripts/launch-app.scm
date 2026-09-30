#!/usr/bin/env -S guile --no-auto-compile -s
!#
;;; launch-app.scm ID -- start the application with desktop-file ID.
;;;
;;; Uses gtk-launch or gio when installed, which handle field codes and
;;; startup notification; otherwise runs the entry's Exec without field codes.
(use-modules (ice-9 popen) (ice-9 rdelim) (srfi srfi-1))
(load (string-append (canonicalize-path (dirname (car (command-line)))) "/json.scm"))

(define id (cadr (command-line)))

(define (on-path? program)
  (any (lambda (dir) (access? (string-append dir "/" program) X_OK))
       (string-split (or (getenv "PATH") "/usr/bin:/bin") #\:)))

(define (entry-path)
  (let ((dirs (cons (or (getenv "XDG_DATA_HOME") (string-append (getenv "HOME") "/.local/share"))
                    (string-split (or (getenv "XDG_DATA_DIRS") "/usr/local/share:/usr/share") #\:))))
    (find file-exists?
          (append-map (lambda (d)
                        ;; An ID's dashes may stand for subdirectories.
                        (list (string-append d "/applications/" id)
                              (string-append d "/applications/"
                                             (string-map (lambda (c) (if (char=? c #\-) #\/ c)) id))))
                      dirs))))

(define (exec-line path)
  (call-with-input-file path
    (lambda (port)
      (let loop ((group #f))
        (let ((line (read-line port)))
          (cond ((eof-object? line) #f)
                ((string-prefix? "[" line) (loop (string-trim-both line)))
                ((and (equal? group "[Desktop Entry]") (string-prefix? "Exec=" line))
                 (substring line 5))
                (else (loop group))))))))

(define (without-field-codes exec)
  ;; %% is a literal %; every other %X is dropped.
  (let loop ((chars (string->list exec)) (out '()))
    (cond ((null? chars) (list->string (reverse out)))
          ((and (char=? (car chars) #\%) (pair? (cdr chars)))
           (loop (cddr chars) (if (char=? (cadr chars) #\%) (cons #\% out) out)))
          (else (loop (cdr chars) (cons (car chars) out))))))

(define (detach . command)
  (unless (zero? (primitive-fork))
    (primitive-exit 0))
  (setsid)
  (apply execlp (car command) command))

(cond ((on-path? "gtk-launch") (detach "gtk-launch" id))
      ((and (on-path? "gio") (entry-path)) => (lambda (path) (detach "gio" "launch" path)))
      ((and (entry-path) (exec-line (entry-path)))
       => (lambda (exec) (detach "/bin/sh" "-c" (string-append "exec " (without-field-codes exec)))))
      (else (format (current-error-port) "launch-app: no application ~a~%" id)
            (exit 1)))
