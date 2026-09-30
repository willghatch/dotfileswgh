;;; json.scm -- write Scheme data as JSON, for the eww bar's scripts.
;;;
;;; An alist with symbol keys becomes an object, any other list an array,
;;; #t/#f true/false, symbols strings.

(use-modules (srfi srfi-1) (ice-9 popen) (ice-9 rdelim))

(define (json-object? value)
  (and (pair? value) (every (lambda (e) (and (pair? e) (symbol? (car e)))) value)))

(define (write-json-string s port)
  (write-char #\" port)
  (string-for-each
   (lambda (c)
     (case c
       ((#\") (display "\\\"" port))
       ((#\\) (display "\\\\" port))
       ((#\newline) (display "\\n" port))
       ((#\tab) (display "\\t" port))
       (else (if (< (char->integer c) 32)
                 (format port "\\u~4,'0x" (char->integer c))
                 (write-char c port)))))
   s)
  (write-char #\" port))

(define (write-json value port)
  (cond ((eq? value #t) (display "true" port))
        ((eq? value #f) (display "false" port))
        ((null? value) (display "[]" port))
        ((string? value) (write-json-string value port))
        ((symbol? value) (write-json-string (symbol->string value) port))
        ((and (number? value) (exact? value)) (display value port))
        ((number? value) (display (exact->inexact value) port))
        ((json-object? value)
         (display "{" port)
         (let loop ((entries value) (first #t))
           (unless (null? entries)
             (unless first (display "," port))
             (write-json-string (symbol->string (caar entries)) port)
             (display ":" port)
             (write-json (cdar entries) port)
             (loop (cdr entries) #f)))
         (display "}" port))
        ((list? value)
         (display "[" port)
         (let loop ((items value) (first #t))
           (unless (null? items)
             (unless first (display "," port))
             (write-json (car items) port)
             (loop (cdr items) #f)))
         (display "]" port))
        (else (write-json-string (object->string value) port))))

(define (print-json value)
  "Print VALUE as one line of JSON and flush, as eww's listeners expect."
  (write-json value (current-output-port))
  (newline)
  (force-output))

(define (command-output command)
  "The first line COMMAND, a shell command, prints, or #f."
  (let* ((port (open-input-pipe (string-append command " 2>/dev/null")))
         (line (read-line port)))
    (close-pipe port)
    (and (string? line) line)))

(define (command-lines command)
  "All lines COMMAND, a shell command, prints."
  (let ((port (open-input-pipe (string-append command " 2>/dev/null"))))
    (let loop ((lines '()))
      (let ((line (read-line port)))
        (if (eof-object? line)
            (begin (close-pipe port) (reverse lines))
            (loop (cons line lines)))))))

(define (truncate-text text limit)
  (if (> (string-length text) limit)
      (string-append (substring text 0 (- limit 1)) "…")
      text))

(define (markup-escape text)
  (fold (lambda (pair s) (string-join (string-split* s (car pair)) (cdr pair)))
        text '(("&" . "&amp;") ("<" . "&lt;") (">" . "&gt;"))))

(define (string-split* s sep)
  "S split at each occurrence of the string SEP."
  (let loop ((start 0) (parts '()))
    (let ((i (string-contains s sep start)))
      (if i
          (loop (+ i (string-length sep)) (cons (substring s start i) parts))
          (reverse (cons (substring s start) parts))))))
