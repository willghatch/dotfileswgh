#!/usr/bin/env racket
#lang rash
(require racket/cmdline racket/port racket/string)

(define orig-music-dir "/vsvr/vdata/nonprivate-medium/music")
(define output-dir "/vsvr/musc-compressed")
(define max-jobs 8)
(define verbose? #f)
(command-line
 #:once-each
 ["--orig-dir" orig-dir "directory of master music files"
               (set! orig-music-dir orig-dir)]
 ["--out-dir" out-dir "output directory"
              (set! output-dir out-dir)]
 ["--verbose" "be verbose?" (set! verbose? #t)]
 [("--jobs" "-j") number-of-jobs "number of jobs"
                  (set! max-jobs number-of-jobs)])

(set! orig-music-dir (path->complete-path orig-music-dir))
(set! output-dir (path->complete-path output-dir))
(when (not (directory-exists? orig-music-dir))
  (error "orig-music-dir path does not exist"))
(when (not (directory-exists? output-dir))
  {mkdir -p $output-dir})

(define (vprintf . args)
  (when verbose?
    (apply printf args)))

(define (path->directory p)
  (apply build-path (reverse (cdr (reverse (explode-path p))))))
(define (path->last p)
  (car (reverse (explode-path p))))

(define (find-with-ext extension)
  #{find $orig-music-dir -type f -name (string-append "*." extension) |> port->lines})

(define oggfiles (find-with-ext "ogg"))
(define opusfiles (find-with-ext "opus"))
(define mp3files (find-with-ext "mp3"))
(define flacfiles (find-with-ext "flac"))

(define (album+file-names f)
  (build-path (path->last (path->directory f))
              (path->last f)))

(define (up-to-date? orig copy)
  (and (file-exists? copy)
       (< (file-or-directory-modify-seconds orig)
          (file-or-directory-modify-seconds copy))))

(define (ogg/mp3->do-thunk f)
  (λ ()
    (define outfile (build-path output-dir (album+file-names f)))
    (if (up-to-date? f outfile)
        (vprintf "~a up to date, skipping\n" outfile)
        (begin
          {echo Copying $outfile ...}
          #{
            mkdir -p (path->directory outfile)
            chmod 755 (path->directory outfile)
            nice -n 15 cp $f $outfile
            chmod 644 $outfile
            }))))

(define (flac->do-thunk f)
  (λ ()
    (define outfile (build-path output-dir (path-replace-extension
                                            (album+file-names f)
                                            ".ogg")))
    (if (up-to-date? f outfile)
        (vprintf "~a up to date, skipping\n" outfile)
        (begin
          {echo Converting $outfile ...}
          #{
            mkdir -p (path->directory outfile)
            chmod 755 (path->directory outfile)
            nice -n 15 oggenc-from-flac-with-cover.sh $f $outfile
            chmod 644 $outfile
            }))))

(define job-thunks (append
                    (map ogg/mp3->do-thunk oggfiles)
                    (map ogg/mp3->do-thunk opusfiles)
                    (map ogg/mp3->do-thunk mp3files)
                    (map flac->do-thunk flacfiles)))

(define (wait/remove-done-job jobs)
  (define done (apply sync jobs))
  (remove done jobs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GO!
echo Starting music library conversion...

(define last-jobs
  (for/fold ([jobs '()])
            ([thunk job-thunks])
    (if (< (length jobs) max-jobs)
        (cons (thread thunk) jobs)
        (let ([done (apply sync jobs)])
          (cons (thread thunk)
                (wait/remove-done-job jobs))))))
(for ([job last-jobs])
  (sync job))

echo Done with music library conversion.
