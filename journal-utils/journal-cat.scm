#!/usr/bin/guile \
-e journal-cat-month -s
!#

;;; Commentary:
;;;
;;; A utility for concatenating a directory of files numbered "1.txt", "2.txt",
;;; "3.txt", etc. to a file. (We can't use cat because the lexicographic
;;; ordering of numbers doesn't match their intuitiveordering.)
;;;
;;; Intended to be used with my journaling system for emacs. As such, it might
;;; make sense to just rewrite it in emacs lisp.
;;;
;;; Code:
(use-modules (ice-9 ftw)
             (ice-9 match)
             (ice-9 regex))

(define (text-file? filename)
  (string-match "\\.txt$" filename))

(define (valid-entry? name)
  (and (not (string=? name "."))
       (not (string=? name ".."))
       (text-file? name)))

(define (strip-ext filename)
  (substring filename 0 (- (string-length filename) 4)))

(define (filename<? f1 f2)
  (let ((f1 (string->number (strip-ext f1)))
        (f2 (string->number (strip-ext f2))))
    (< f1 f2)))

(define (journal-filenames dir)
  (scandir dir valid-entry? filename<?))

(define (write-file in out)
  (do ((c (read-char in) (read-char in)))
      ((eof-object? c))
    (write-char c out)))

(define (concatenate-journal-files dir out-port)
  (let ((filenames (journal-filenames dir)))
    (let lp ((filenames filenames))
      (unless (null? filenames)
        (let* ((filename (string-join (list dir (car filenames)) "/"))
               (in-port (open-file filename "r")))
          (write-file in-port out-port)
          (newline out-port)
          (newline out-port))
        (lp (cdr filenames))))))

(define (journal-cat-month args)
  (match (cdr args)
    ((dir)
     (concatenate-journal-files dir (current-output-port)))))
