;;; Commentary:
;;;
;;; A utility for concatenating a directory of files numbered "1.txt", "2.txt",
;;; "3.txt", etc. to a file. (We can't use cat because the lexicographic
;;; ordering of numbers doesn't match their intuitiveordering.)
;;;
;;; Code:

(require 'journal)

(defvar journal-entry-regex "[1-3]?[0-9]\.txt" "Regex matching a journal entry file")

(defun journal-month-filenames
    (dir)
  (directory-files dir 't journal-entry-regex))

(defun filename<?
    (f1 f2)
  (let ((f1 (string-to-number (file-name-sans-extension (file-name-nondirectory f1))))
        (f2 (string-to-number (file-name-sans-extension (file-name-nondirectory f2)))))
    (< f1 f2)))

(defun journal-cat
    (year month)
  "Concatenates one month of journal entries into a single buffer, deleting the existing content (if any)."
  (let* ((path (concat journal-base-directory "/" year "/" month))
         (filenames (sort (journal-month-filenames path) #'filename<?))
         (digest-buf (find-file (concat path "/" month ".txt"))))
    (unless (= (buffer-size digest-buf) 0)
      (erase-buffer))
    (dolist (filename filenames nil)
      (insert-file-contents filename)
      (goto-char (point-max))
      (insert "\n\n"))))

(provide 'journal-cat)

