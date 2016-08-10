(defvar journal-base-directory "/Users/adhearn/Dropbox/exobrain/journal"
  "The directory that holds journal files, without any date information.")

(defun journal-time->year
    (time)
  (nth 5 (decode-time time)))

(defun journal-time->month
    (time)
  (nth 4 (decode-time time)))

(defun journal-time->day
    (time)
  (nth 3 (decode-time time)))

(defun journal-time->dow
    (time)
  "Returns the day of the week from the given time"
  (nth 6 (decode-time time)))

(defun journal-dow-string
    (time)
  "Returns the day of the week as a three character string (e.g. 'Mon', 'Tue', etc.)"
  (let ((dow (journal-time->dow time)))
    (cond
     ((= dow 0) "Sun")
     ((= dow 1) "Mon")
     ((= dow 2) "Tue")
     ((= dow 3) "Wed")
     ((= dow 4) "Thu")
     ((= dow 5) "Fri")
     ((= dow 6) "Sat")
     (t (error "Invalid day of week: %d", dow)))))

(defun journal-month-string
    (time)
  "Returns the month of the given date as a string. The full name of the month is used, all in lowercase."
  (let ((month-num (journal-time->month time)))
    (cond
     ((= month-num 1) "january")
     ((= month-num 2) "februaury")
     ((= month-num 3) "march")
     ((= month-num 4) "april")
     ((= month-num 5) "may")
     ((= month-num 6) "june")
     ((= month-num 7) "july")
     ((= month-num 8) "august")
     ((= month-num 9) "september")
     ((= month-num 10) "october")
     ((= month-num 11) "november")
     ((= month-num 12) "december")
     (t (error "Invalid month: %d", month-num)))))

(defun journal-day-string
    (time)
  "Returns the day of the month of the given time as a string."
  (number-to-string (journal-time->day time)))

(defun journal-filename-for-date
    (date)
  "Returns the filename corresponding to today's date."
  (let ((year (journal-time->year date))
        (month (journal-month-string date))
        (day (journal-time->day date)))
    (convert-standard-filename
     (format "%s/%d/%s/%d.txt" journal-base-directory year month day))))

(defun journal-timestamp-str
    (time)
  (let ((year (journal-time->year time))
        (month (journal-time->month time))
        (day (journal-time->day time))
        (dow (journal-dow-string time)))
    (format "<%4d-%02d-%02d %s>" year month day dow)))

(defun journal-insert-timestamp
    (time)
  "Inserts a timestamp into the current buffer, followed by a newline."
  (insert (journal-timestamp-str time))
  (insert "\n"))

(defun journal-buffer-empty-p
    (buf)
  "Returns true if the given buffer is empty, false otherwise"
  (= (buffer-size buf) 0))

(defun journal-open-today
    ()
  "Opens the file for today into a new buffer and switches to that buffer."
  (let ((filename (journal-filename-for-date (current-time))))
    (let ((buffer (find-file filename)))
      (when (journal-buffer-empty-p buffer)
        (journal-insert-timestamp (current-time)))
      (visual-line-mode))))

(defun journal ()
  "An interactive functiont that wraps journal-open-today"
  (interactive)
  (journal-open-today))

(provide 'journal)


