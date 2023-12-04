(defpackage :aoc-2023 
  (:use :common-lisp))

(defun solution-day-1 (file-path)
  (let ((sum 0))
  (dolist (line (get-input file-path))
    (setf sum (+ sum (get-num line)))) 
  sum))
  
(defun get-num (line)
  (get-num-helper line nil nil))

(defun get-num-helper (line first-digit last-digit)
  (cond ((equal line "") (+ last-digit (* first-digit 10)))
        ((and (is-digit (char line 0))
              (not first-digit))
         (get-num-helper (subseq line 1) (parse-integer (subseq line 0 1)) (parse-integer (subseq line 0 1))))
        ((is-digit (char line 0))
         (get-num-helper (subseq line 1) first-digit (parse-integer (subseq line 0 1))))
        (t
         (get-num-helper (subseq line 1) first-digit last-digit))))

(defun is-digit (ch)
  (and (> (char-code ch) 47)
       (< (char-code ch) 58)))

(defun get-input (file-path)
  "Read in input file"
  (with-open-file  (in file-path)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))
