(defpackage #:aoc-2023-day-1
  (:use :common-lisp)
  (:export :solution-day-1-part-1 :solution-day-1-part-2))

(in-package #:aoc-2023-day-1)

;; load shared utilities
(load "../utilities.lisp")

(defvar *written-numbers* '("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
(defvar *digits* '(0 1 2 3 4 5 6 7 8 9))
(defvar *digit-assoc-list*
  (loop for written-num in *written-numbers*
	for digit in *digits*
	collect (cons written-num digit)))

(defun solution-day-1-part-1 (file-path)
  (reduce #'+ (mapcar #'get-num (aoc-utilities::get-input file-path))))

(defun solution-day-1-part-2 (file-path)
  (reduce #'+ (mapcar #'get-written-num (aoc-utilities::get-input file-path))))
  
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

(defun get-written-num (line)
  (get-written-num-helper line nil nil))

(defun get-written-num-helper (line first-digit last-digit)
  (cond ((equal line "") (+ last-digit (* first-digit 10)))
        ((and (starts-with-written-digit line)
              (not first-digit))
         (get-written-num-helper (subseq line 1) (get-digit-from-written-num line) (get-digit-from-written-num line)))
        ((and (is-digit (char line 0))
              (not first-digit))
         (get-written-num-helper (subseq line 1) (parse-integer (subseq line 0 1)) (parse-integer (subseq line 0 1))))
	((starts-with-written-digit line)
	 (get-written-num-helper (subseq line 1) first-digit (get-digit-from-written-num line)))
        ((is-digit (char line 0))
         (get-written-num-helper (subseq line 1) first-digit (parse-integer (subseq line 0 1))))
        (t
         (get-written-num-helper (subseq line 1) first-digit last-digit))))

(defun starts-with-written-digit (str)
  (reduce #'(lambda (x &optional y) (or x y)) (mapcar #'(lambda (x) (starts-with str x)) *written-numbers*)))

(defun get-digit-from-written-num (str)
  (cdr (assoc str *digit-assoc-list* :test #'starts-with)))

(defun starts-with (str key)
  (and (>= (length str) (length key))
       (equal key (subseq str 0 (length key)))))

(defun is-digit (ch)
  (and (> (char-code ch) 47)
       (< (char-code ch) 58)))

