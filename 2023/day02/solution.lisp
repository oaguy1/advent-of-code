(defpackage #:aoc-2023-day-2
  (:use :common-lisp)
  (:export :solution-day-1-part-1 :solution-day-1-part-2))

(in-package #:aoc-2023-day-2)

;; load shared utilities
(load "../utilities.lisp")

(defvar *max-blue* 14)
(defvar *max-green* 13)
(defvar *max-red* 12)

(defun solution-day-1-part-1 (file-path)
  (reduce #'+ (mapcar #'get-id (remove-if-not #'valid-game-p (aoc-utilities::get-input file-path)))))

(defun get-id (str)
  (parse-integer (cadr (split-by-space (car (split-by-colon str))))))

(defun valid-game-p (str)
  (every #'within-max-dice (map 'list #'split-by-space (car (map 'list #'split-by-punctuation (cdr (split-by-colon str)))))))

(defun within-max-dice (entry)
  (cond ((string= "green" (cadr entry)) (<= (parse-integer (car entry)) *max-green*))
	((string= "red" (cadr entry)) (<= (parse-integer (car entry)) *max-red*))
	((string= "blue" (cadr entry)) (<= (parse-integer (car entry)) *max-blue*))
	(t (error "unsupported color"))))

(defun split-by-punctuation (str)
  (split-string (lambda (x)
		  (or (eql x #\:)
		      (eql x #\;)
		      (eql x #\,))) str))

(defun split-by-space (str)
  (split-string (lambda (x) (eql x #\Space)) str))

(defun split-by-colon (str)
  (split-string (lambda (x) (eql x #\:)) str))

(defun split-by-semicolon (str)
  (split-string (lambda (x) (eql x #\;)) str))

(defun split-by-comma (str)
  (split-string (lambda (x) (eql x #\,)) str))

(defun split-string (delimiter-p str)
  (split-string-helper delimiter-p str ""))

(defun split-string-helper (delimiter-p str next-token)
  (cond ((string= str "")
	 (list (string-trim '(#\Space #\Tab #\Newline) next-token)))
	((funcall delimiter-p (char str 0))
	 (concatenate 'list (list (string-trim '(#\Space #\Tab #\Newline) next-token)) (split-string-helper delimiter-p (subseq str 1) "")))
	(t
	 (split-string-helper delimiter-p (subseq str 1) (concatenate 'string next-token (subseq str 0 1)))))) 

