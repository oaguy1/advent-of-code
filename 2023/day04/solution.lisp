(defpackage #:aoc-2023-day-4
  (:use :common-lisp :uiop)
  (:export :solution-day-4-part-1 :solution-day-4-part-2))

(in-package #:aoc-2023-day-4)

(defvar *max-blue* 14)
(defvar *max-green* 13)
(defvar *max-red* 12)

(defun solution-day-4-part-1 (file-path)
  (reduce #'+ (map 'list #'get-score (uiop:read-file-lines file-path))))

(defun get-id (str)
  (parse-integer (cadr (split-by-space (car (split-by-colon str))))))

(defun valid-game-p (str)
  (every #'within-max-dice (map 'list #'split-by-space (car (map 'list #'split-by-punctuation (cdr (split-by-colon str)))))))

(defun get-power (str)
  (map 'list #'split-by-space (car (map 'list #'split-by-punctuation (cdr (split-by-colon str))))))


(defun get-score (str)

(defun split-by-space (str)
  (split-string-by (lambda (x) (eql x #\Space)) str))

(defun split-by-colon (str)
  (split-string-by (lambda (x) (eql x #\:)) str))

(defun split-by-bar (str)
  (split-string-by (lambda (x) (eql x #\|)) str))

(defun split-string-by (delimiter-p str)
  (split-string-by-helper delimiter-p str ""))

(defun split-string-by-helper (delimiter-p str next-token)
  (cond ((string= str "")
	 (list (string-trim '(#\Space #\Tab #\Newline) next-token)))
	((funcall delimiter-p (char str 0))
	 (concatenate 'list (list (string-trim '(#\Space #\Tab #\Newline) next-token)) (split-string-by-helper delimiter-p (subseq str 1) "")))
	(t
	 (split-string-by-helper delimiter-p (subseq str 1) (concatenate 'string next-token (subseq str 0 1))))))

