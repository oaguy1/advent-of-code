(defpackage #:aoc-2023-day-2
  (:use :common-lisp :uiop)
  (:export :solution-day-2-part-1 :solution-day-2-part-2))

(in-package #:aoc-2023-day-2)

(defvar *max-blue* 14)
(defvar *max-green* 13)
(defvar *max-red* 12)

(defun solution-day-2-part-1 (file-path)
  (reduce #'+ (mapcar #'get-id (remove-if-not #'valid-game-p (uiop:read-file-lines file-path)))))

(defun solution-day-2-part-2 (file-path)
  (reduce #'+ (map 'list #'get-power (uiop:read-file-lines file-path))))

(defun get-id (str)
  (parse-integer (cadr (split-by-space (car (split-by-colon str))))))

(defun valid-game-p (str)
  (every #'within-max-dice (map 'list #'split-by-space (car (map 'list #'split-by-punctuation (cdr (split-by-colon str)))))))

(defun get-power (str)
  (get-power-helper (map 'list #'split-by-space (car (map 'list #'split-by-punctuation (cdr (split-by-colon str))))) -1 -1 -1))

(defun get-power-helper (dice-list max-green max-red max-blue)
  (cond ((not dice-list)
	 (* max-green max-red max-blue))
	((string= "green" (cadar dice-list))
	 (get-power-helper (cdr dice-list) (max max-green (parse-integer (caar dice-list))) max-red max-blue))
	((string= "red" (cadar dice-list))
	 (get-power-helper (cdr dice-list) max-green (max max-red (parse-integer (caar dice-list))) max-blue))
	((string= "blue" (cadar dice-list))
	 (get-power-helper (cdr dice-list) max-green max-red (max max-blue (parse-integer (caar dice-list)))))
	(t (error "unsupported color"))))
		
(defun within-max-dice (entry)
  (cond ((string= "green" (cadr entry)) (<= (parse-integer (car entry)) *max-green*))
	((string= "red" (cadr entry)) (<= (parse-integer (car entry)) *max-red*))
	((string= "blue" (cadr entry)) (<= (parse-integer (car entry)) *max-blue*))
	(t (error "unsupported color"))))

(defun split-by-punctuation (str)
  (split-string-by (lambda (x)
		  (or (eql x #\:)
		      (eql x #\;)
		      (eql x #\,))) str))

(defun split-by-space (str)
  (split-string-by (lambda (x) (eql x #\Space)) str))

(defun split-by-colon (str)
  (split-string-by (lambda (x) (eql x #\:)) str))

(defun split-by-semicolon (str)
  (split-string-by (lambda (x) (eql x #\;)) str))

(defun split-by-comma (str)
  (split-string-by (lambda (x) (eql x #\,)) str))

(defun split-string-by (delimiter-p str)
  (split-string-by-helper delimiter-p str ""))

(defun split-string-by-helper (delimiter-p str next-token)
  (cond ((string= str "")
	 (list (string-trim '(#\Space #\Tab #\Newline) next-token)))
	((funcall delimiter-p (char str 0))
	 (concatenate 'list (list (string-trim '(#\Space #\Tab #\Newline) next-token)) (split-string-by-helper delimiter-p (subseq str 1) "")))
	(t
	 (split-string-by-helper delimiter-p (subseq str 1) (concatenate 'string next-token (subseq str 0 1))))))

