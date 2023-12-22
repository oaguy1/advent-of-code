(defpackage #:aoc-2023-day-6
  (:use :common-lisp :uiop)
  (:export :solution-day-6-part-1 :solution-day-6-part-2))

(in-package #:aoc-2023-day-6)

(defun solution-day-6-part-1 (file-path)
  (let ((games (parse-input-part-1 file-path)))
    (reduce #'* (mapcar #'ways-to-win games))))

(defun solution-day-6-part-2 (file-path)
  (let ((game (parse-input-part-2 file-path)))
    (ways-to-win game)))

(defun ways-to-win (game)
  (let ((total-time (car game))
	(winning-dist (cadr game))
	(winning-games 0))
    (loop for holding-time from 1 to total-time
	  do
	     (let ((speed holding-time)
		   (remaining-time (- total-time holding-time)))
	       (if (> (* speed remaining-time) winning-dist)
		   (incf winning-games))))
    winning-games))
		   
(defun parse-input-part-1 (file-path)
  (let* ((lines (uiop:read-file-lines file-path))
	 (times (mapcar #'parse-integer (split-by-space (cadr (split-by-colon (car lines))))))
	 (dists (mapcar #'parse-integer (split-by-space (cadr (split-by-colon (cadr lines)))))))
    (loop for time in times
	  for dist in dists
	  collect (list time dist))))

(defun parse-input-part-2 (file-path)
  (let* ((lines (uiop:read-file-lines file-path))
	 (time (format nil "~{~A~}" (split-by-space (cadr (split-by-colon (car lines))))))
	 (dist (format nil "~{~A~}" (split-by-space (cadr (split-by-colon (cadr lines)))))))
    (list (parse-integer time) (parse-integer dist))))

(defun split-by-space (str)
  (split-string-by (lambda (x) (eql x #\Space)) str))

(defun split-by-colon (str)
  (split-string-by (lambda (x) (eql x #\:)) str))

(defun split-string-by (delimiter-p str)
  (split-string-by-helper delimiter-p str ""))

(defun split-string-by-helper (delimiter-p str next-token)
  (cond ((string= str "")
	 (list (string-trim '(#\Space #\Tab #\Newline) next-token)))
	((funcall delimiter-p (char str 0))
	 (if (string= next-token "")
	     (split-string-by-helper delimiter-p (subseq str 1) next-token)
	     (concatenate 'list (list (string-trim '(#\Space #\Tab #\Newline) next-token)) (split-string-by-helper delimiter-p (subseq str 1) ""))))
	(t
	 (split-string-by-helper delimiter-p (subseq str 1) (concatenate 'string next-token (subseq str 0 1))))))
