(defpackage #:aoc-2023-day-4
  (:use :common-lisp :uiop)
  (:export :solution-day-4-part-1 :solution-day-4-part-2))

(in-package #:aoc-2023-day-4)


(defun solution-day-4-part-1 (file-path)
  (reduce #'+ (map 'list #'get-score (uiop:read-file-lines file-path))))

(defun solution-day-4-part-2 (file-path)
  (let* ((scores (init-scores file-path))
	 (queue (uiop:read-file-lines file-path)))
    (how-many-cards queue scores)))

(defun init-scores (file-path)
  (let ((hash (make-hash-table)))
    (dolist (line (uiop:read-file-lines file-path))
      (setf (gethash (get-id line) hash) 1))
    hash))

(defun how-many-cards (queue scores)
  (let ((total 0))
    (dolist (card queue)
      (let ((id (get-id card))
	    (score (get-num-matches card)))
	(if (> score 0)
	    (loop for i from (+ id 1) to (+ id score)
		  do (setf (gethash i scores) (+ (gethash i scores) (gethash id scores)))))
	(setf total (+ total (gethash id scores)))))
    total))


(defun get-id (str)
  (parse-integer (cadr (split-by-space (car (split-by-colon str))))))


(defun get-score (str)
  (let* ((score 0)
	 (strs (split-by-bar (cadr (split-by-colon str))))
	 (winning-nums (map 'list #'parse-integer (split-by-space (car strs))))
	 (card-nums (map 'list #'parse-integer (split-by-space (cadr strs)))))
    (dolist (curr-num winning-nums)
      (if (member curr-num card-nums)
	  (incf score)))
    (if (> score 0)
	(expt 2 (- score 1))
	0)))

(defun get-num-matches (str)
  (let* ((score 0)
	 (strs (split-by-bar (cadr (split-by-colon str))))
	 (winning-nums (map 'list #'parse-integer (split-by-space (car strs))))
	 (card-nums (map 'list #'parse-integer (split-by-space (cadr strs)))))
    (dolist (curr-num winning-nums)
      (if (member curr-num card-nums)
	  (incf score)))
    score))

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
	 (if (string= next-token "")
	     (split-string-by-helper delimiter-p (subseq str 1) next-token)
	     (concatenate 'list (list (string-trim '(#\Space #\Tab #\Newline) next-token)) (split-string-by-helper delimiter-p (subseq str 1) ""))))
	(t
	 (split-string-by-helper delimiter-p (subseq str 1) (concatenate 'string next-token (subseq str 0 1))))))

