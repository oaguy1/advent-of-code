(defpackage #:aoc-2023-day-3
  (:use :common-lisp :uiop)
  (:export :solution-day-3-part-1 :solution-day-3-part-2))

(in-package #:aoc-2023-day-3)

(defun solution-day-3-part-1 (file-path)
  (let* ((arr (init-arr file-path))
	 (i-max (array-dimension arr 1))
	 (j-max (array-dimension arr 0))
	 (curr-num-buff "")
	 (valid-part-numbers '()))
    (do ((j 0 (+ 1 j)))
	((= j j-max) nil)
      (do ((i 0 (+ 1 i)))
	  ((= i i-max) nil)
	(if (is-connected-p arr i j)
	    (setf curr-num-buff (concatenate 'string curr-num-buff (coerce (list (aref arr j i)) 'string)))
	    (if (not (string= curr-num-buff ""))
		(progn
		  (setf valid-part-numbers (append valid-part-numbers (list (parse-integer curr-num-buff))))
		  (setf curr-num-buff "")))))
      (if (not (string= curr-num-buff ""))
	  (progn
	    (setf valid-part-numbers (append valid-part-numbers (list (parse-integer curr-num-buff))))
	    (setf curr-num-buff ""))))
    (reduce #'+ valid-part-numbers)))

(defun solution-day-3-part-2 (file-path)
  (let* ((arr (init-arr file-path))
	 (nums (init-nums arr))
	 (i-max (array-dimension arr 1))
	 (j-max (array-dimension arr 0))
	 (gear-ratios '()))
    (do ((j 0 (+ 1 j)))
	((= j j-max) nil)
      (do ((i 0 (+ 1 i)))
	  ((= i i-max) nil)
	(princ (aref arr j i))
	(if (eql #\* (aref arr j i))
	    (setf gear-ratios (append gear-ratios (list (gear-ratio nums i j)))))))
    (pprint gear-ratios)
    (reduce #'+ gear-ratios)))

(defun init-nums (arr)
  (let ((nums (make-array (list (array-dimension arr 0) (array-dimension arr 1)) :initial-element -1))
	(i-max (array-dimension arr 1))
	(j-max (array-dimension arr 0))
	(curr-num-buff "")
	(curr-coords '()))
    (do ((j 0 (+ 1 j)))
	((= j j-max) nil)
      (do ((i 0 (+ 1 i)))
	  ((= i i-max) nil)
	(if (digit-char-p (aref arr j i))
	    (progn
	      (setf curr-num-buff (concatenate 'string curr-num-buff (coerce (list (aref arr j i)) 'string)))
	      (setf curr-coords (append curr-coords (list (list j i)))))
	    (if (not (string= curr-num-buff ""))
		(progn
		  (mapcar #'(lambda (x) (setf (aref nums (car x) (cadr x)) (parse-integer curr-num-buff))) curr-coords)
		  (setf curr-num-buff "")
		  (setf curr-coords '())))))
      (if (not (string= curr-num-buff ""))
	  (progn
	    (mapcar #'(lambda (x) (setf (aref nums (car x) (cadr x)) (parse-integer curr-num-buff))) curr-coords)
	    (setf curr-num-buff "")
	    (setf curr-coords '()))))
    nums))
  
(defun init-arr (file-path)
  (let* ((lines (uiop:read-file-lines file-path))
	 (arr (make-array (list (length lines) (length (car lines))))))
    (do* ((temp lines (cdr temp))
	  (line (car lines) (car temp))
	  (i 0 (+ 1 i)))
	 ((null temp) arr)
      (do* ((curr-line line (subseq curr-line 1))
	    (j 0 (+ 1 j)))
	   ((string= "" curr-line) nil)
	(setf (aref arr i j) (char curr-line 0))))))

(defun is-connected-p (arr i j &key (next-digit nil) (prev-digit nil))
  ;; skip all this work if the character isn't a number
  (when (not (digit-char-p (aref arr j i)))
    (return-from is-connected-p nil))
  
  (let ((results '())
	(max-i (- (array-dimension arr 1) 1))
	(max-j (- (array-dimension arr 0) 1)))

    ;; diagonally up and to the left, don't search if we are the next digit
    (let ((x (- i 1))
	  (y (- j 1))) 
      (unless (or (or (= i 0) (= j 0)) next-digit)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :diagonal-up-left))))))

    ;; directly to the left, could be a prev digit
    (let ((x (- i 1))
	  (y j))
      (unless (or (= i 0) next-digit)
	(if (digit-char-p (aref arr y x))
	    (setf results (append results (list (list (is-connected-p arr x y :prev-digit t) :behind))))
	    (setf results (append results (list (list (special-char-p (aref arr y x)) :behind)))))))

    ;; diagonally down and to the left, don't search if we are the next digit
    (let ((x (- i 1))
	  (y (+ j 1)))
      (unless (or (or (= j max-j) (= i 0)) next-digit)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :diagonal-down-left))))))

    ;; directly above
    (let ((x i)
	  (y (- j 1)))
      (unless (= j 0)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :above))))))

    ;; directly below
    (let ((x i)
	  (y (+ j 1)))
      (unless (= j max-j)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :below))))))

    ;; diagonally up and to the right
    (let ((x (+ i 1))
	  (y (- j 1)))
      (unless (or (= i max-i) (= j 0) prev-digit)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :diagonal-up-right))))))

    ;; diagonally down and to the right
    (let ((x (+ i 1))
	  (y (+ j 1)))
      (unless (or (= j max-j) (= i max-i) prev-digit)
        (setf results (append results (list (list (special-char-p (aref arr y x)) :diagonal-down-right))))))
 
    ;; directly to the right, could be a next digit
    (let ((x (+ i 1))
	  (y j))
      (unless (or (= i max-i) prev-digit)
	(if (digit-char-p (aref arr y x))
	    (setf results (append results (list (list (is-connected-p arr x y :next-digit t) :right))))
	    (setf results (append results (list (list (special-char-p (aref arr y x)) :right)))))))

    (reduce #'(lambda (x y) (or x y)) (map 'list #'car results))))

(defun gear-ratio (nums i j)
  (let ((results '())
	(max-i (- (array-dimension nums 1) 1))
	(max-j (- (array-dimension nums 0) 1))
	(top-left -1)
	(top-center -1)
	(top-right -1)
	(left -1)
	(right -1)
	(bottom-left -1)
	(bottom-center -1)
	(bottom-right -1))

    ;; diagonally up and to the left
    (let ((x (- i 1))
	  (y (- j 1)))
      (unless (or (= i 0) (= j 0))
	(setf top-left (aref nums y x))
	(unless (< top-left 0)
	  (setf results (append (list top-left) results)))))

    ;; directly above
    (let ((x i)
	  (y (- j 1)))
      (unless (= j 0)
	(setf top-center (aref nums y x))
	(unless (or (< top-center 0) (= top-center top-left))
	  (setf results (append (list top-center) results)))))

    ;; diagonally up and to the right
    (let ((x (+ i 1))
	  (y (- j 1)))
      (unless (or (= i max-i) (= j 0))
	(setf top-right (aref nums y x))
	(unless (or (< top-right 0) (= top-right top-center))
	  (setf results (append (list top-right) results)))))

    ;; directly to the left, could be a prev digit
    (let ((x (- i 1))
	  (y j))
      (unless (or (= i 0))
	(setf left (aref nums y x))
	(unless (< left 0)
	  (setf results (append (list left) results)))))

    ;; directly to the right, could be a next digit
    (let ((x (+ i 1))
	  (y j))
      (unless (= i max-i)
	(setf right (aref nums y x))
	(unless (< right 0)
	  (setf results (append (list right) results)))))

    ;; diagonally down and to the left, don't search if we are the next digit
    (let ((x (- i 1))
	  (y (+ j 1)))
      (unless (or (= j max-j) (= i 0))
	(setf bottom-left (aref nums y x))
	(unless (< bottom-left 0)
	  (setf results (append (list bottom-left) results)))))

    ;; directly below
    (let ((x i)
	  (y (+ j 1)))
      (unless (= j max-j)
	(setf bottom-center (aref nums y x))
	(unless (or (< bottom-center 0) (= bottom-center bottom-left))
	  (setf results (append (list bottom-center) results)))))

    ;; diagonally down and to the right
    (let ((x (+ i 1))
	  (y (+ j 1)))
      (unless (or (= j max-j) (= i max-i))
	(setf bottom-right (aref nums y x))
	(unless (or (< bottom-right 0) (= bottom-right bottom-center))
	  (setf results (append (list bottom-right) results)))))
 

    (if (= 1 (length results))
	0
	(reduce #'* results))))

(defun special-char-p (ch)
  (cond ((eql ch #\.) nil)
	((digit-char-p ch) nil)
	(t t)))
