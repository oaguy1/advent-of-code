(defpackage #:aoc-2023-day-7
  (:use :common-lisp :uiop)
  (:export :solution-day-7-part-1 :solution-day-7-part-2))

(in-package #:aoc-2023-day-7)


(defvar *card-chars* (list #\A #\K #\Q #\J #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2))
(defvar *hand-types* (list :five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind :two-pairs :pair :high-card))

(defun solution-day-7-part-1 (file-path)
  (let* ((hands (aoc-2023-day-7::parse-input file-path))
	 (sorted-hands (sort hands #'aoc-2023-day-7::hand-comparator :key #'car)))
    (reduce #'+ (mapcar #'(lambda (x) (* (- (length sorted-hands) (position x sorted-hands)) (cadr x))) sorted-hands))))

(defun card-comparator (x y)
  (< (position x *card-chars*) (position y *card-chars*)))

(defun hand-type-comparator (x y)
  (< (position x *hand-types*) (position y *hand-types*)))

(defun count-comparator (x y)
  (if (eql (cdr x) (cdr y))
      (card-comparator (car x) (car y))
      (< (cdr x) (cdr y))))
 
(defclass hand ()
  ((cards
    :initarg :cards
    :accessor hand-cards)))

(defmethod hand-type ((h hand))
  (let* ((card-counts (value-counts (hand-cards h)))
	 (counts (sort (map 'list #'cdr card-counts) #'>)))
    (cond ((member 5 counts) (list :five-of-a-kind (significant-cards :five-of-a-kind card-counts)))
	  ((member 4 counts) (list :four-of-a-kind (significant-cards :four-of-a-kind card-counts)))
	  ((and (member 3 counts) (member 2 counts)) (list :full-house (significant-cards :full-house card-counts)))
	  ((member 3 counts) (list :three-of-a-kind (significant-cards :three-of-a-kind card-counts)))
	  ((and (= 2 (car counts)) (= 2 (cadr counts))) (list :two-pairs (significant-cards :two-pairs card-counts)))
	  ((member 2 counts) (list :pair (significant-cards :pair card-counts)))
	  (t (list :high-card (sort (hand-cards h) #'card-comparator))))))

(defun significant-cards (type-of-hand card-counts)
  (case type-of-hand
    (:five-of-a-kind (list (caar (value-assoc 5 card-counts))))
    (:four-of-a-kind (list (caar (value-assoc 4 card-counts))))
    (:full-house (list (caar (value-assoc 3 card-counts)) (caar (value-assoc 2 card-counts))))
    (:three-of-a-kind (list (caar (value-assoc 3 card-counts))))
    (:two-pairs (sort (list (caar (value-assoc 2 card-counts)) (caadr (value-assoc 2 card-counts))) #'card-comparator))
    (:pair (list (caar (value-assoc 2 card-counts))))))
    

(defun value-assoc (item assoc-list)
  (remove-if-not #'(lambda (x) (eql (cdr x) item)) assoc-list))

(defun hand-comparator (hand1 hand2)
  (let ((type-of-hand1 (hand-type hand1))
	(type-of-hand2 (hand-type hand2)))
    (if (eql (car type-of-hand1) (car type-of-hand2))
	(if (< 1 (length (cdr type-of-hand1)))
	    (let ((lst1 (cadr type-of-hand1))
		  (lst2 (cadr type-of-hand2)))
	      (loop while (eql (car lst1) (car lst2))
		    do
		       (setf lst1 (cdr lst1))
		       (setf lst2 (cdr lst2)))
	      (card-comparator (car lst1) (car lst2)))
	    (card-comparator (caadr type-of-hand1) (caadr type-of-hand2)))
	(hand-type-comparator (car type-of-hand1) (car type-of-hand2)))))
    

(defun value-counts (lst &optional (counts '()))
  (if (not lst)
      counts
      (progn
	(let ((curr-count (assoc (car lst) counts)))
	  (if (not curr-count)
	      (value-counts (cdr lst) (acons (car lst) 1 counts))
	      (progn
		(incf (cdr (assoc (car lst) counts)))
		(value-counts (cdr lst) counts)))))))

(defun parse-input (file-path)
  (let ((lines (uiop:read-file-lines file-path)))
    (loop for line in lines
	  collect (list
		   (make-instance 'hand :cards (map 'list #'identity (car (split-by-space line))))
		   (parse-integer (cadr (split-by-space line)))))))


(defun split-by-space (str)
  (split-string-by (lambda (x) (eql x #\Space)) str))

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
