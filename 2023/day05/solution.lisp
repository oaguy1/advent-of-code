(defpackage #:aoc-2023-day-5
  (:use :common-lisp :uiop)
  (:export :solution-day-5-part-1 :solution-day-4-part-2))

(in-package #:aoc-2023-day-5)


(defclass value-mapping ()
  ((destination-start
    :initarg :destination-start
    :accessor mapping-destination-start)
   (source-start
    :initarg :source-start
    :accessor mapping-source-start)
   (range
    :initarg :range
    :accessor mapping-range)))


(defmethod in-map-p ((value-map value-mapping) value)
  (and (>= value (mapping-source-start value-map))
      (<= value (+ (mapping-source-start value-map) (mapping-range value-map)))))

(defmethod map-value ((value-map value-mapping) value)
  (if (in-map-p value-map value)
      (+ (mapping-destination-start value-map) (- value (mapping-source-start value-map)))
      nil))

(defclass value-map-collection ()
  ((value-mappings
    :initarg :value-mappings
    :accessor collection-value-maps)))
	
(defmethod map-value ((collection value-map-collection) value)
  (let ((results (mapcar #'(lambda (value-map) (map-value value-map value)) (collection-value-maps collection))))
    (if (every #'not results)
	value
	(find-if #'(lambda (x) (not (not x))) results))))

(defclass mapping-chain ()
  ((mapping-collections
    :initarg :collections
    :accessor chain-mapping-collections)))

(defmethod map-value ((chain mapping-chain) value)
  (let ((input value))
    (dolist (map-collection (chain-mapping-collections chain))
      (setf input (map-value map-collection input)))
    input))

(defun solution-day-5-part-1 (file-path)
  (let ((file-input (uiop:read-file-lines file-path))
	(seeds '())
	(seed-to-soil-map nil)
	(soil-to-fertilizer-map nil)
	(fertilizer-to-water-map nil)
	(water-to-light-map nil)
	(light-to-temperature-map nil)
	(temerature-to-humidity-map nil)
	(humidity-to-location-map nil)
	(map-chain nil))

    ;; read in seeds and advance three lines 
    (setf seeds (parse-seeds (car file-input)))
    (setf file-input (cdddr file-input))

    ;; read in seed-to-soil map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf seed-to-soil-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))
 
    ;; read in soil-to-fertilizer map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf soil-to-fertilizer-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    ;; read in fertilizer-to-water map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf fertilizer-to-water-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    ;; read in water-to-light map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf water-to-light-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    ;; read in light-to-temperature map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf light-to-temperature-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    ;; read in temerature-to-humidity map and advance two lines
    (let ((mappings '()))
      (loop while (not (string= "" (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf temerature-to-humidity-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    ;; read in humidity-to-location map and advance two lines
    (let ((mappings '()))
      (loop while (not (not (car file-input)))
	    do
	       (setf mappings (append mappings (list (parse-mapping (car file-input)))))
	       (setf file-input (cdr file-input)))
      (setf humidity-to-location-map (make-instance 'value-map-collection :value-mappings mappings)))
    (setf file-input (cddr file-input))

    (setf map-chain (make-instance 'mapping-chain
				   :collections (list
						 seed-to-soil-map
						 soil-to-fertilizer-map
						 fertilizer-to-water-map
						 water-to-light-map
						 light-to-temperature-map
						 temerature-to-humidity-map
						 humidity-to-location-map)))

    (apply #'min (map 'list #'(lambda (x) (map-value map-chain x)) seeds))))

(defun parse-seeds (str)
  (mapcar #'parse-integer (split-by-space (cadr (split-by-colon str)))))

(defun parse-mapping (str)
  (let ((mapping (mapcar #'parse-integer (split-by-space str))))
    (make-instance 'value-mapping :destination-start (car mapping) :source-start (cadr mapping) :range (caddr mapping))))

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
