(defpackage :aoc-utilities
  (:use :common-lisp))

(in-package :aoc-utilities)

(defun get-input (file-path)
  "Read in input file"
  (with-open-file  (in file-path)
    (loop for line = (read-line in nil nil)
          while line
          collect line)))
