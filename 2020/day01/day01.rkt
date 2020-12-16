#lang scheme

(define iter-each-item
  (λ (check? l)
    (cond
      ((<= (length l) 1) '())
      ((check? l) (cons (car l) (cons (car (cdr l)) '())))
      (#t (or (iter-each-item check? (cons (car l) (cdr (cdr l))))
              (iter-each-item check? (cdr l)))))))
         

(define mul-first-two
  (λ (l) (* (car l) (car (cdr l)))))

(define day01
  (λ (expenses)
    (mul-first-two (iter-each-item (λ (l) (eq? (+ (car l) (car (cdr l))) 2020)) expenses))))