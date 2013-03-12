#lang racket

(local ((define (filter fun list)
          (cond 
            ((empty? list) empty)
            ((fun (car list)) (cons (car list) (filter fun (cdr list))))
            (else (filter fun (cdr list))))))
  (filter (lambda (x) (> x 4)) '(1 2 3 4 5 6 7 8)))