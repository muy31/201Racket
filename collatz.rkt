#lang racket

(require racket/trace)

;; Pythagorean function
(define (bigbrainfunc a b)
  (sqrt (+ (expt a 2) (expt b 2))))

;; The Collatz conjecture
(define (collatz n)
  (if (= (modulo n 2) 0) (/ n 2)
      (+ 1 (* n 3))))


;; generate the series based on Collatz
(define (c-series n)
  (print n)
  (newline)
  (if (equal? n 1) 'done
      (let ((next (collatz n)))
	(c-series next))))

;; generate the series based on Collatz
(define (c-series2 n)
  (print n)
  (newline)
  (if (equal? n 1) 'done
      (c-series2 (collatz n))))
