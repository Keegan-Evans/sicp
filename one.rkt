#%require racket/trace
#lang sicp


(define (+new1 a b)
  (if (= a 0) b (inc (+new1 (dec a) b))))

(define (+new2 a b)
  (if (= a 0) b (+new2 (dec a) (inc b))))
