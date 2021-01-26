#lang sicp
(#%require racket/trace)

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))


; EXERCISES

; 1.8
(define (cbrt x)
  (cb-iter 1.0 x))

(define (cb-iter guess x)
  (if (cb-good-enough? guess x)
      guess
      (cb-iter (cb-improve guess x) x)))

(define (cb-improve guess x)
  (/ (/ x (+ (square guess) (* 2 guess))) 3))

(define (cb-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cube x)
  (* x x x)


; Exercise 1.9

(define (+new1 a b)
  (if (= a 0) b (inc (+new1 (dec a) b))))

(trace +new1)

(define (+new2 a b)
  (if (= a 0) b (+new2 (dec a) (inc b))))

(trace +new2)


