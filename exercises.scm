; Chapter 1 
; 1.1
;
; 10
; 12
; 8
; (/ 6 2) 3
; (define a 3) ***WARNING -- defining global variable: a
; (define b (+ 3 1))***WARNING -- defining global variable: b
; (+ a b (* a b)): 19
; (= a b): #f
; (if (and (> b a) (< b (* a b))) b a) : 4
; (cond ((= a 4) 6)((= b 4) (+ 6 7 a))(else 25)): 16
; (+ 2 (if (> b a) b a)): 6
; (* (cond ((> a b) a)((< a b) b)(else -1))(+ a 1)): 16

; 1.2

(/ (+ 5 4 (- 2 (+ 6 (/ 4 5)))) (* 3 (- 6 2) (- 2 7)))

; 1.3

(define (sum-of-squares-of-largest-two a b c)
  (cond ((and (>= a c) (>= b c))
         (sum-of-squares a b))
        ((and (>= b a) (>= c a))
         (sum-of-squares b c))
        (else (sum-of-squares a c))))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

; probably a better way to solve this is to create a sum of squares
; procedure and also a procedure that finds the two largest inputs (or
; even better, a procedure to sort a set of numbers that you give it and
; then just takes the first two for the purposes of this procedure)

; 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0)
       + 
       -)
       a b))

; In this problem, we calculate the sum of a and the absolute value of
; b. Rather than directly finding the absolute value of be, we change
; the function that we use to combine it with a, that is if the value of
; b is below 0, we can subtract it from a, this is the same as adding
; the absolute value, while if it is non-negative, we simply add it. In
; this case, the "compound operator" is either + or - depending on the
; value of the input b. This procedure uses the special for `if` to
; determine this.

; 1.5

; With an applicative order interpreter, this input result in the
; inpterpreter returning a value of 0, because it never looks at the
; value of (p). However with normal-order evaluation, the interpreter
; will attempt to evaluate the value of all of the arguments, because
; attempting to resolve the value of (p) will result in endless attempts
; to evaluate (p), eventually the memory will fill up and the
; interpreter will crash. 

; 1.6
; 
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))
; 
; (define (ni-sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (ni-sqrt-iter (improve guess x) x)))
; 
; The special form if only will evaluate one argument, the new procedure
; does not specify to do this and thus it will try to evaluate both,
; leading to repeated calls to itself.


; 1.7
; With large numbers, you might run out of precision for the storing of
; the numbers. In the case of small numbers, 0.0001 can proportionally
; be huge compared to the number. 
; The new strategy should work equally well with both large and small
; numbers, since it is based on proportional change. It might appear
; more accurate on small numbers.

(define (prop-chng-sqrt last-guess guess x)
  (if (proportional-good-enough? last-guess guess x)
      guess
      (prop-chng-sqrt guess (improve guess x) x)))

(define (proportional-good-enough? last-guess guess x)
  (if (<
        (abs (- 1
                (/ last-guess 
                   guess)))
        0.001)
      #t
      #f))

(define (prop-sqrt x)
  (prop-chng-sqrt (+ x 0.0) 1.0 x))

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
  (* x x x))