require racket/trace)
(define (inc x)
  (+ x 1))
(define (dec x)
  (- x 1))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (square x)
  (* x x))
(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; Change Counting

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
	((or (< amount 0) (= kinds-of-coins 0)) 0)
	(else (+ (cc amount
		     (- kinds-of-coins 1))
		 (cc (- amount
			(first-denomination
			  kinds-of-coins))
		     kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
	((= kinds-of-coins 2) 5)
	((= kinds-of-coins 3) 10)
	((= kinds-of-coins 4) 25)
	((= kinds-of-coins 5) 50)))
(define (trace-count-change)
  (trace cc)
  (trace first-denomination))

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
  (* x x x))


; Exercise 1.9

(define (+new1 a b)
  (if (= a 0) b (inc (+new1 (dec a) b))))

(trace +new1)

(define (+new2 a b)
  (if (= a 0) b (+new2 (dec a) (inc b))))

(trace +new2)


; ex 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(trace A)


; 1.11

(define (f111 n)
  (define (f-iter a b c cnt)
    (cond ((< n 3) n)
          ((<= cnt 0) a)
          (else (f-iter (+ a 
                           (* 2 b)
                           (* 3 c))
                        a
                        b
                        (- cnt 1)))))
  (f-iter 2 1 0 (- n 2)))

; 1.12

;(define (f112 n)
;  (define (pasc-iter previous-row row column)
;    (cond ((and (= row column)
;                (= n row))
;           (display 1))
;          ())))
;

(define (pasc-triangle n)
  (define (build-row prev row place)
    (cond ((= place row)
           (list 1))
          ((= place 1)
           (cons 1 (build-row prev row (+ 1 place))))
          (else (cons (+ (car prev) (cadr prev)) 
                      (build-row (cdr prev) row (+ 1 place))))))
  (define (build-triangle prev row)
    (let ((this-row (build-row prev row 1)))
      (cond ((= row n)
             (list this-row))
            (else (append (list this-row)
                        (build-triangle this-row (+ row 1)))))))

  ; Calculate the triangle                  
  (define this-triangle (build-triangle (list) 1))
  
  ; print the triangle
  (define (print-triangle triangle)
    (define (triangle-print-helper triangle row)
      (if (null? triangle)
          "done"
          (begin
            ;(display (line-pad (get-pad-width row width)))
            (display (car triangle))
            ;(display (line-pad (get-pad-width row width)))
            (newline)
            (triangle-print-helper (cdr triangle) (+ 1 row)))))
    (triangle-print-helper  triangle 1))
  ; While I would like to be able to print it centered, I don't think
  ; tis the best use of my time right now, it may be something I come
  ; back to later, though below follows some of the preliminary work.
  ;(define (line-pad pad-width)
  ;  (if (= pad-width 0)
  ;      ""
  ;      (string-append " " (line-pad (- pad-width 1)))))

  ;(define (get-pad-width row width)
  ;  (- (- width 1)  row))

  ;(define (get-width triangle)
  ;  (if (null? triangle)
  ;      0
  ;      (+ 1 (get-width (cdr triangle)))))
  ;(display this-triangle)
  ;(display (get-pad-width 1 (get-width this-triangle)))
  ;(newline)
  ;(print-triangle this-triangle)
  (print-triangle this-triangle))

; Exercise 1.16
(define (fast-expt b n)
  (define (feh a b n)
    (cond ((= n 0) 
           a)
          ((even? n)
           (feh (* (square b) a) b (- n 2)))
          (else (feh (* b a)
                     b
                     (- n 1)))))
  (feh 1 b n))

; 1.17 

(define (fast-mult a b)
  (define (double x)
    (+ x x))
  (define (mult-it y a b)
    (cond ((= b 0)
           y)
          ((even? b)
           (mult-it (+ (double a) y) 
                    a
                    (- b 2)))
          (else (mult-it (+ y a)
                         a
                         (- b 1)))))
  (mult-it 0 a b)) 

; 1.18 Upon looking at this problem and looking at other people's answers, I
; did this exercise in the last one and should have created a much simpler
; recursive process in the last exercise...the wording is a bit ambiguous.jj

; 1.19 

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q cnt)
  (cond ((= cnt 0) b)
        ((even? cnt)
         (fib-iter a 
                   b
                   (+ (square p) 
                      (square q))
                   (+ (* 2 
                         p
                         q)
                      (square q))
                   (/ cnt 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- cnt 1)))))

; 1.2.5 Greatest Common Divisors if r is the remainder when a is divided by b,
; then the common divisors fo a and b are precisely the same as the common
; divisors of b and r.  
; GCD(a,b) = GCD(b,r)
; gcd(206,40) = gcd(40,6) = gcd(6,4) = gcd(4,2) = gcd(2,0) = 2
(define (gcd a b)
  (if (= b 0)
      a
      (begin
        (display (list a b))
        (newline)
        (gcd b (remainder a b)))))
(gcd 206 40)

; Lames Theorom: If Euclids Algorithm requires k steps to compute the gcd of
; some pairm then the smaller number in the pair must be greater than or equal
; to the kth fibonacci number.  
    
; 1.20
;
; Normal-order evaluation will return an error, as it will try to evaluate
; all of the arguements rather than only evaluating until a conditional
; returns #t, which is the case when using applicative order evaluation.
; with this being the case, normal-order evaluation would result to one
; more call to `remainder`, though this final call would return an error,
; as calling remainder with 0 in the denomenator place is undefined. In
; the example, there are 4 cals to `remainder` using applicative order
; evaluation and 5 calls to `remainder` by a process using normal-order
; evaluation. Additionally, the normal-order version will return an error,
; rather than finding the GCD.


(define remain-lst
  (list (remainder  206 40)
        (remainder 40 6)
        (remainder 6 4)
        (remainder 4 2)
        ;(remainder 2 0)
        ))
(define (view-each lst)
  (if (empty? lst)
      '()
      (cons (car lst) (view-each (cdr lst)))))

(view-each remain-lst)

; 1.2.6 Testing for Primality
; Two methods presented here:
;   - a O(sqrt(n)) one 
;   - a probablistic one with a O(log n) order of growth

; Determine if a number is prime by looking for its smallest divisor

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

; Using these procedures, we can test primality by determining if the smallest
; divisor of a number is itself.

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 6001)

; The Fermat Test
;
; Fermat's Little Theorom:
;
;     If n is a prime number and a is a any positive integr less than n, then a
;     raised to the nth power is congruent to a modulo n.
;
; congruent modulo n = True for two numbers if they have the same remainder
; when divided by n.
;
; modulo n: Short for remainder modulo n, that is the remainder of a number a
; when divided by n.
;
; When n is not prime, most numbers a<n with not satisfy the above relation. By
; trying many values of a, we can get a good idea of whether n is prime or not.

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

