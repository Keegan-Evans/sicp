; SICP Functions
(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

; notes
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

; (define (pasc-triangle n)
;   (define (build-row prev row place)
;     (cond ((= place row)
;            (list 1))
;           ((= place 1)
;            (cons 1 (build-row prev row (+ 1 place))))
;           (else (cons (+ (car prev) (cadr prev)) 
;                       (build-row (cdr prev) row (+ 1 place))))))
;   (define (build-triangle prev row)
;     (let ((this-row (build-row prev row 1)))
;       (cond ((= row n)
;              (list this-row))
;             (else (append (list this-row)
;                         (build-triangle this-row (+ row 1)))))))
; 
;   ; Calculate the triangle                  
;   (define this-triangle (build-triangle (list) 1))
;   
;   ; print the triangle
;   (define (print-triangle triangle)
;     (define (triangle-print-helper triangle row)
;       (if (null? triangle)
;           "done"
;           (begin
;             ;(display (line-pad (get-pad-width row width)))
;             (display (car triangle))
;             ;(display (line-pad (get-pad-width row width)))
;             (newline)
;             (triangle-print-helper (cdr triangle) (+ 1 row)))))
;     (triangle-print-helper  triangle 1))
;   (print-triangle this-triangle))

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
;(gcd 206 40)

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
  (if (null? lst)
      '()
      (cons (car lst) (view-each (cdr lst)))))

;(view-each remain-lst)

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

(define (test-prime) (prime? 6001))

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

(define random
  (let ((*seed* 1) (quotient quotient) (modulo modulo) (+ +) (- -) (* *) (> >))
    (lambda (x)
      (let* ((hi (quotient *seed* 127773))
	     (low (modulo *seed* 127773))
	     (test (- (* 16807 low) (* 2836 hi))))
	(if (> test 0)
	    (set! *seed* test)
	    (set! *seed* (+ test 2147483647))))
      (modulo *seed* x))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a ))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; Exercise 1.21
; (smallest-divisor 199) => 199
; (smallest-divisor 1999) => 1999
; (smallest-divisor 19999) => 7

; Exercise 1.22

; ; only run the following when running the functions, otherwise it changes how
; ; the interpreter processes mutable pairs.
; ; Make mutable lists print neater
; ; see https://stackoverflow.com/questions/9347294/mcons-in-dr-racket
; ;(require sicp)
; ;(print-as-expression #f)
; ;(#%require r5rs/init)
; 
; (define (timed-prime-test n)
;   (newline) 
;   (display n)
;   (start-prime-test n (runtime)))
; 
; (define (start-prime-test n start-time)
;   (if (prime? n)
;       (report-prime (- (runtime) start-time))
;       #f))
; 
; (define (report-prime elapsed-time)
;   (display " *** ")
;   (display elapsed-time ))
; 
; (define (search-for-primes low high)
;   (define (helper x)
;     (cond ((<= x high)
;            (timed-prime-test x)
;            (helper (+ x 2)))))
;   (helper (if (odd? low) low (+ low 1))))
; 
; (search-for-primes 1000 1019)

; (define (find-primes start upper)
;   (find-primes-helper start upper (list)))
; (define (find-primes-helper x upper primes)
;   (cond ((= (length primes) 3) primes)
;         ((>= x upper) primes)
;         ((even? x) (find-primes-helper (+ x 1) upper primes))
;         ((prime? x) (find-primes-helper (+ x 2) upper (cons x primes)))
;         (else (find-primes-helper (+ x 2) upper primes))))
; (time (prime? 982451653))
; 
; (define (gen-list n x)
;   (if (= n 0)
;       (list x)
;       (cons x (gen-list (- n 1) x))))
; 
; (define instances-list (gen-list 1000 982451653))
; 
; (time (fast-prime? 982451653 10000))
; (time (map prime? instances-list))
; 
; (define (fast-prime?-timed-reps x)
;   (time (lambda (n) (fast-prime? x n))))

; ((fast-prime?-timed-reps 982451653) 1000)

; (map (fast-prime?-timed-reps 982451653) (list 1 10 100 1000 10000 100000))

; 1.23
(define (smallest-divisor-fast n)
  (find-divisor-fast n 2))

(define (find-divisor-fast n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-fast n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (next x)
  (if (= x 2)
      3
      (+ x 2)))
; 1.26
; Because it evaluates each argument to the multiplication seperately.

; 1.27
(define (car-test n)
  (define (ct a)
    (cond ((and (< a n)
               (= (remainder a n) (expt a n)))
           (cons a 
                 (ct (+ 1 a))))
          ((< a n)
           (cons 0 (ct (+ a 1))))
          (else (list))))
  (ct 1))

(define (only-greater lst)
  (cond ((null? lst)
         lst)
        ((> (car lst) 0)
         (cons (car lst)
               (only-greater (cdr lst))))
        (else (only-greater (cdr lst)))))

;; Section 1.3 

(define (sum term a next b)
  (if (> a b) 
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (sum-integers a b)
  (sum identity a inc b))

; 1.29

(define (simpsons f a b n)
  (define a1 (exact->inexact a))
  (define b1 (exact->inexact b))
  (define h (/ (- b1 a1)
               n))

  (define (y k)
    (f (+ a1
          (* k h))))

  (define (sim-fun k)
    (cond ((or (= k 0)
               (= k n))
           (y k))
          ((odd? k)
           (* 4 
              (y k)))
          (else (* 2 (y k)))))
  (* (/ h 3)
     (sum sim-fun 0 inc n)))

; 1.30

(define (sum2 term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ (term a)
                          result))))
  (iter a 0))

; 1.31
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (identity x) x)

(define (factorial x)
  (product identity 1 inc x))

; part b
(define (prod2 term a next b)
  (define (iter-prod a result)
    (if (> a b)
        result
        (iter-prod (next a) (* (term a) result))))
  (iter-prod a 1))

(define (inc2 x)
  (+ x 2))

; approximate pi
(define approximate-pi
  (exact->inexact
  (* 4
     (/ (* 2 (square (product identity 4 inc2 10002)))
        (square (product identity 3 inc2 10000))))))
; 1.32 
(define (recursive-accumulate combiner null-value term a next b)
   (if (> a b)
       null-value
       (combiner (term a)
                 (recursive-accumulate combiner
                             null-value
                             term
                             (next a)
                             next
                             b))))

(define (recursive-accumulate-sum term a next b)
  (recursive-accumulate + 0 term a next b))

(define (recursive-accumulate-product term a next b)
  (recursive-accumulate * 1 term a next b))

; Iteratively
(define (iterative-accumulate combiner null-value term a next b)
  (define (iter-fun a result)
    (if (> a b)
        result
        (iter-fun (next a) (combiner (term a) result))))
  (iter-fun a null-value))

(define (iterative-accumulate-sum term a next b)
  (iterative-accumulate + 0 term a next b))

(define (iterative-accumulate-product term a next b)
  (iterative-accumulate * 1 term a next b))

; Exercise 1.33

(define (filtered-accumulate pred combiner null-value term a next b)
 (define (filter-acc-iter a result)
   (cond ((> a b) 
          result)
         ((pred a)
          (filter-acc-iter (next a) (combiner (term a) result)))
         (else
           (filter-acc-iter (next a) result))))
 (filter-acc-iter a null-value))

;; 1.3.2
; Constructing Procedures using lambda
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

; Using let to create local variables
; This is especially useful as an alternative to auxiliary procedure

; with auxiliary procedures:
(define (f1 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

; with anonymous lambda procedure
(define (f2 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

; using let to make using anonymous lambda more convinent
(define (f3 x y)
  (let ((a (+ 1 ( * x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))
; The general form:
; (let ((<var1><expression1>)
;       (<var2><expression2>)
;       ...
;       (<varn><expressionn>))
;   <body>)
;
; Which really is the same as:
; ((lambda (<var1> ... <varn>)
;    <body>)
;  <exp1>
;  ...
;  <expn>)

; Exercise 1.34
; evaluated as: (f f) -> (f 2) -> (2 2)
; Because 2 is not a procedure, scheme will return an error stating thus, it
; makes no sense to pass a number an argument of any kin; evaluated as: (f f)
; -> (f 2) -> (2 2)
; Because 2 is not a procedure, scheme will return an error stating thus, it
; makes no sense to pass a number an argument of any kind

; 1.3.3 Procedures as General Methods

; the half interval method is used to find the roots of an equation f(x)=0,
; where f is a continuous function.

; given points a and b, if f(a) < 0 < f(b), f must have atleast one zero
; between a and b. To find zero, let x be avg(a, b), if f(x) > 0, the x must
; have a zezo between a and x, while if f(x) < 0, then there must be a root
; between b and x, this can be done repeatedly, until we arrive at a
; sufficently precise interval. Requires theta(log(L/T)), L is the length of
; the original interval and T is the size of the interval of the acceptable
; error.

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (close-enough? x y) (< (abs (- x y)) 0.001))

; a function to safely run search

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "values are not opposite sign" a b)))))

; Finding fixed point functions
; a fixed point of a function satisfies f(x) = x, which we can find by
; repeatedly applying the function to its own result, after starting with a
; guess

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (trace close-enough?)
  (trace try)
  (try first-guess))

; a square root function using fixed-point
(define (sqrt-2 x)
  (fixed-point (lambda (y) (average y (/ x y )))
               1.0))

; Exercise 1.35

(define (f135)
  (define (golden x)
    (+ 1 (/ 1 x)))
  (fixed-point golden 1.0))

; Exercise 1.36
(define (fixed-point-print f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess round-number)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (begin 
            (display (cons round-number '(tries to succed)))
            (newline)
            (display next)
            (newline))
          (begin
            (try next)))))
