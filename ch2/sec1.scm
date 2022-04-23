; reduces(end 2.1.1), handles positive and negative(ex2.1)
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
    (cond ((and (< n 0) (< d 0))
           (cons (/ (abs n) g) (/ (abs d) g)))
          ((or (< n 0) (< d 0))
           (cons (/ (- (abs n)) g) (/ (abs d) g)))
          (else
            (cons (/ n g) (/ d g))))))
 
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x ))
  (newline))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; ex2.2
(define (make-segment point-a point-b)
  (cons point-a point-b))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-midpoint segment)
  (make-point
    (/ (+
         (x-point (start-segment segment))
         (x-point (end-segment segment)))
       2)
    (/ (+
         (y-point (start-segment segment))
         (y-point (end-segment segment)))
       2)))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

; ex2.3

; complicated arbitrary rectangle
(define (segment-slope segment)
  (/ (-
       (y-point (end-segment segment))
       (y-point (start-segment segment)))
     (-
       (x-point (end-segment segment))
       (x-point (start-segment segment)))))

(define (reciprocal-slope slope-x)
  (if (integer? slope-x)
      (* -1
         (/ 1 slope-x))
      (* -1
         (/ (denominator slope-x)
            (numerator slope-x)))))

; simple, parallel to axises rectangle
(define (rect1 width height)
  ; corners - bottom-left, bottom-right, top-right, top-left
  (let ((bl (make-point 0 0))
        (br (make-point width 0))
        (tr (make-point width height))
        (tl (make-point 0 height)))
    (list (make-segment bl br)
          (make-segment br tr)
          (make-segment tr tl)
          (make-segment tl bl))))

; ex2.4
(define (lambda-cons x y)
  (lambda (m) (m x y)))

(define (lambda-car z)
  (z (lambda (p q) p)))

    ; solution:
(define (lambda-cdr z)
  (z (lambda (p q) q)))

; lambda-cons returns a procedure, with an

; ex 2.5
(define exp-prod-cons
  (lambda (a b) (* (expt 2 a)
                   (expt 3 b))))

(define log-base
  (lambda (x base)
    (/ (log x)
       (log base))))

(define exp-prod-car
  (lambda (base)
    (cond ((= (remainder in 3) 0)
           (exp-prod-cons (/ in 3)))
          (else (log-base z 2)))))

(define exp-prod-car
  (lambda (base)
    (cond ((= (remainder in 2) 0)
           (exp-prod-cons (/ in 2)))
          (else (log-base z 3)))))

(define inc-viewer
  (lambda (z inc-size)
    (define out (/ z inc-size))
    (display "new value: ")
    (display out)
    (newline)
    (display "remainder: ")
    (display (remainder z inc-size))
    (newline)
    (display "--------------------")
    (newline)
    out))

(define ex2.5
  (lambda ()
    (define z (exp-prod-cons 3 4))
    ;(inc-viewer z 3)
    (define y (inc-viewer z 3))
    (define x (inc-viewer y 3))
    (define w (inc-viewer x 3))
    (define v (inc-viewer w 3))
    (define u (inc-viewer v 3))
    (display "Ok, the remainder was not 0, so we will take the log base 2 of
             the last value for which the remainder was 0: ")
    (display (log-2 v))
    (newline)
    '()))

; ex 2.6
(define no-num-zero (lambda (f) (lambda (x) x)))

(define (no-num-add-1 n)
  (lambda (f) (lambda (x) ((n f) x))))

; ex 2.6 extended: basic language with lambda calculus/church numerals
; util to convert from church numerals to natural
(define (church->nat n)
  ((n add1) 0))

; numerals
(define church-zero
  (lambda (f) (lambda (x) x)))
(define church-one
  (lambda (f) (lambda (x) (f x))))
(define church-two
  (lambda (f) (lambda (x) (f (f x)))))
(define church-three
  (lambda (f) (lambda (x) (f (f (f x))))))
(define church-four
  (lambda (f) (lambda (x) (f (f (f (f x)))))))

; functions

;(letrec ((x (lambda (x ...) e))))

; multi-value lambda?

; add n & m like ((church-add n) m)
(define lc-add
  (lambda (n) (lambda (m) (lambda (f) (lambda (x) (f ((n (m f)) x)))))))

; multiply n * m

; subtract n - m

; divide n/m

; cons

; car

; cdr

; 2.1.4 extended

(define (make-interval a b)
  (if (<= a b)
    (cons a b)
    (cons b a)))

(define (lb i)
  (car i))

(define (ub i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lb x)
                    (lb y))
                 (+ (ub x)
                    (ub y))))

(define (mult-interval x y)
  (trace-let  combs
       ((p1 (* (lb x) (lb y)))
        (p2 (* (lb x) (ub y)))
        (p3 (* (ub x) (lb y)))
        (p4 (* (ub x) (ub y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (old-div-interval x y)
  (mult-interval x
                     (make-interval (/ 1.0 (lb y))
                                    (/ 1.0 (ub y)))))

; pre-define some intervals to use
(define i1 (make-interval 3 5))
(define i2 (make-interval 2 4))
(define i3 (make-interval 1 2))
(define i4 (make-interval 2 10))
(define i5 (make-interval 2 2))

; 2.8
(define (sub-interval x y)
  (make-interval (- (lb x)
                    (ub y))
                 (- (ub x)
                    (lb y))))

; 2.9
(define (width-interval i)
  (- (ub i)
     (lb i)))

(define (interval-width-demo)
  (display (cons "width i1: " (width-interval i1)))
  (newline)
  (display (cons "width i2: " (width-interval i2)))
  (newline)
  (display (cons "width i3: " (width-interval i3)))
  (newline)
  (display (cons "width i4: " (width-interval i4)))
  (newline)
  (newline)
  (display "For addition & subtraction the width of the new interval is the sum of the
           previous...")
  (newline)
  (display "width i1 (2) + i4 (8): ")
  (display (width-interval (add-interval i1 i4)))
  (newline)
  (display "width i2 (2) + i3 (1): ")
  (display (width-interval (add-interval i2 i3)))
  (newline)
  (display "width i3 (1) + i4 (8): ")
  (display (width-interval (add-interval i3 i4)))
  (newline)
  (display "For multiplication no relationship exists, if so two operations
           with identical intervals should produce the same width, but they
           don't:")
  (newline)
  (display "width i1 (2) * i4 (8): ")
  (display (width-interval (mult-interval i1 i4)))
  (newline)
  (display "width i2 (2) * i4 (8): ")
  (display (width-interval (mult-interval i2 i4)))
)
; 2.10
(define (div-interval x y)
  (if (= (width-interval y) 0)
    (display "The width of the interval you are trying to divide by is 0")
    (mult-interval x (make-interval (/ 1.0 (lb y))
                                        (/ 1.0 (ub y))))))

(define (demonstrate-div-width-0)
  (display "Normal: i4 (8) / i1 (2): ")
  (display (div-interval i4 i1))
  (newline)
  (display "Divide by zero width interval: i4 (8) / i5 (0): ")
  (display (div-interval i4 i5)))
; 2.11       even posible
; 0000 <<<<	y
; 0001 <<<>	y
; 0010 <<><	n
; 0011 <<>>	y
; 0100 <><<	y
; 0101 <><>	y
; 0110 <>><	n
; 0111 <>>>	y
; 1000 ><<<	n
; 1001 ><<>	n
; 1010 ><><	n
; 1011 ><>>	n
; 1100 >><<	y
; 1101 >><>	y
; 1110 >>><	n
; 1111 >>>>	y
;   xl  xu  yl  yu
;   <   <   <   <   x   c
;   <   <   <   >   x   c
;   <   <   >   >   x   c
;   <   >   <   <   x   c
;   <   >   <   >   x   c
;   <   >   >   >   x   c
;   >   >   <   <   x   c
;   >   >   <   >   x
;   >   >   >   >   x

(define (cond-mult-interval a b)
  (cond
    ; all negative
    ((and (negative? (lb a))
          (negative? (ub a))
          (negative? (lb b))
          (negative? (ub b)))
     (make-interval (* (ub a) (ub b))
                    (* (lb a) (lb b))))

    ; lower & upper a & lower b negative
    ((and (negative? (lb a))
          (negative? (ub a))
          (negative? (lb b))
          (not (negative? (ub b))))
     (make-interval (* (lb a) (ub b))
                    (* (lb a) (lb b))))

    ; lower & upper a negative
    ((and (negative? (lb a))
          (negative? (ub a))
          (not (negative? (lb b)))
          (not (negative? (ub b))))
     (make-interval (* (lb a) (ub b))
                    (* (ub a) (lb b))))

    ; lower a, lower & upper b negative
    ((and (negative? (lb a))
          (not (negative? (ub a)))
          (negative? (lb b))
          (negative? (ub b)))
     (make-interval (* (ub a) (lb b))
                    (* (lb a) (lb b))))

    ; lower a & lower b negative
    ((and (negative? (lb a))
          (not (negative? (ub a)))
          (negative? (lb b))
          (not (negative? (ub b))))
     (make-interval (min (* (lb a) (ub b))
                         (* (ub a) (lb b)))
                    (max (* (ub a) (ub b))
                         (* (lb a) (lb b)))))

    ; lower a negative
    ((and (negative? (lb a))
          (not (negative? (ub a)))
          (not (negative? (lb b)))
          (not (negative? (ub b))))
     (make-interval (* (lb a) (ub b))
                    (* (ub a) (ub b))))

    ; lower & upper b negative
    ((and (not (negative? (lb a)))
          (not (negative? (ub a)))
          (negative? (lb b))
          (negative? (ub b))
     (make-interval (* (lb a) (ub b))
                    (* (ub a) (lb b)))))

    ; lower b negative
    ((and (not (negative? (lb a)))
          (not (negative? (ub a)))
          (negative? (lb b))
          (not (negative? (ub b))))
     (make-interval (* (ub a) (lb b))
                    (* (ub a) (ub b))))

    ; all positive
    ((and (not (negative? (lb a)))
          (not (negative? (ub a)))
          (not (negative? (lb b)))
          (not (negative? (ub b))))
     (make-interval (* (lb a) (lb b))
                    (* (ub a) (ub b))))
    (else (display "invalid interval found")
          (newline)(display a)
          (newline)(display b))))


; INTERMEZZO
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lb i) (ub i)) 2))

(define (width i)
  (/ (- (ub i) (lb i)) 2))


; 2.12
(define (make-center-percent c p)
  (make-interval (- c (* c (* p 0.01)))
                 (+ c (* c (* p 0.01)))))
(define (interval-percent i)
  (let ((c (center i))
        (w (width i)))
    (/ (* 100 w) c)))

; 2.13
(define (demonstrate-2.13)
  (define a (make-center-percent 5 1))
  (define b (make-center-percent 18 0.75))
  (define c (make-center-percent 19873 0.02))
  (define d (make-center-percent 98731 0.012))
  (newline)(display "Interval a created with a center of 5 and percent 1")
  (newline)(display "Interval b created with a centr of 18 and percent 0.75")
  (newline)(display "Interval c created with a center of 19873 and percent
                    0.02")
  (newline)(display "Interval d created with a center of 98731 and percent
                    0.012")
  (newline)
  (newline)(display (format "~a: interval percent ~a" "(mult-interval a b)"
                            (interval-percent (mult-interval a b))))
  (newline)
  (newline)(display (format "~a: interval percent ~a" "(mult-interval c d)"
                            (interval-percent (mult-interval c d))))
  (newline)
  (newline)(display "For small percentages, the percent of the products is the
                    sum of the percentages"))

; INTERMEZZO
(define (par1 r1 r2)
  (div-interval (mult-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval
                    (div-interval one r1)
                    (div-interval one r2)))))


; 2.14 Tolerances accumulate, since there is no tolerance(well, rather there
; is 'zero' tolerance) on the ones used in the reciprocals there is only the
; tolerance generated by adding the reciprocals(which essentially have the
; same tolerances as the original intervals), while with par1 there are a lot
; more total intervals involved and so the tolerances build up, especiall with
; div-interval where the total tolerance is the sum of the tolerance of the
; numerator and the tolerance of the denominator.

; 2.15
; The number of occurences of each uncertain interval should occur the same
; number of times as the number of times that they physicall appear in the
; circuit.

; 2.16
; I do not think I have the math skills to do this, it seems like you would
; have to know how much each interval contributes in which situations. With
; add and divide this seems kind of straight forward(maybe, not so sure
; actually because I would think the proportion of on percent width of one
; interval compared to another would still make a difference) and immeadiately
; gets much harder for other operations, much less combinations. It seems
; like, differential equations might be needed to calculate and track the
; amount each difference made :s ;(
