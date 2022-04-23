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
