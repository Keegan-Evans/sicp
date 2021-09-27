; Rational Number Arithmetic Implementation
(define (make-rat n d)
  (let ((g (GCD n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rt (- (* (numer x) (denom y))
              (* (numer y) (denom x)))
           (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Define the GCD function from 1.2.5 here
(define (GCD a b)
  (if (= b 0)
      a
      (GCD b (remainder a b))))

; access rather than creation time reduction
(define (acc-make-rat n d)
  (cons n d))

(define (acc-numer x)
  (let ((g (GCD (car x) (cdr x))))
    (/ (car x) g)))

(define (acc-denom x)
  (let ((g (GCD (car x) (cdr x))))
    (/ (cdr x) g)))
