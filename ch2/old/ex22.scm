(define (ex2.2)
  (list "make-segment" "start-segment" "end-segment" "make-point" "point-x"
        "point-y"))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-point x y)
  (cons x y))

(define (make-segment start-point end-point)
  (list start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cadr segment))

(define (generate-segment x1 y1 x2 y2)
  (make-segment (make-point x1 y1)
                (make-point x2 y2)))

(define (print-segment segment)
  (display "start point: ")
  (print-point (start-segment segment))
  (display "end point: ")
  (print-point (end-segment segment)))

(define (midpoint-segment segment)
  (cons (/ (+ (x-point (end-segment segment))
              (x-point (start-segment segment)))
           2)
        (/ (+ (y-point (end-segment segment))
              (y-point (start-segment segment)))
           2)))
(define (print-midpoint segment)
  (print-point (midpoint-segment segment)))
