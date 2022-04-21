(load "ch2/sec1.scm")

(display "Complicated Version - Unfinished")
(define alpha
  (make-segment
    (make-point 0 0)
    (make-point 4 3)))

(newline)
(display alpha)

(define slope-alpha
  (segment-slope alpha))

(newline)
(display slope-alpha)

(define perpendicular-slope-alpha
  (reciprocal-slope slope-alpha))

(newline)
(display perpendicular-slope-alpha)
(display "\n")

; Simpler Version
(display "---Simpler Version---")

(define square_a (rect1 4 3))
(display square_a)

(exit)
