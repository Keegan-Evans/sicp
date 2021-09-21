; 2.1
(define (ex2.1)
  (define (make-rat n d)
      (cond ((and (< n 0) (< d 0))
             (cons (abs n) (abs d)))
            ((or (< n 0)(< d 0))
             (cons (- (abs n)) (abs d)))
            (else (cons n d))))
  (lambda (x y) (make-rat x y)))
