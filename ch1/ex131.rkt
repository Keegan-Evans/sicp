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
