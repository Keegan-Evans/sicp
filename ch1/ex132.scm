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
