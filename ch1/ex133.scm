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
