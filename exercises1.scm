#| # Structure and Interpretation of Computer Programs - Exercises
## 1.1
10
*10*
(+ 5 3 4)
*12*
(- 9 1)
*8*
(/ 6 2)
*3*
(+  (* 2 4) (- 4 6))
*6*
(define a 3)
*a*
(define b (+ a 1))
*b*
(+ a b (* a b))
*19*
(= a b)
*FALSE*

(if (and (> b a) (< b (* a b)))
	b 
	a)
*4*

(cond  (( = a 4)  6)
	((= b 4)(+ 6 7 a))
	(else 25))
*16*

(+ 2 (if (> b a) b a))
*6*

(* 	(cond 	((> a b) a )
					((< a b) b )
					(else -1))
		(+ a 1 ))
*16*

1.2
(/(+ 4
     5
     (- 2
        (- 3
           (+ 6
              (/ 4 5)
           )
        )
     )
   )
  (* 3
     (- 6 2)
     (- 2 7)))
 |#
;;; 1.3
;;; define square function first
(define (square x) (* x x))

;;; define sum of squares 
(define (sum_of_squares x y)
   (+ (square x)
      (square y)))
   

(define (sum_biggest_squares a b c)
    (cond((OR(AND (> b a) (> c a)) (= a c)) (sum_of_squares b c))
         ((OR(AND (> a b) (> c b)) (= a b)) (sum_of_squares a c))
         ((OR(AND (> a c) (> b c)) (= b c)) (sum_of_squares a b))))

;;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#| It looks to the most interior function/set of parentheses, that is the expression (> b 0), that is, is "b" greater than zero? this is then fed to next function out, that is the conditional (if proposition return_if_true return_if_false) , which in this case is either the symol "+" or  the symbol "-", which is returned as the operator of the next function, defining it is as either a addition ofor subtraction function. 
|#

;;; 1.5
;;; If the function is evaluated using normal-order evaluation, then the function will return 0. However, if applicative-order evalution is used, then the program will try keep running (p) as it calls itself, therefore never reaching a primitive function to be evaluated and therefore never completing running.
