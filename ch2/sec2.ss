; Chapter 2, Section 2
; 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
    (car lst)
    (last-pair (cdr lst))))

; 2.18
(define (rev lst)
  (define (reviter org reved)
    (if (null? org)
      reved
      (reviter (cdr org) (cons (car org) reved))))
  (reviter lst '()))

; 2.19
(define (count-change amount)
  (cc amount 5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount (first-denomination coin-values))
                 coin-values)))))

(define (no-more? coin-values)
  (if (null? coin-values)
    #t
    #f))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

; 2.20
(define (same-parity first . others)
  (define (gather-parity parity others)
    (cond ((null? others) (list))
          ((boolean=? parity (odd? (car others)))
           (cons (car others)
                 (gather-parity parity (cdr others))))
          (else (gather-parity parity (cdr others)))))
  (let ((parity (odd? first)))
    (cons first (gather-parity parity others))))

; 2.21
(define square (lambda (x) (* x x)))

(define (square-list-rec items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list-rec (cdr items)))))


(define (square-list-map items)
  (map square items))

; 2.22
(define (square-list-iter-1 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things))
                  answer))))
  (iter items '()))
; Here you are starting at the front of the list and putting it on the back of
; the output list, then adding each subsequent result to the front of the
; list.

(define (square-list-iter-2 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer
                  (square (car things))
                  ))))
  (iter items '()))
; in this case, you are forming pairs in the right order, except that the list
; structure is malformed, in that at the termination of the list(chain of
; pairs) there is a number rather than an empty list/nil.

; 2.23
(define (for-each-ex proc items)
  (cond ((null? items) (void))
        (else
          (proc (car items))
          (for-each-ex proc (cdr items)))))

; 2.24
; (1 (2 (3 4)))

; 2.25
(define (demo-2-25-a)
  (cadr (caddr (list 1 3 (list 5 7) 9))))

(define (demo-2-25-b)
  (caar (list (list 7))))

(define (demo-2-25-c)
  (cadadr
    (cadadr
      (cadadr (list 1 (list 2 (list 3 (list 4 ( list 5 (list 6 7))))))))))

; 2.26
; (append x y): (1 2 3 4 5 6)
; (cons x y): ((1 2 3) 4 5 6)
; (list x y): ((1 2 3) (4 5 6))

; 2.27
(define (deep-reverse thingy)
  (define (deep-reverse-iter base reversed-so-far)
    (cond ((null? base) reversed-so-far)
          ((pair? (car base)) (deep-reverse-iter
                          (cdr base)
                          (cons (deep-reverse-iter (car base) '())
                                 reversed-so-far)))
          (else (deep-reverse-iter (cdr base)
                                   (cons (car base)
                                         reversed-so-far)))))

  (deep-reverse-iter thingy '()))

(define (demo-2-26)
  (define x (list (list 1 2) (list 3 4)))
  (deep-reverse x))

; 2.28
(define (demo-2-28)
  (let ((tl (list (list 3 (list 4)) 5 (list 2 (list 6 7 (list 8 9) 1)))))
    (display "Starting tree: ")(display tl)(newline)
    (display "iterative: ")(display (fringe-iter tl))(newline)
    (display "recursive: ")(display (fringe-recurse tl))(newline)))


; iterative method --- overly complicated/does not work with list of identical
; lists...pay more attention to what is going on and get working.
(define (fringe-iter tree)
  (define (fringer tree result)
    (cond ((null? tree) result)
          ((pair? tree)
           (fringer (car tree) (fringer (cdr tree) result)))
          (else (cons tree result))))

  (fringer tree '()))


; recursive method
(define (fringe-recurse tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else
          (append (fringe-recurse (car tree))
                  (fringe-recurse (cdr tree))))))

; 2.29
(define (mobile-demo)
  (let ((l1 (make-branch 5 3))
        (r1 (make-branch 10 1)))
    (let ((m1 (make-mobile l1 r1)))
      (display (left-branch m1)))))

(define (make-mobile left right)
  (list left right))

(define (make-branch len struct)
  (list len struct))

; a
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

  ;(if (pair? (cdr branch))
  ;    (cdr branch)
  ;    (cadr branch)))

;(define (weight? branch)
;  (if (pairs? (branch-structure
;      (
(define (weight? struct)
  (if (number? struct)
      #t
      #f))

; a mobile will have a null cdr, a branch with a weight at the end a list with
; a number in it.
(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (weight? struct)
        struct
        (total-weight struct))))

; b total-weight

(define (total-weight mobile)
  (if (null? mobile)
      0
      (+ (branch-weight (left-branch mobile))
         (branch-weight (right-branch mobile)))))

; c balance considerations

(define (node-balanced? left-length left-weight right-length right-weight)
  (if (= (* left-length left-weight)
         (* right-length right-weight))
    #t
    #f))

(define (balanced? mobile)
  (let ((ll (branch-length (left-branch mobile)))
        (ls (branch-structure (left-branch mobile)))
        (rl (branch-length (right-branch mobile)))
        (rs (branch-structure (right-branch mobile))))
    (cond ((and (weight? ls) (weight? rs))
           (node-balanced? ll ls rl rs))
          ((and (weight? ls) (not (weight? rs)))
           (and
             (node-balanced?
               ll ls
               rl (total-weight rs))
             (balanced? rs)))
          ((and (not (weight? ls)) (weight? rs))
           (and
             (node-balanced?
               ll (total-weight ls)
               rl rs)
             (balanced? ls)))
          (else
            (and
              (node-balanced?
                ll (total-weight ls)
                rl (total-weight rs))
              (balanced? ls)
              (balanced? rs))))))

(define ba (make-branch 10 1))
(define bb (make-branch 5 5))
(define bc (make-branch 25 1))
(define bbc (make-branch (/ 10 6)
                         (make-mobile bb bc)))
(define m3 (make-mobile ba bbc))

(define aa (make-branch 10 1))
(define ab (make-branch 5 5))
(define ac (make-branch 25 1))
(define abc (make-branch (/ 10 6)
                         (make-mobile ab ac)))
(define m4 (make-mobile aa abc))
; d
; no need to take the car of the cdr of branches
(define (right-branch mobile)
  (cdr mobile))
(define (cons-branch-structure branch)
  (cdr branch))
(define (cons-make-mobile left right)
  (cons left right))
(define (cons-make-branch len struct)
  (cons len struct))

(define (cons-branch-weight branch)
  (let ((struct (cons-branch-structure branch)))
    (if (weight? struct)
        struct
        (total-weight struct))))

(define (cons-total-weight mobile)
  (if (null? mobile)
      0
      (+ (cons-branch-weight (left-branch mobile))
         (cons-branch-weight (right-branch mobile)))))

(define (balanced? mobile)
  (let ((ll (branch-length (left-branch mobile)))
        (ls (branch-structure (left-branch mobile)))
        (rl (branch-length (right-branch mobile)))
        (rs (branch-structure (right-branch mobile))))
    (cond ((and (weight? ls) (weight? rs))
           (node-balanced? ll ls rl rs))
          ((and (weight? ls) (not (weight? rs)))
           (and
             (node-balanced?
               ll ls
               rl (total-weight rs))
             (balanced? rs)))
          ((and (not (weight? ls)) (weight? rs))
           (and
             (node-balanced?
               ll (total-weight ls)
               rl rs)
             (balanced? ls)))
          (else
            (and
              (node-balanced?
                ll (total-weight ls)
                rl (total-weight rs))
              (balanced? ls)
              (balanced? rs))))))

; mapping over trees
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree))
         (* factor tree))
        (else
          (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))

(define (map-scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (map-scale-tree sub-tree factor)
             (* sub-tree factor)))
  tree))

; 2.30
(define test-tree (list 1 2 (list 3) 4 (list 5 (list 6 7 8 (list 9)) 10)))

; recursive
(define (square-tree tree)
  (cond ((null? tree)
         '())
        ((not (pair? tree))
         (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))
; iterative
(define (iter-square-tree tree)
  (define (ist-help tree results)
    (cond ((null? tree)
           '())
          ((not (pair? tree))
           (square tree))
          (else
            (cons (ist-help (car tree) results)
                  (ist-help (cdr tree) results)))))
  (ist-help tree '()))

; map
(define (map-square-tree tree)
  (map (lambda (sub-tree)
         (cond ((pair? sub-tree)
                (map-square-tree sub-tree))
               (else
                 (square sub-tree))))
       tree))

; 2.31
(define (tree-map func tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

; 2.32

(define (subsets s)
  (if (null? s)
    '()
    (let ((rest (subsets (cdr s))))
      (append rest (map (car s) rest)))))

; section 2.2.3
(define (accumulate op intitial sequence)
  (if (null? sequence)
    intitial
    (op (car sequence)
        (accumulate op intitial (cdr sequence)))))
; 2.33
; a)
(define (map33 p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

; b)
(define (append33 seq1 seq2)
  (accumulate cons seq2 seq1))

; c)
(define (len seq)
  (accumulate (lambda (x y) (if (not (pair? x))
                                (+ 1 y)
                                (+ (len x) y)))
              0
              seq))

; 2.34

(define (horner-eval x coeefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coeefficient-sequence))

; 2.35
(define (count-leaves-accumulation t)
  (accumulate (lambda (current-branch rest)
                (if (not (pair? current-branch))
                    (+ 1 rest)
                    (+ (count-leaves-accumulation current-branch)
                       rest)))
              0
              t))
; 2.36
(define (each-car seqs)
  (cond ((null? seqs)
         '())
        (else
          (cons (caar seqs)
                (each-car (cdr seqs))))))
(define (each-cdr seqs)
  (cond ((null? seqs)
         '())
        ;((null? (car seqs))
        ; '())
        (else (cons (cdar seqs)
                    (each-cdr (cdr seqs))))))
(define (position-groups seqs)
  (cond ((null? seqs)
         '())
        ((null? (car seqs))
         '())
        (else
          (cons (each-car seqs)
                (position-groups (each-cdr seqs))))))
(define (accumulate-n op initial seqs)
  (cond ((null? seqs)
         '())
        ((null? (car seqs))
         '())
        (else
          (cons (accumulate op initial (each-car seqs))
                (accumulate-n op initial (each-cdr seqs))))))
; 2.37
(define (dot-product v w)
  ;(cond ((null?
  (accumulate +
                0
                (accumulate-n * 1 (list v w))))

(define (dot-vec v) (lambda (x) (dot-product x v)))
(define (matrix-*-vector m v)
  ;(let ((dot-vec v) (lambda (x) (dot-product x v)))
  (accumulate-n dot-vec '() (position-groups m)))
;
(define (accumulate-dot op initial seqs)
  (cond ((null? seqs)
         0)
        ((null? (car seqs))
         initial)
        (else
          (+ (accumulate op initial (car seqs))
             (accumulate-dot op initial (cdr seqs))))))
;(define (map-components op mat1 mat2)
;  (cond ((not (= (length mat1)
;                 (length mat2)))
;         (display (list "error, un matched sequence lengths.\nFirst: " (length mat1)
;                  "\nSecond: " (length mat2))))
;        ((null? mat1)
;         0)
;        ((pair? mat1)
;         (accumulate op (op (car mat1) (car mat2))
;                        (map-components op (cdr mat1) (cdr mat2)))
;
;
(define (dot-product v w)
  (let ((comb (list v w)))
    (accumulate + 
                0
                (accumulate-n * 1 comb))))

; columns of m times elements of v
(define (matrix-*-vector m v)
  )
; columns of m1 dot rows m2
(define (matrix-*-matrix m1 m2)
  )

; rows of m_out are columns of m, maybe use each-car?
(define (transpose-matrix m)
  )
