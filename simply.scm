;;; simply.scm version 3.13 (8/11/98)

;;; This file uses Scheme features we don't talk about in _Simply_Scheme_.
;;; Read at your own risk.

(if (equal? 'foo (symbol->string 'foo))
    (error "Simply.scm already loaded!!")
    #f)

;; Make number->string remove leading "+" if necessary

(if (char=? #\+ (string-ref (number->string 1.0) 0))
    (let ((old-ns number->string) (char=? char=?) (string-ref string-ref)
	  (substring substring) (string-length string-length))
      (set! number->string
	    (lambda args
	      (let ((result (apply old-ns args)))
		(if (char=? #\+ (string-ref result 0))
		    (substring result 1 (string-length result))
		    result)))))
    'no-problem)

(define number->string
  (let ((old-ns number->string) (string? string?))
    (lambda args
      (if (string? (car args))
	  (car args)
	  (apply old-ns args)))))

;; Get strings in error messages to print nicely (especially "")

(define whoops
  (let ((string? string?) (string-append string-append)	(error error)
	(cons cons) (map map) (apply apply))
    (define (error-printform x)
      (if (string? x)
	  (string-append "\"" x "\"")
	  x))
    (lambda (string . args)
      (apply error (cons string (map error-printform args))))))


;; ROUND returns an inexact integer if its argument is inexact,
;; but we think it should always return an exact integer.
;; (It matters because some Schemes print inexact integers as "+1.0".)
;; The (exact 1) test is for PC Scheme, in which nothing is exact.
(if (and (inexact? (round (sqrt 2))) (exact? 1))
    (let ((old-round round) (inexact->exact inexact->exact))
      (set! round
	    (lambda (number)
	      (inexact->exact (old-round number)))))
    'no-problem)

;; Remainder and quotient blow up if their argument isn't an integer.
;; Unfortunately, in SCM, (* 365.25 24 60 60) *isn't* an integer.

(if (inexact? (* .25 4))
    (let ((rem remainder) (quo quotient) (inexact->exact inexact->exact)
	  (integer? integer?))
      (set! remainder
	    (lambda (x y)
	      (rem (if (integer? x) (inexact->exact x) x)
		   (if (integer? y) (inexact->exact y) y))))
      (set! quotient
	    (lambda (x y)
	      (quo (if (integer? x) (inexact->exact x) x)
		   (if (integer? y) (inexact->exact y) y)))))
    'done)


;; Random
;; If your version of Scheme has RANDOM, you should take this out.
;; (It gives the same sequence of random numbers every time.) 

(define random
  (let ((*seed* 1) (quotient quotient) (modulo modulo) (+ +) (- -) (* *) (> >))
    (lambda (x)
      (let* ((hi (quotient *seed* 127773))
	     (low (modulo *seed* 127773))
	     (test (- (* 16807 low) (* 2836 hi))))
	(if (> test 0)
	    (set! *seed* test)
	    (set! *seed* (+ test 2147483647))))
      (modulo *seed* x))))


;;; Logo-style word/sentence implementation

(define word?
  (let ((number? number?) (symbol? symbol?) (string? string?))
    (lambda (x)
      (or (symbol? x) (number? x) (string? x)))))

(define sentence?
  (let ((null? null?) (pair? pair?) (word? word?) (car car) (cdr cdr))
    (define (list-of-words? l)
      (cond ((null? l) #t)
	    ((pair? l)
	     (and (word? (car l)) (list-of-words? (cdr l))))
	    (else #f)))
    list-of-words?))

(define empty?
  (let ((null? null?) (string? string?) (string=? string=?))
    (lambda (x)
      (or (null? x)
	  (and (string? x) (string=? x ""))))))


(define char-rank
  ;; 0 Letter in good case or special initial
  ;; 1 ., + or -
  ;; 2 Digit
  ;; 3 Letter in bad case or weird character
  (let ((*the-char-ranks* (make-vector 256 3))
	(= =) (+ +) (string-ref string-ref) (string-length string-length)
	(vector-set! vector-set!) (char->integer char->integer)
	(symbol->string symbol->string) (vector-ref vector-ref))
    (define (rank-string str rank)
      (define (helper i len)
	(if (= i len)
	    'done
	    (begin (vector-set! *the-char-ranks*
				(char->integer (string-ref str i))
				rank)
		   (helper (+ i 1) len))))
      (helper 0 (string-length str)))
    (rank-string (symbol->string 'abcdefghijklmnopqrstuvwxyz) 0)
    (rank-string "!$%&*/:<=>?~_^" 0)
    (rank-string "+-." 1)
    (rank-string "0123456789" 2)
    (lambda (char)		    ;; value of char-rank
      (vector-ref *the-char-ranks* (char->integer char)))))

(define string->word
  (let ((= =) (<= <=) (+ +) (- -) (char-rank char-rank) (string-ref string-ref)
	(string-length string-length) (string=? string=?) (not not)
	(char=? char=?) (string->number string->number)
	(string->symbol string->symbol))
    (lambda (string)
      (define (subsequents? string i length)
	(cond ((= i length) #t)
	      ((<= (char-rank (string-ref string i)) 2)
	       (subsequents? string (+ i 1) length))
	      (else #f)))
      (define (special-id? string)
	(or (string=? string "+")
	    (string=? string "-")
	    (string=? string "...")))
      (define (ok-symbol? string)
	(if (string=? string "")
	    #f
	    (let ((rank1 (char-rank (string-ref string 0))))
	      (cond ((= rank1 0) (subsequents? string 1 (string-length string)))
		    ((= rank1 1) (special-id? string))
		    (else #f)))))

      (define (nn-helper string i len seen-point?)
	(cond ((= i len)
	       (if seen-point?
		   (not (char=? (string-ref string (- len 1)) #\0))
		   #t))
	      ((char=? #\. (string-ref string i))
	       (cond (seen-point? #f)
		     ((= (+ i 2) len) #t)  ; Accepts "23.0"
		     (else (nn-helper string (+ i 1) len #t))))
	      ((= 2 (char-rank (string-ref string i)))
	       (nn-helper string (+ i 1) len seen-point?))
	      (else #f)))
      (define (narrow-number? string)
	(if (string=? string "")
	    #f
	    (let* ((c0 (string-ref string 0))
		   (start 0)
		   (len (string-length string))
		   (cn (string-ref string (- len 1))))
	      (if (and (char=? c0 #\-) (not (= len 1)))
		  (begin
		   (set! start 1)
		   (set! c0 (string-ref string 1)))
		  #f)
	      (cond ((not (= (char-rank cn) 2)) #f)  ; Rejects "-" among others
		    ((char=? c0 #\.) #f)
		    ((char=? c0 #\0)
		     (cond ((= len 1) #t)  ; Accepts "0" but not "-0"
			   ((= len 2) #f)  ; Rejects "-0" and "03"
			   ((char=? (string-ref string (+ start 1)) #\.)
			    (nn-helper string (+ start 2) len #t))
			   (else #f)))
		    (else (nn-helper string start len #f)))))) 

      ;; The body of string->word:
      (cond ((narrow-number? string) (string->number string))
	    ((ok-symbol? string) (string->symbol string))
	    (else string)))))

(define char->word
  (let ((= =) (char-rank char-rank) (make-string make-string) (char=? char=?)
        (string->symbol string->symbol) (string->number string->number))
    (lambda (char)
      (let ((rank (char-rank char))
	    (string (make-string 1 char)))
	(cond ((= rank 0) (string->symbol string))
	      ((= rank 2) (string->number string))
	      ((char=? char #\+) '+)
	      ((char=? char #\-) '-)
	      (else string))))))

(define word->string
  (let ((number? number?) (string? string?) (number->string number->string)
	(symbol->string symbol->string))
    (lambda (wd)
      (cond ((string? wd) wd)
	    ((number? wd) (number->string wd))
	    (else (symbol->string wd))))))

(define count
  (let ((word? word?) (string-length string-length)
	(word->string word->string) (length length))
    (lambda (stuff)
      (if (word? stuff)
	  (string-length (word->string stuff))
	  (length stuff)))))

(define word
  (let ((string->word string->word) (apply apply) (string-append string-append)
	(map map) (word? word?) (word->string word->string) (whoops whoops))
    (lambda x
      (string->word
       (apply string-append
	      (map (lambda (arg)
		     (if (word? arg)
			 (word->string arg)
			 (whoops "Invalid argument to WORD: " arg)))
		   x))))))

(define se
  (let ((pair? pair?) (null? null?) (word? word?) (car car) (cons cons)
	(cdr cdr) (whoops whoops))
    (define (paranoid-append a original-a b)
      (cond ((null? a) b)
	    ((word? (car a))
	     (cons (car a) (paranoid-append (cdr a) original-a b)))
	    (else (whoops "Argument to SENTENCE not a word or sentence"
			 original-a ))))
    (define (combine-two a b)                ;; Note: b is always a list
      (cond ((pair? a) (paranoid-append a a b))
	    ((null? a) b)
	    ((word? a) (cons a b))
	    (else (whoops "Argument to SENTENCE not a word or sentence:" a))))
    ;; Helper function so recursive calls don't show up in TRACE
    (define (real-se args)
      (if (null? args)
	  '()
	  (combine-two (car args) (real-se (cdr args)))))
    (lambda args
      (real-se args))))

(define sentence se)

(define first
  (let ((pair? pair?) (char->word char->word) (string-ref string-ref)
	(word->string word->string) (car car) (empty? empty?)
	(whoops whoops) (word? word?))
    (define (word-first wd)
      (char->word (string-ref (word->string wd) 0)))
    (lambda (x)
      (cond ((pair? x) (car x))
	    ((empty? x) (whoops "Invalid argument to FIRST: " x))
	    ((word? x) (word-first x))
	    (else (whoops "Invalid argument to FIRST: " x))))))

(define last
  (let ((pair? pair?) (- -) (word->string word->string) (char->word char->word)
	(string-ref string-ref) (string-length string-length) (empty? empty?)
	(cdr cdr) (car car) (whoops whoops) (word? word?))
    (define (word-last wd)
      (let ((s (word->string wd)))
	(char->word (string-ref s (- (string-length s) 1)))))
    (define (list-last lst)      
      (if (empty? (cdr lst))
	  (car lst)
	  (list-last (cdr lst))))
    (lambda (x)
      (cond ((pair? x) (list-last x))
	    ((empty? x) (whoops "Invalid argument to LAST: " x))
	    ((word? x) (word-last x))
	    (else (whoops "Invalid argument to LAST: " x))))))

(define bf
  (let ((pair? pair?) (substring substring) (string-length string-length)
	(string->word string->word) (word->string word->string) (cdr cdr)
	(empty? empty?) (whoops whoops) (word? word?))
    (define string-bf
      (lambda (s)
      (substring s 1 (string-length s))))
    (define (word-bf wd)
      (string->word (string-bf (word->string wd))))
    (lambda (x)
      (cond ((pair? x) (cdr x))
	    ((empty? x) (whoops "Invalid argument to BUTFIRST: " x))
	    ((word? x) (word-bf x))
	    (else (whoops "Invalid argument to BUTFIRST: " x))))))

(define butfirst bf)

(define bl
  (let ((pair? pair?) (- -) (cdr cdr) (cons cons) (car car) (substring substring)
	(string-length string-length) (string->word string->word)
	(word->string word->string) (empty? empty?) (whoops whoops) (word? word?))
    (define (list-bl list)
      (if (null? (cdr list))
	  '()
	  (cons (car list) (list-bl (cdr list)))))
    (define (string-bl s)
      (substring s 0 (- (string-length s) 1)))  
    (define (word-bl wd)
      (string->word (string-bl (word->string wd))))
    (lambda (x)
      (cond ((pair? x) (list-bl x))
	    ((empty? x) (whoops "Invalid argument to BUTLAST: " x))
	    ((word? x) (word-bl x))
	    (else (whoops "Invalid argument to BUTLAST: " x))))))

(define butlast bl)

(define item
  (let ((> >) (- -) (< <) (integer? integer?) (list-ref list-ref)
	(char->word char->word) (string-ref string-ref)
	(word->string word->string) (not not) (whoops whoops)
	(count count) (word? word?) (list? list?))
    (define (word-item n wd)
      (char->word (string-ref (word->string wd) (- n 1))))
    (lambda (n stuff)
      (cond ((not (integer? n))
	     (whoops "Invalid first argument to ITEM (must be an integer): "
		     n))
	    ((< n 1)
	     (whoops "Invalid first argument to ITEM (must be positive): "
		     n))
	    ((> n (count stuff))
	     (whoops "No such item: " n stuff))
	    ((word? stuff) (word-item n stuff))
	    ((list? stuff) (list-ref stuff (- n 1)))
	    (else (whoops "Invalid second argument to ITEM: " stuff))))))

(define equal?
  ;; Note that EQUAL? assumes strings are numbers.
  ;; (strings-are-numbers #f) doesn't change this behavior.
  (let ((vector-length vector-length) (= =) (vector-ref vector-ref)
	(+ +) (string? string?) (symbol? symbol?) (null? null?) (pair? pair?)
	(car car) (cdr cdr) (eq? eq?) (string=? string=?)
	(symbol->string symbol->string) (number? number?)
	(string->word string->word) (vector? vector?) (eqv? eqv?))
    (define (vector-equal? v1 v2)
      (let ((len1 (vector-length v1))
	    (len2 (vector-length v2)))
	(define (helper i)
	  (if (= i len1)
	      #t
	      (and (equal? (vector-ref v1 i) (vector-ref v2 i))
		   (helper (+ i 1)))))
	(if (= len1 len2)
	    (helper 0)
	    #f)))
    (lambda (x y)
      (cond ((null? x) (null? y))
	    ((null? y) #f)
	    ((pair? x)
	     (and (pair? y)
		  (equal? (car x) (car y))
		  (equal? (cdr x) (cdr y))))
	    ((pair? y) #f)
	    ((symbol? x)
	     (or (and (symbol? y) (eq? x y))
		 (and (string? y) (string=? (symbol->string x) y))))
	    ((symbol? y)
	     (and (string? x) (string=? x (symbol->string y))))
	    ((number? x)
	     (or (and (number? y) (= x y))
		 (and (string? y)
		      (let ((possible-num (string->word y)))
			(and (number? possible-num)
			     (= x possible-num))))))
	    ((number? y)
	     (and (string? x)
		  (let ((possible-num (string->word x)))
		    (and (number? possible-num)
			 (= possible-num y)))))
	    ((string? x) (and (string? y) (string=? x y)))
	    ((string? y) #f)
	    ((vector? x) (and (vector? y) (vector-equal? x y)))
	    ((vector? y) #f)
	    (else (eqv? x y))))))

(define member?
  (let ((> >) (- -) (< <) (null? null?) (symbol? symbol?) (eq? eq?) (car car)
	(not not) (symbol->string symbol->string) (string=? string=?)
	(cdr cdr) (equal? equal?) (word->string word->string)
	(string-length string-length) (whoops whoops) (string-ref string-ref)
	(char=? char=?) (list? list?) (number? number?) (empty? empty?)
	(word? word?) (string? string?))
    (define (symbol-in-list? symbol string lst)
      (cond ((null? lst) #f)
	    ((and (symbol? (car lst))
		  (eq? symbol (car lst))))
	    ((string? (car lst))
	     (cond ((not string)
		    (symbol-in-list? symbol (symbol->string symbol) lst))
		   ((string=? string (car lst)) #t)
		   (else (symbol-in-list? symbol string (cdr lst)))))
	    (else (symbol-in-list? symbol string (cdr lst)))))
    (define (word-in-list? wd lst)
      (cond ((null? lst) #f)
	    ((equal? wd (car lst)) #t)
	    (else (word-in-list? wd (cdr lst)))))
    (define (word-in-word? small big)
      (let ((one-letter-str (word->string small)))
	(if (> (string-length one-letter-str) 1)
	    (whoops "Invalid arguments to MEMBER?: " small big)
	    (let ((big-str (word->string big)))
	      (char-in-string? (string-ref one-letter-str 0)
			       big-str
			       (- (string-length big-str) 1))))))
    (define (char-in-string? char string i)
      (cond ((< i 0) #f)
	    ((char=? char (string-ref string i)) #t)
	    (else (char-in-string? char string (- i 1)))))
    (lambda (x stuff)
      (cond ((empty? stuff) #f)
	    ((word? stuff) (word-in-word? x stuff))
	    ((not (list? stuff))
	     (whoops "Invalid second argument to MEMBER?: " stuff))
	    ((symbol? x) (symbol-in-list? x #f stuff))
	    ((or (number? x) (string? x))
	     (word-in-list? x stuff))
	    (else (whoops "Invalid first argument to MEMBER?: " x))))))

(define before?
  (let ((not not) (word? word?) (whoops whoops) (string<? string<?)
	(word->string word->string))
    (lambda (wd1 wd2)
      (cond ((not (word? wd1))
	     (whoops "Invalid first argument to BEFORE? (not a word): " wd1))
	    ((not (word? wd2))
	     (whoops "Invalid second argument to BEFORE? (not a word): " wd2))
	    (else (string<? (word->string wd1) (word->string wd2)))))))


;;; Higher Order Functions

(define filter
  (let ((null? null?) (car car) (cons cons) (cdr cdr) (not not)
	(procedure? procedure?) (whoops whoops) (list? list?))
    (lambda (pred l)
      ;; Helper function so recursive calls don't show up in TRACE
      (define (real-filter l)
	(cond ((null? l) '())
	      ((pred (car l))
	       (cons (car l) (real-filter (cdr l))))
	      (else (real-filter (cdr l)))))
      (cond ((not (procedure? pred))
	     (whoops "Invalid first argument to FILTER (not a procedure): "
		     pred))
	    ((not (list? l))
	     (whoops "Invalid second argument to FILTER (not a list): " l))
	    (else (real-filter l))))))

(define keep
  (let ((+ +) (= =) (pair? pair?) (substring substring)
	(char->word char->word) (string-ref string-ref)
	(string-set! string-set!) (word->string word->string)
	(string-length string-length) (string->word string->word)
	(make-string make-string) (procedure? procedure?)
	(whoops whoops) (word? word?) (null? null?))
    (lambda (pred w-or-s)
      (define (keep-string in i out out-len len)
	(cond ((= i len) (substring out 0 out-len))
	      ((pred (char->word (string-ref in i)))
	       (string-set! out out-len (string-ref in i))
	       (keep-string in (+ i 1) out (+ out-len 1) len))
	      (else (keep-string in (+ i 1) out out-len len))))
      (define (keep-word wd)
	(let* ((string (word->string wd))
	       (len (string-length string)))
	  (string->word
	   (keep-string string 0 (make-string len) 0 len))))
      (cond ((not (procedure? pred))
	     (whoops "Invalid first argument to KEEP (not a procedure): "
		    pred))
	    ((pair? w-or-s) (filter pred w-or-s))
	    ((word? w-or-s) (keep-word w-or-s))
	    ((null? w-or-s) '())
	    (else
	     (whoops "Bad second argument to KEEP (not a word or sentence): "
		     w-or-s))))))

(define appearances
  (let ((count count) (keep keep) (equal? equal?))
    (lambda (item aggregate)
      (count (keep (lambda (element) (equal? item element)) aggregate)))))

(define every
  (let ((= =) (+ +) (se se) (char->word char->word) (string-ref string-ref)
	(empty? empty?) (first first) (bf bf) (not not) (procedure? procedure?)
	(whoops whoops) (word? word?) (word->string word->string)
	(string-length string-length))
    (lambda (fn stuff)
      (define (string-every string i length)
	(if (= i length)
	    '()
	    (se (fn (char->word (string-ref string i)))
		(string-every string (+ i 1) length))))
      (define (sent-every sent)
	;; This proc. can't be optimized or else it will break the
	;; exercise where we ask them to reimplement sentences as
	;; vectors and then see if every still works.
	(if (empty? sent)
	    sent		; Can't be '() or exercise breaks.
	    (se (fn (first sent))    
		(sent-every (bf sent)))))
      (cond ((not (procedure? fn))
	     (whoops "Invalid first argument to EVERY (not a procedure):"
		     fn))
	    ((word? stuff)
	     (let ((string (word->string stuff)))
	       (string-every string 0 (string-length string))))
	    (else (sent-every stuff))))))

(define accumulate
  (let ((not not) (empty? empty?) (bf bf) (first first) (procedure? procedure?)
	(whoops whoops) (member member) (list list))
    (lambda (combiner stuff)
      (define (real-accumulate stuff)
	(if (empty? (bf stuff))
	    (first stuff)
	    (combiner (first stuff) (real-accumulate (bf stuff)))))
      (cond ((not (procedure? combiner))
	     (whoops "Invalid first argument to ACCUMULATE (not a procedure):"
		     combiner))
	    ((not (empty? stuff)) (real-accumulate stuff))
	    ((member combiner (list + * word se)) (combiner))
	    (else
	     (whoops "Can't accumulate empty input with that combiner"))))))

(define reduce
  (let ((null? null?) (cdr cdr) (car car) (not not) (procedure? procedure?)
	(whoops whoops) (member member) (list list))
    (lambda (combiner stuff)
      (define (real-reduce stuff)
	(if (null? (cdr stuff))
	    (car stuff)
	    (combiner (car stuff) (real-reduce (cdr stuff)))))
      (cond ((not (procedure? combiner))
	     (whoops "Invalid first argument to REDUCE (not a procedure):"
		     combiner))
	    ((not (null? stuff)) (real-reduce stuff))
	    ((member combiner (list + * word se append)) (combiner))
	    (else (whoops "Can't reduce empty input with that combiner"))))))

(define repeated
  (let ((= =) (- -))
    (lambda (fn number)
      (if (= number 0)
	  (lambda (x) x)
	  (lambda (x)
	    ((repeated fn (- number 1)) (fn x)))))))


;; Tree stuff
(define make-node cons)
(define datum car)
(define children cdr)


;; I/O
        
(define show
  (let ((= =) (length length) (display display) (car car) (newline newline)
	(not not) (output-port? output-port?) (apply apply) (whoops whoops))
    (lambda args
      (cond
       ((= (length args) 1)
	(display (car args))
	(newline))
       ((= (length args) 2)
	(if (not (output-port? (car (cdr args))))
	    (whoops "Invalid second argument to SHOW (not an output port): "
		    (car (cdr args))))
	(apply display args)
	(newline (car (cdr args))))
       (else (whoops "Incorrect number of arguments to procedure SHOW"))))))

(define show-line
  (let ((>= >=) (length length) (whoops whoops) (null? null?)
	(current-output-port current-output-port) (car car) (not not)
	(list? list?) (display display) (for-each for-each) (cdr cdr)
	(newline newline))
    (lambda (line . args)
      (if (>= (length args) 2)
	  (whoops "Too many arguments to show-line")
	  (let ((port (if (null? args) (current-output-port) (car args))))
	    (cond ((not (list? line))
		   (whoops "Invalid argument to SHOW-LINE (not a list):" line))
		  ((null? line) #f)
		  (else
		   (display (car line) port)
		   (for-each (lambda (wd) (display " " port) (display wd port))
			     (cdr line))))
	    (newline port))))))

(define read-string
  (let ((read-char read-char) (eqv? eqv?) (apply apply)
	(string-append string-append) (substring substring) (reverse reverse)
	(cons cons) (>= >=) (+ +) (string-set! string-set!) (length length)
	(whoops whoops) (null? null?) (current-input-port current-input-port)
	(car car) (cdr cdr) (eof-object? eof-object?) (list list)
	(make-string make-string) (peek-char peek-char))
    (define (read-string-helper chars all-length chunk-length port)
      (let ((char (read-char port))
	    (string (car chars)))
	(cond ((or (eof-object? char) (eqv? char #\newline))
	       (apply string-append
		      (reverse
		       (cons
			(substring (car chars) 0 chunk-length)
			(cdr chars)))))
	      ((>= chunk-length 80)
	       (let ((newstring (make-string 80)))
		 (string-set! newstring 0 char)
		 (read-string-helper (cons newstring chars)
				     (+ all-length 1)
				     1
				     port)))
	      (else
	       (string-set! string chunk-length char)
	       (read-string-helper chars
				   (+ all-length 1)
				   (+ chunk-length 1)
				   port)))))
    (lambda args
      (if (>= (length args) 2)
	  (whoops "Too many arguments to read-string")
	  (let ((port (if (null? args) (current-input-port) (car args))))
	    (if (eof-object? (peek-char port))
		(read-char port)
		(read-string-helper (list (make-string 80)) 0 0 port)))))))

(define read-line
  (let ((= =) (list list) (string->word string->word) (substring substring)
	(char-whitespace? char-whitespace?) (string-ref string-ref)
	(+ +) (string-length string-length) (apply apply)
	(read-string read-string))
    (lambda args
      (define (tokenize string)
	(define (helper i start len)
	  (cond ((= i len)
		 (if (= i start)
		     '()
		     (list (string->word (substring string start i)))))
		((char-whitespace? (string-ref string i))
		 (if (= i start)
		     (helper (+ i 1) (+ i 1) len)
		     (cons (string->word (substring string start i))
			   (helper (+ i 1) (+ i 1) len))))
		(else (helper (+ i 1) start len))))
        (if (eof-object? string)
            string
            (helper 0 0 (string-length string))))
      (tokenize (apply read-string args)))))

(define *the-open-inports* '())
(define *the-open-outports* '())

(define align
  (let ((< <) (abs abs) (* *) (expt expt) (>= >=) (- -) (+ +) (= =)
	(null? null?) (car car) (round round) (number->string number->string)
	(string-length string-length) (string-append string-append)
	(make-string make-string) (substring substring)
	(string-set! string-set!) (number? number?)
	(word->string word->string))
    (lambda (obj width . rest)
      (define (align-number obj width rest)
	(let* ((sign (< obj 0))
	       (num (abs obj))
	       (prec (if (null? rest) 0 (car rest)))
	       (big (round (* num (expt 10 prec))))
	       (cvt0 (number->string big))
	       (cvt (if (< num 1) (string-append "0" cvt0) cvt0))
	       (pos-str (if (>= (string-length cvt0) prec)
			    cvt
			    (string-append
			     (make-string (- prec (string-length cvt0)) #\0)
			     cvt)))
	       (string (if sign (string-append "-" pos-str) pos-str))
	       (length (+ (string-length string)
			  (if (= prec 0) 0 1)))
	       (left (- length (+ 1 prec)))
	       (result (if (= prec 0)
			   string
			   (string-append
			    (substring string 0 left)
			    "."
			    (substring string left (- length 1))))))
	  (cond ((= length width) result)
		((< length width)
		 (string-append (make-string (- width length) #\space) result))
		(else (let ((new (substring result 0 width)))
			(string-set! new (- width 1) #\+)
			new)))))
      (define (align-word string)
	(let ((length (string-length string)))
	  (cond ((= length width) string)
		((< length width)
		 (string-append string (make-string (- width length) #\space)))
		(else (let ((new (substring string 0 width)))
			(string-set! new (- width 1) #\+)
			new)))))
      (if (number? obj)
	  (align-number obj width rest)
	  (align-word (word->string obj))))))

(define open-output-file
  (let ((oof open-output-file) (cons cons))
    (lambda (filename)
      (let ((port (oof filename)))
	(set! *the-open-outports* (cons port *the-open-outports*))
	port))))

(define open-input-file
  (let ((oif open-input-file) (cons cons))
    (lambda (filename)
      (let ((port (oif filename)))
	(set! *the-open-inports* (cons port *the-open-inports*))
	port))))

(define remove!
  (let ((null? null?) (cdr cdr) (eq? eq?) (set-cdr! set-cdr!) (car car))
    (lambda (thing lst)
      (define (r! prev)
	(cond ((null? (cdr prev)) lst)
	      ((eq? thing (car (cdr prev)))
	       (set-cdr! prev (cdr (cdr prev)))
	       lst)
	      (else (r! (cdr prev)))))
      (cond ((null? lst) lst)
	    ((eq? thing (car lst)) (cdr lst))
	    (else (r! lst))))))

(define close-input-port
  (let ((cip close-input-port) (remove! remove!))
    (lambda (port)
      (set! *the-open-inports* (remove! port *the-open-inports*))
      (cip port))))

(define close-output-port
  (let ((cop close-output-port) (remove! remove!))
    (lambda (port)
      (set! *the-open-outports* (remove! port *the-open-outports*))
      (cop port))))

(define close-all-ports
  (let ((for-each for-each)
	(close-input-port close-input-port)
	(close-output-port close-output-port))
    (lambda ()
      (for-each close-input-port *the-open-inports*)
      (for-each close-output-port *the-open-outports*)
      'closed)))

;; Make arithmetic work on numbers in string form:
(define maybe-num
  (let ((string? string?) (string->number string->number))
    (lambda (arg)
      (if (string? arg)
	  (let ((num (string->number arg)))
	    (if num num arg))
	  arg))))

(define logoize
  (let ((apply apply) (map map) (maybe-num maybe-num))
    (lambda (fn)
      (lambda args
	(apply fn (map maybe-num args))))))

;; special case versions of logoize, since (lambda args ...) is expensive
(define logoize-1
  (let ((maybe-num maybe-num))
    (lambda (fn)
      (lambda (x) (fn (maybe-num x))))))

(define logoize-2
  (let ((maybe-num maybe-num))
    (lambda (fn)
      (lambda (x y) (fn (maybe-num x) (maybe-num y))))))

(define strings-are-numbers
  (let ((are-they? #f)
        (real-* *) (real-+ +) (real-- -) (real-/ /) (real-< <)
        (real-<= <=) (real-= =) (real-> >) (real->= >=) (real-abs abs)
        (real-acos acos) (real-asin asin) (real-atan atan)
        (real-ceiling ceiling) (real-cos cos) (real-even? even?)
        (real-exp exp) (real-expt expt) (real-floor floor) (real-align align)
        (real-gcd gcd) (real-integer? integer?) (real-item item)
        (real-lcm lcm) (real-list-ref list-ref) (real-log log)
        (real-make-vector make-vector) (real-max max) (real-min min)
        (real-modulo modulo) (real-negative? negative?)
        (real-number? number?) (real-odd? odd?) (real-positive? positive?)
        (real-quotient quotient) (real-random random) (real-remainder remainder)
        (real-repeated repeated) (real-round round) (real-sin sin)
        (real-sqrt sqrt) (real-tan tan) (real-truncate truncate)
        (real-vector-ref vector-ref) (real-vector-set! vector-set!)
        (real-zero? zero?) (maybe-num maybe-num) (number->string number->string)
	(cons cons) (car car) (cdr cdr) (eq? eq?) (show show) (logoize logoize)
	(logoize-1 logoize-1) (logoize-2 logoize-2) (not not) (whoops whoops))

    (lambda (yesno)
      (cond ((and are-they? (eq? yesno #t))
	     (show "Strings are already numbers"))
	    ((eq? yesno #t)
	     (set! are-they? #t)
	     (set! * (logoize real-*))
	     (set! + (logoize real-+))
	     (set! - (logoize real--))
	     (set! / (logoize real-/))
	     (set! < (logoize real-<))
	     (set! <= (logoize real-<=))
	     (set! = (logoize real-=))
	     (set! > (logoize real->))
	     (set! >= (logoize real->=))
	     (set! abs (logoize-1 real-abs))
	     (set! acos (logoize-1 real-acos))
	     (set! asin (logoize-1 real-asin))
	     (set! atan (logoize real-atan))
	     (set! ceiling (logoize-1 real-ceiling))
	     (set! cos (logoize-1 real-cos))
	     (set! even? (logoize-1 real-even?))
	     (set! exp (logoize-1 real-exp))
	     (set! expt (logoize-2 real-expt))
	     (set! floor (logoize-1 real-floor))
	     (set! align (logoize align))
	     (set! gcd (logoize real-gcd))
	     (set! integer? (logoize-1 real-integer?))
	     (set! item (lambda (n stuff)
			  (real-item (maybe-num n) stuff)))
	     (set! lcm (logoize real-lcm))
	     (set! list-ref (lambda (lst k) 
			      (real-list-ref lst (maybe-num k))))
	     (set! log (logoize-1 real-log))
	     (set! max (logoize real-max))
	     (set! min (logoize real-min))
	     (set! modulo (logoize-2 real-modulo))
	     (set! negative? (logoize-1 real-negative?))
	     (set! number? (logoize-1 real-number?))
	     (set! odd? (logoize-1 real-odd?))
	     (set! positive? (logoize-1 real-positive?))
	     (set! quotient (logoize-2 real-quotient))
	     (set! random (logoize real-random))
	     (set! remainder (logoize-2 real-remainder))
	     (set! round (logoize-1 real-round))
	     (set! sin (logoize-1 real-sin))
	     (set! sqrt (logoize-1 real-sqrt))

	     (set! tan (logoize-1 real-tan))
	     (set! truncate (logoize-1 real-truncate))
	     (set! zero? (logoize-1 real-zero?))
	     (set! vector-ref
		   (lambda (vec i) (real-vector-ref vec (maybe-num i))))
	     (set! vector-set!
		   (lambda (vec i val)
		     (real-vector-set! vec (maybe-num i) val)))
	     (set! make-vector
		   (lambda (num . args)
		     (apply real-make-vector (cons (maybe-num num)
						   args))))
	     (set! list-ref
		   (lambda (lst i) (real-list-ref lst (maybe-num i))))
	     (set! repeated
		   (lambda (fn n) (real-repeated fn (maybe-num n)))))
	    ((and (not are-they?) (not yesno))
	     (show "Strings are already not numbers"))
	    ((not yesno)
	     (set! are-they? #f) (set! * real-*) (set! + real-+)
	     (set! - real--) (set! / real-/) (set! < real-<)
	     (set! <= real-<=) (set! = real-=) (set! > real->)
	     (set! >= real->=) (set! abs real-abs) (set! acos real-acos)
	     (set! asin real-asin) (set! atan real-atan)
	     (set! ceiling real-ceiling) (set! cos real-cos)
	     (set! even? real-even?)
	     (set! exp real-exp) (set! expt real-expt)
	     (set! floor real-floor) (set! align real-align)
	     (set! gcd real-gcd) (set! integer? real-integer?)
	     (set! item real-item)
	     (set! lcm real-lcm) (set! list-ref real-list-ref)
	     (set! log real-log) (set! max real-max) (set! min real-min)
	     (set! modulo real-modulo) (set! odd? real-odd?)
	     (set! quotient real-quotient) (set! random real-random)
	     (set! remainder real-remainder) (set! round real-round)
	     (set! sin real-sin) (set! sqrt real-sqrt) (set! tan real-tan)
	     (set! truncate real-truncate) (set! zero? real-zero?)
	     (set! positive? real-positive?) (set! negative? real-negative?)
	     (set! number? real-number?) (set! vector-ref real-vector-ref)
	     (set! vector-set! real-vector-set!)
	     (set! make-vector real-make-vector)
	     (set! list-ref real-list-ref) (set! item real-item)
	     (set! repeated real-repeated))
	    (else (whoops "Strings-are-numbers: give a #t or a #f")))
	    are-they?)))


;; By default, strings are numbers:
(strings-are-numbers #t)
