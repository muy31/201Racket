#lang racket

(require racket/trace)

(provide hours
	 xxxx
	 depth
	 sum
	 prod
	 count-if
	 average
	 types
	 tree-replace
	 tree-min
	 count-leaves
	 map-tree
	 )

; ********************************************************
; CS 201 HW #2  DUE Monday 10/4/21, 11:59 pm
;                via the submit system on the Zoo

; ********************************************************
; Name: Muyi Aghedo
; Email address: muyi.aghedo@yale.edu
; ********************************************************

; This file may be opened in DrRacket.  Lines beginning with
; semicolons are comments.

; If you are asked to write a procedure, please make sure it has the
; specified name, and the specified number and order of arguments.
; The names of the formal arguments need not be the same as in the
; problem specification.

; For each problem, the intended inputs to your procedures are
; specified (for example, "positive integers") and your procedures
; need not do anything reasonable for other possible inputs.

; You may write auxiliary procedures in addition to the requested
; one(s) -- for each of your auxiliary procedures, please include a
; comment explaining what it does, and giving an example or two.

; You may also use procedures you have written elsewhere in this
; assignment or previous assignments.  They only need to be defined
; once somewhere within this file.

; Please use the predicate equal? to test equality of values that may
; not be numbers.  To test equality of numbers, you can use =.

; Also, most of these procedures involve car/cdr recursion.  In that
; case, your code needs to implement the recursion, not simply invoke
; a racket procedure (like flatten) that allows you to avoid the problem.

; Reading: Chapters 1 and 2 of the Racket Guide.

; ********************************************************
; ** problem 0 ** (1 easy point) 
; Replace the number 0 in the definition below to indicate
; the number of hours you spent doing this assignment
; Decimal numbers (eg, 6.237) are fine.  Exclude time
; spent reading.

(define hours 5)

; ********************************************************
; ** problem 00 ** (1 fairly easy point)

; Below is a UNIX transcript with one command replaced by XXXX

(define transcript "
bash-4.4$ pwd
/home/accts/sbs5/cs201/hws/new
bash-4.4$ ls 
bash-4.4$ touch file
bash-4.4$ ls -l
total 0
-rw-rw-r-- 1 sbs5 cs201ta 0 Jan 26 18:27 file
bash-4.4$ XXXX
bash-4.4$ ls -l
total 0
-rw-rw-r-- 1 sbs5 cs201ta 0 Jan 26 18:27 homework
")

; define xxxx below to be the correct UNIX command.

(define xxxx "mv file homework")

; ********************************************************
; ** problem 1 ** (8 points)
; Write a procedure

; (depth tree)

; which takes a tree as input and return an integer 
; indicating the maximum level of the tree.

; Examples

; (depth '()) => 0
; (depth '(1 2 3)) => 1
; (depth '(a (b (c (d))))) => 4
; (depth '((((((0))))))) => 6

; ********************************************************
(define (depth tree)
  
  (if (or (null? tree) (not (list? tree)))
      0
      (if (list? (first tree))
          
          (if (< (+ 1 (depth (first tree))) (depth (rest tree)))
              (depth (rest tree))
              (+ 1 (depth (first tree)))
             )
          
          (if (< 1 (depth (rest tree)))
              (depth (rest tree))
              1
             )
       )
  )
)

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 2 ** (10 points)
; Write a procedure

; (sum tree)

;; which returns the total of the numeric leaves of the tree.
;; That is, add up all the numeric leaves.

;; Examples:

;; (sum '(1 2 3 4)) => 10
;; (sum '(a (1 (b (2 (3 "four")))))) => 6
;; (sum '(((((((((8)))))))))) => 8
;; (sum '((((((((()))))))))) => 0

; (Replace this comment with your procedure(s).)

(define (sum tree)
  (if (or (null? tree) (equal? (list? tree) #f))
      ;;empty list (base case)
      (if(number? tree) tree 0)

      (cond [(= (depth tree) 1) (apply + (map (lambda(x) (if(number? x) x 0)) tree))]
            [else (+ (sum (first tree)) (sum (rest tree)) )]

      )
  )
)


; ********************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (prod tree)

;; which returns the product of all the leaves of the tree.
;; It ignores leaves which are not numbers.
;; If there are no numbers in the tree, prod return 'none

;; Examples:

;; (prod '(1 2 3 4 5)) => 120
;; (prod '(((((((((((8)))))))))))) => 8
;; (prod '(())) => 'none
;; (prod '(a b c d e f g)) => 'none
;; (prod '(a b c d e f g 8)) => 8
;; (prod '(a (1 (b (2 (3 "four")))))) => 6


; (Replace this comment with your procedure(s).)

(define (prodHelper tree)
    (if (or (null? tree) (equal? (list? tree) #f))
      ;;empty list (base case)
      (if(number? tree) tree 1)

      (cond [(= (depth tree) 1) (apply * (map (lambda(x) (if(number? x) x 1)) tree))]
            [else (* (prodHelper (first tree)) (prodHelper (rest tree)) )]

      )
  )
)

(define (prod tree)
  (if (= (prodHelper tree) 1) 'none (prodHelper tree))
)


; ********************************************************
; ** problem 4 ** (10 points)
; Write a procedure

;; (count-if pred tree)

;; which returns the number of leaves of the tree that satisfy
;; the given predicate pred

; Examples
 
; (count-if odd? '(1 2 3)) => 2
; (count-if even? '(1 2 3)) => 1
; (count-if integer? '(1 (2 (3)))) => 3
; (count-if string? '()) => 0
; (count-if even? '((((((8 8 8))))))) => 3
; (count-if (lambda (x) (> x 5)) '((((((9 9 9))))))) => 3

; ********************************************************
; (Replace this comment with your procedure(s).)

(define (count-if pred tree)
  (cond [(null? tree) 0]
        [(not (list? tree)) (if (pred tree) 1 0)]
        [else (+ (count-if pred (first tree)) (count-if pred (rest tree)))])
)



; ********************************************************
; ** problem 5 ** (10 points)
; Write a procedure

; (average tree)

;; that takes a nested list tree and returns the average of the
;; numeric elements of the tree.
;; If the tree contains no elements or no numeric elements, return 'NA

;; Examples

;; (average '(1 2 3 4 5)) => 3
;; (average '(1 2 3 4 5 a b c d e)) => 3
;; (average '(1 (2 (3 d e f) (4 5 6 7)))) => 4
;; (average '((()))) => 'NA
;; (average '(a b c d e)) => 'NA
;; (average '(a b c d e 2 3 4)) => 3
;; (average '()) => 'NA
;; (average '(1.2 1.3 1.4 1.5)) => 1.35

;; (Replace this comment with your procedure(s).)


(define (average tree)
  (if (= (count-if number? tree) 0)
      'NA
      (/ (sum tree) (count-if number? tree))
      )
  )

; ********************************************************
; ** problem 6 ** (10 points)
; Write a procedure

; (types tree)

; that takes a nested list tree and returns a list of the data
; types of the leaves in the tree in alphabetical order

; Examples

;; (types '(1 1 1 1)) => '(integer)
;; (types '(1 ((a)))) => '(integer symbol)
;; (types '(1 (("a")))) => '(integer string)
;; (types '(1 ((#\a)))) => '(character integer)
;; (types '(1.0 ((#\a)))) => '(character flonum)
;; (types '(1.0 (("a")))) => '(flonum string)
;; (types '(2/3 .666 "two-thirds")) => '(flonum rational string)

; ********************************************************

(define (typeof item)
  (cond [(flonum? item) 'flonum]
        [(integer? item) 'integer]
        [(rational? item) 'rational]
        [(string? item) 'string]
        [(boolean? item) 'boolean]
        [(symbol? item) 'symbol]
        [(char? item) 'character]
        [else 'unknown]
        )
  )

(define (types tree)
  (cond [(null? tree) '()]
        [(not (list? tree)) (typeof tree)]
        [else (sort (remove-duplicates (append (if (list? (types (first tree))) (types (first tree)) (list (types (first tree)))) (types (rest tree)))) symbol<?)])
  )

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (tree-replace old new tree)

; that returns a copy of the given tree with each element that is
; equal to old replaced by new


; Examples:

;; (tree-replace 1 2 '(1 2 3 4 5)) => '(2 2 3 4 5)
;; (tree-replace 1 2 '((1) (1 (2)))) => '((2) (2 (2)))
;; (tree-replace 2 1 '((1) (1 (2)))) => '((1) (1 (1)))
;; (tree-replace 1 2 '((((((1))))))) => '((((((2))))))
;; (tree-replace '((1)) 2 '((((((1))))))) => '((((2))))
;; (tree-replace 'x 'y '(a b x y x a z)) => '(a b y y y a z)
;; (tree-replace 'q 'y '(a b x y x a z)) => '(a b x y x a z)

; ********************************************************
(define (tree-replace old new tree)

  (cond [(null? tree) '()]
        [(not (list? tree))  (if (equal? tree old) new tree)]
        [(equal? tree old) new]
        [else (append (list (tree-replace old new (first tree)))  (tree-replace old new (rest tree)))])
  
)

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 8 ** (10 points)
; Write a procedure 

; (tree-min tree)

; that takes a nested list tree whose leaf nodes are integers 
; and returns the minimum of those integers.

; Examples:

; (tree-min '(1 2 3)) => 1
; (tree-min '(1 (2 (-3)))) => -3
; (tree-min '()) => '()
; (tree-min '(((((((((7)))))))))) => 7
; (tree-min '((((((((()))))))))) => '()

; ********************************************************
(define (tree-minHelper tree)
  (cond [(null? tree) +inf.0]
        [(not (list? tree)) (if (number? tree) tree +inf.0)]
        [else (if (< (tree-minHelper (first tree)) (tree-minHelper (rest tree))) (tree-minHelper (first tree)) (tree-minHelper (rest tree)))])
  )

(define (tree-min tree)
  (if (equal? (tree-minHelper tree) +inf.0) '() (tree-minHelper tree)))

; (Replace this comment with your procedure(s).)

; ********************************************************
; ** problem 9 ** (10 points)
; Write the procedure

; (count-leaves tree)

; count-leaves takes a nest list tree as an argument and returns
; an integer which is the number of leaves in the tree.

; Examples:

; (count-leaves '(1 2 3)) => 3
; (count-leaves '()) => 0
; (count-leaves '(1 (2 (3 (4))))) => 4
; (count-leaves '(((((((7)))))))) => 1

;; Hint: this is very similar to count-if.  In fact, you may define
;; count-leaves using count-if

; ********************************************************
(define (count-leaves tree)
  (cond [(null? tree) 0]
        [(not (list? tree)) 1]
        [else (+ (count-leaves (first tree)) (count-leaves (rest tree)))]
        )
  )

; (Replace this comment with your procedures.)

; ********************************************************
; ** problem 10 ** (10 points)
; Write a procedure

; (map-tree proc tree)

; which takes two arguments, a procedure proc and a nested list tree,
; and returns a copy of tree with each leaf node replaced by
; the result of applying proc to that leaf.

; Examples:

; (map-tree even? '(1 2 3 4)) => '(#f #t #f #t)
; (map-tree even? '(1 (2 (3 (4))))) => '(#f (#t (#f (#t))))
; (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) => '(2 (3 (4 (5 6 7))))
; (map-tree odd? '()) => '()

; ********************************************************
(define (map-tree proc tree)
 (cond [(null? tree) '()]
        [(not (list? tree)) (proc tree)]
        [else (append (list (map-tree proc (first tree))) (map-tree proc (rest tree)))])
  
  )

; (Replace this comment with your procedure(s).)


; ********************************************************
; ********************************************************
; ********  testing, testing. 1, 2, 3 ....
; ********************************************************

(define *testing-flag* #t)

(define (testold name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))
	
(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((OK (if (procedure? expected)
		    	(expected got)
			(equal? got expected)))
		(prefix (if OK
			    'OK
			    'X)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))


(test 'hours hours (lambda (x) (> x 0)))
(test 'xxxx xxxx "cannot test without giving it away")

(test 'depth (depth '())  0)
(test 'depth (depth '(1 2 3)) 1)
(test 'depth (depth '(a (b (c (d))))) 4)
(test 'depth (depth '((((((0))))))) 6)
	
(test 'sum (sum '(1 2 3 4)) 10)
(test 'sum (sum '(a (1 (b (2 (3 "four")))))) 6)
(test 'sum (sum '(((((((((8)))))))))) 8)
(test 'sum (sum '((((((((()))))))))) 0)

(test 'prod (prod '(1 2 3 4 5)) 120)
(test 'prod (prod '(((((((((((8)))))))))))) 8)
(test 'prod (prod '(())) 'none)
(test 'prod (prod '(a b c d e f g)) 'none)
(test 'prod (prod '(a b c d e f g 8)) 8)
(test 'prod (prod '(a (1 (b (2 (3 "four")))))) 6)

(test 'count-if (count-if odd? '(1 2 3)) 2)
(test 'count-if (count-if even? '(1 2 3)) 1)
(test 'count-if (count-if integer? '(1 (2 (3)))) 3)
(test 'count-if (count-if string? '()) 0)
(test 'count-if (count-if even? '((((((8 8 8))))))) 3)
(test 'count-if (count-if (lambda (x) (> x 5)) '((((((9 9 9))))))) 3)

(test 'average (average '(1 2 3 4 5)) 3)
(test 'average (average '(1 2 3 4 5 a b c d e)) 3)
(test 'average (average '(1 (2 (3 d e f) (4 5 6 7)))) 4)
(test 'average (average '((()))) 'NA)
(test 'average (average '(a b c d e)) 'NA)
(test 'average (average '(a b c d e 2 3 4)) 3)
(test 'average (average '()) 'NA)
(test 'average (average '(1.2 1.3 1.4 1.5)) 1.35)

(test 'types (types '(1 1 1 1)) '(integer))
(test 'types (types '(1 ((a)))) '(integer symbol))
(test 'types (types '(1 (("a")))) '(integer string))
(test 'types (types '(1 ((#\a)))) '(character integer))
(test 'types (types '(1.0 ((#\a)))) '(character flonum))
(test 'types (types '(1.0 (("a")))) '(flonum string))
(test 'types (types '(2/3 .667 "two-thirds")) '(flonum rational string))

(test 'tree-replace (tree-replace 1 2 '(1 2 3 4 5)) '(2 2 3 4 5))
(test 'tree-replace (tree-replace 1 2 '((1) (1 (2)))) '((2) (2 (2))))
(test 'tree-replace (tree-replace 2 1 '((1) (1 (2)))) '((1) (1 (1))))
(test 'tree-replace (tree-replace 1 2 '((((((1))))))) '((((((2)))))))
(test 'tree-replace (tree-replace '((1)) 2 '((((((1))))))) '((((2)))))
(test 'tree-replace (tree-replace 'x 'y '(a b x y x a z)) '(a b y y y a z))
(test 'tree-replace (tree-replace 'q 'y '(a b x y x a z)) '(a b x y x a z))

(test 'tree-min (tree-min '(1 2 3)) 1)
(test 'tree-min (tree-min '(1 (2 (-3)))) -3)
(test 'tree-min (tree-min '()) '())
(test 'tree-min (tree-min '(((((((((7)))))))))) 7)
(test 'tree-min (tree-min '((((((((()))))))))) '())

(test 'count-leaves (count-leaves '(1 2 3)) 3)
(test 'count-leaves (count-leaves '()) 0)
(test 'count-leaves (count-leaves '(1 (2 (3 (4))))) 4)
(test 'count-leaves (count-leaves '(((((((7)))))))) 1)

(test 'map-tree (map-tree even? '(1 2 3 4)) '(#f #t #f #t))
(test 'map-tree (map-tree even? '(1 (2 (3 (4))))) '(#f (#t (#f (#t)))))
(test 'map-tree (map-tree (lambda (x) (+ x 1)) '(1 (2 (3 (4 5 6))))) '(2 (3 (4 (5 6 7)))))
(test 'map-tree (map-tree odd? '()) '())

; ========================================================
; **** end of hw #2
; ========================================================