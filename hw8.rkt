#lang racket

(provide 
 lorint time-calls
 total-order?
 sorted? insert merge
 isort msort
 count-compares
 make-queue)

; Please do not change lines above this one.

;************************************************************
; CS 201 HW #8  DUE Friday December 10th at 11:59 pm, 
; via the submit system on the Zoo. 
;************************************************************
; Name: Muyi Aghedo
; Email address: muyi.aghedo@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (10 points)

; Write two procedures

; (lorint count bound)
; (time-calls reps proc args)

; (lorint count bound) takes a nonnegative
; integer count and a positive integer bound
; and returns a list of count randomly chosen integers 
; in the range from 0 through bound - 1.

; (time-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to args with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Examples of lorint
;> (lorint 10 100)
;'(49 14 28 15 12 80 33 69 18 57)
;> (lorint 10 3)
;'(0 0 2 1 0 0 1 2 0 1)

; The following examples of time-calls were run on my workstation and
; show that calling the built-in plus procedure 10,000 times on
; the arguments 13 and 14 took somewhat more than 0.001 seconds,
; while doing the same thing 100,000 times took somewhat more
; than 0.01 seconds, and a million times took somewhat more than 0.1
; seconds.  The first two runs show random variation in the measured times.

; When the number of repetitions is multiplied by 10, the time is
; also (approximately) multiplied by 10.

;> (time-calls 10000 + (list 13 14))
;0.00168701171875
;> (time-calls 10000 + (list 13 14))
;0.00122412109375
;> (time-calls 100000 + (list 13 14))
;0.012380859375
;> (time-calls 1000000 + (list 13 14))
;0.12706494140625

; The following examples show timings (on my workstation)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers in the range 0 to 9 inclusive.
; About a third of a second suffices in the last case.

;> (time-calls 1 lorint (list 100000 10))
;0.074503173828125
;> (time-calls 1 lorint (list 200000 10))
;0.19560009765625
;> (time-calls 1 lorint (list 300000 10))
;0.33381982421875
;******************************(******************************

(define (lorint count bound)
  (cond [(= count 0) '()]
        [else (append (list (random 0 bound)) (lorint (- count 1) bound))]
        )
  )

(define (time-calls reps proc args)
  (define stTime (current-inexact-monotonic-milliseconds))
  (for ([i reps])
    (apply proc args)
    )
   (/ (-  (current-inexact-monotonic-milliseconds) stTime) 1000)
  )

;************************************************************
; ** problem 2 ** (15 points)
; For this problem, use your procedure time-calls
; to time the built-in Racket procedures:

; length, take, drop

; and report the following measurements, and answer the following questions.
; Comment out your responses with semicolons.

; For length, report measurements of 100 repetitions of calling length
; on a list of length k * 100,000 for k = 1,2,3,4.

; For take and drop, report measurements of 100 repetitions of calling take (or drop)
; on a list of length k * 100,000 for k = 1,2,3,4, with the number
; of elements to take (or drop) being half the length of the list.

; You may want to do several measurements because of random variation.

; For the procedures length, take, and drop, replace length-runtime,
; take-runtime, and drop-runtime with either O(1) or O(n) to most accurately
; reflect each procedure's respective running time as a function of the length n
; of the list argument.

; Compare the times taken by the three procedures on comparable inputs -- which is 
; fastest? slowest? Replace length-take-drop-ordering with an ordering of these
; procedures. Then, in the space below, explain *why* on the basis of how lists and
; their operations are implemented. (Complex statistical analysis is not
; necessary.)
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; Please report measurements here.

;; length (seconds): 0.027, 0.029, 0.0417, 0.062
;; take: 0.14, 0.25, 0.41, 0.56
;; drop: 0.007, 0.013, 0.023, 0.036

(define length-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.

(define take-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.

(define drop-runtime "O(n)")
; Please briefly comment below on how your measurements support this conclusion.

; Please do not use commas, and please order from fastest to slowest
; e.g. (define length-reverse-powerset-ordering '(length reverse powerset))
(define length-take-drop-ordering '(drop length take))

; Please explain your ordering here.

;;For a constant list of length m, drop was the fastest, length was next, and take was significantly the slowest

;************************************************************
; We represent a total ordering on a set X of values via a predicate
; (compare? x y), that returns #t or #f.  The results must
; satisfy the following properties for all values x, y, z from the set X:
; (1) if (equal? x y) => #t then (compare? x y) => #t,
;* (1) (if (equal? x y) 
;           (compare? x y) 
;           #t)
; (2) if (and (compare? x y) (compare? y x)) => #t, then (equal? x y) => #t,
;* (2) (if (and (compare? x y) (compare? y x)) 
;           (equal? x y) 
;           #t)
; (3) if (and (compare? x y)(compare? y z)) => #t, then (compare? x z) => #t,
;* (3) (if (and (compare? x y)(compare? y z)) 
;           (compare? x z) 
;           #t)
; (4) (or (compare? x y) (compare? y x)) => #t.
;* (4) (or (compare? x y) (compare? y x))

; If the set X is finite, then we can write a procedure to test
; whether all these properties hold of a proposed total ordering compare? 
; on the set X.  This is what the next problem asks you to do.
; Note that you do NOT need to complete this problem before doing
; the subsequent ones.

;************************************************************
; ** problem 3 ** (10 points)
; Write one procedure

; (total-order? compare? domain)

; that takes a predicate (compare? x y) and a list of values domain
; such that whenever x and y are values from domain, (compare? x y)
; returns either #t or #f.
; The procedure returns #t if compare? is a total order on domain
; (that is, satisfies the four properties above for all x, y, z from domain),
; and #f otherwise.

; Hint: it might be helpful to write a procedure to check these conditions
; one pair x, y at a time.

; QUESTION: What is the running time of your procedure in terms of n,
; the number of elements in the domain.  Assume compare? takes time O(1).
; Give your answer in terms of O, Theta, or Omega, as appropriate and
; explain why it is correct. Replace "replace" in total-order-runtime
; with your answer.

; Examples
;> (total-order? <= '(1 3 5 4))
;#t
;> (total-order? < '(1 3 5 4))
;#f
;> (total-order? >= '(3 2 4 5 1))
;#t
;> (total-order? string<=? (list "hi" "hey" "hello"))
;#t
;> (total-order? equal? (list "hi" "hey" "hello"))
;#f
;************************************************************

(define total-order-runtime "O(n^3)")
; Explain your answer here.

(define (total-order? compare? domain)
  (define rD (rest domain))
  (define break #t)

  (for ([i (length domain)] #:when break)
    (define x (list-ref domain i))
    (for ([j (length domain)])
      (define y (list-ref domain j))

      (if (equal? x y) (set! break (and break (compare? x y))) (set! break (and break #t)))
      (if (and (compare? x y) (compare? y x)) (set! break (and break (equal? x y))) (set! break (and break #t)))
      (set! break (and break (or (compare? x y) (compare? y x))))
      
      (for ([k (length domain)])
        (define z (list-ref domain k))
        (if (and (compare? x y)(compare? y z)) (set! break (and break (compare? x z))) (set! break (and break #t)))
        )
      )
    )
  break
)

;************************************************************

; Now we turn to sorting a list of elements with respect to a given
; comparison operator.  You don't need to have done problem 3 to
; do the following problems.

;************************************************************
; ** problem 4 ** (15 points)
; Write three procedures

; (sorted? compare? lst)
; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For each of these procedures, you may assume that
; compare? is a total order on the elements of lst,
; item and the elements of lst, and the elements of lst1 and lst2,
; respectively.

; (sorted? compare? lst)
; takes a list of items and returns #t or #f
; depending on whether the items of lst are
; sorted with respect to the comparison predicate
; compare?
; In other words, the result should be #f if and only if
; there are two consecutive elements of lst for
; which compare? returns #f.

; (insert compare? item lst)
; inserts an item into a list lst of items
; which is sorted with respect to the compare?
; predicate, so that the resulting list of
; items is also sorted with respect to the
; compare? predicate.

; (merge compare? lst1 lst2)
; takes two lists of elements lst1 and lst2, each of which is sorted with
; respect to the compare? predicate, and produces as its result a list
; of all the items in lst1 and lst2 (preserving duplicates) that is
; sorted with respect to compare?

; Examples
;> (sorted? <= '(1 4 5 8 10))
;#t
;> (sorted? >= '(10 9 4 7 6))
;#f
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangement"))
;'("the" "hello" "best" "arrangement")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (sorted? compare? lst)
  (define sorted #t)
  (define sList (rest lst))
  
  (for ([i (length sList)] #:when sorted)
    (set! sorted (compare? (list-ref lst i) (list-ref sList i)))
    ;(print sorted)
    )
  sorted
  )

(define (insert compare? item lst)
  (define strtPt (if (= (length lst) 0) 0 (random 0 (length lst))) )
  (define insertPt (look compare? item lst strtPt))

  (append (take lst insertPt) (list item) (drop lst insertPt))
  
  )

(define (look compare? item lst point [prev 0])

  (define tion (compare? (list-ref lst point) item))
  
  (if tion
      (cond [(= point (- (length lst) 1)) (+ point 1)]
            [(equal? prev #f) (+ point 1)]
            [else (look compare? item lst (+ point 1) tion)])

      (cond [(= point 0) 0]
            [(equal? prev #t) point]
            [else (look compare? item lst (- point 1) tion)])
      )
  )

(define (merge compare? lst1 lst2)
 (cond [(empty? lst2) lst1]
       [else (merge compare? (insert compare? (first lst2) lst1) (rest lst2))])
)

;************************************************************
; ** problem 5 ** (10 points)
; Write two procedures

; (isort compare? lst)
; (msort compare? lst)

; Each takes a total order comparison predicate compare? and a list
; lst of items, and returns a list of all the elements in lst (duplicates
; preserved) arranged so that they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (merge lst1 lst2) and should
; implement merge sort.

; Examples
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (isort compare? lst)
  (define nList lst)
  
  (cond [(empty? lst) lst]
        [else (for ([n (length lst)] #:unless (sorted? compare? nList))
                (cond [(not (= 0 n))
                       (set! nList (append (insert compare? (first (drop nList n)) (take nList n)) (rest (drop nList n))))]
                )
              )
            ])
  nList
)

(define (msort compare? lst)
  (cond [(empty? lst) lst]
        [(= (length lst) 1) (flatten lst)]
        [else (msort compare? (mergeLst compare? lst))]
        )
  )


(define (mergeLst compare? lst)
  (cond [(empty? lst) '()]
        [(and (= (length lst) 1) (not (list? (first lst)))) (list lst)]
        [(list? (first lst)) (if (= (length lst) 1) lst (append (list (merge compare? (first lst) (second lst))) (mergeLst compare? (drop lst 2)) ))]
        [else (append (list (merge compare? (list (first lst)) (list (second lst)))) (mergeLst compare? (drop lst 2))) ])
  )


;************************************************************
; ** problem 6 ** (20 points)
; (1)(a) Give empirical evidence that your implementation of insertion sort
;        (isort, above) has best case time Omega(n) and worst case time of
;        O(n^2).
;

;   My insertion sort is definitely the correct implementation of an insertion sort as I compare an upcoming element against all the previous elements,
;   which have already been sorted. The new element is inserted into the earlier sorted elements, maintaining order and implementing insertion sort.

;   When doubling the length of the list, the average time it took to calculate the sort quadrupled. This means big O is n^2 and by default big Omega is n.

;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.
    
;; Insertion sort with 50 elements, 1000 iterations: 0.174 s

; (2)(a) Give empirical evidence that your implementation of merge sort
;        (msort, above) has best case and worst case times of Theta(n log n).

;; As the length of the list increases, the time it takes too complete also increases, but not as much as insertion sort on average.
;; At really low values of n, it is slower than insertion but over time it is much faster.

;    (b) Indicate the average case running time and give empirical evidence
;        to support your claim.

; Be sure to use sufficiently long lists of integers and possibly repeat/average
; measurements.

; (3) Please identify inputs that give best and worst cases for your
; implementations of (a) isort and (b) msort. Be sure that you use sufficiently
; long lists of randomly chosen integers in a range larger than the length of
; the list, so that there are unlikely to be many duplicate values.

; best cases for both: already sorted inputs, worst cases for both... reverse sorted inputs 
; worst case for merge sort: every

; (4) Roughly what is the longest list of random integers that your (a) isort
; procedure can sort in 10 seconds?  Same question for your (b) msort procedure?


; Because of memory caching and other effects, the timing behaviors will not
; necessarily be uniform over the whole range of feasible input lengths.
;************************************************************

; Please comment out all parts of your answer that are not a definition. Do NOT delete
; the question prompts below.

; 3a) Please briefly describe best case inputs for your implementation of isort here.

;; Already appropriately sorted shorter lists.

; 3a) Please briefly describe worst case inputs for your implementation of isort here.

;; Reversely sorted longer lists.

; 1b) Please indicate average case running time of your implementation of isort here.

;; Randomly sorted list with 100 elements, 1000 iterations done: 0.000945 s average sort

; 3b) Please briefly describe best case inputs for your implementation of msort here.

;; A list of sorted corresponding pairs of numbers. (e.g. '(1 3 0 2 5 7 9))

; 3b) Please briefly describe worst case inputs for your implementation of msort here.

;; Reversely sorted list of elements

; 2b) Please indicate average case running time of your implementation of msort here.

;; Randomly sorted list with 100 elements, 1000 iterations done: 0.000577 s average sort

; 1a, 2a) Please provide evidence for the above claims here.


;'(testing iSort ---X--- got: 0.009305600002408028 expected: number)
;'(testing iSort ---X--- got: 0.046294799998402594 expected: number)
;'(testing iSort ---X--- got: 0.18362570000439882 expected: number)
;'(testing iSort ---X--- got: 0.9455886999964714 expected: number)
;'(testing iSort ---X--- got: 7.223296400003135 expected: number)
;'(testing mSort ---X--- got: 0.010761700004339219 expected: number)
;'(testing mSort ---X--- got: 0.04205409999936819 expected: number)
;'(testing mSort ---X--- got: 0.14521579999476672 expected: number)
;'(testing mSort ---X--- got: 0.5772126999944448 expected: number)
;'(testing mSort ---X--- got: 2.4922045999988915 expected: number)
;'(testing mSort ---X--- got: 12.599069600000977 expected: number)

; 4a, 4b) Please indicate the longest list of random integers that a) your isort
; procedure can sort in 10 seconds and b) your msort procedure can sort
; in 10 seconds.

;; a. 2500 - 3000 elements
;; b. around 4000 elements


;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (count-compares sort compare? lst)

; that returns the number of calls to the compare? procedure
; when we apply the procedure sort to the arguments compare? and lst.
; Think of sort as a procedure like msort or isort, taking a comparison
; predicate and a list as its arguments, though sort could
; be some other sorting procedure devised for testing purposes.

; The trick here is to take the compare? procedure and "wrap" it
; in another procedure that will count the number of times it
; is called.  Then call sort with the "wrapped" compare? and lst
; as inputs.  Finally, return the final count from the "wrapped"
; compare? as the value from count-compares.

; Please read about the mutator set! to help you keep the count.

; Examples (yours may randomly vary.)
;> (count-compares msort <= (lorint 10 100))
;23
;> (count-compares msort <= (lorint 10 100))
;22
;> (count-compares isort <= (lorint 10 100))
;34
;************************************************************

(define (count-compares sort compare? lst)
  (define count 0)
  (define (proc x y) (and (compare? x y) (set! count (+ 1 count))))
  (define nl (sort proc lst))
  (print nl)
  count
  )
  
;************************************************************
; ** problem 8 ** (10 points)

; In the Runtime lecture notes, we present a stack data structure.

(define (make-stack name (data empty))
  (let ((stack data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? stack))
        ((copy)
         (if (null? args)
             'Error:usage:copy_stack
	     (make-stack (first args) stack)))
        ((show)
	 stack)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_stack
	     (equal? stack ((first args) 'show))))
	
        ((push)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! stack (cons (first args) stack))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? stack)
             'Error:stack-empty
	     (car stack)))
        ((pop)
         (if (null? stack)
             'Error:stack-empty
             (let ((result (car stack)))
               (set! stack (cdr stack))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))
    ))

  
; Write a queue data structure, similar to the stack above.
; Whereas a stack is LIFO (last in first out), a queue is 
; FIFO = first in, first out

; Your queue data structure should implement all the same methods
; as the stack data structure.  However, push is called enqueue,
; and pop is called dequeue.  Here are examples.

; (define q1 (make-queue 'queue1))
; (q1 'name) => 'queue1
; (q1 'empty) => 'invalid-method
; (q1 'show) => '()
; (q1 'enqueue) => 'Error:usage:push_element
; (q1 'enqueue 4) => 4
; (q1 'enqueue 5) => 5
; (q1 'enqueue 6) => 6
; (q1 'peek) => 4
; (q1 'enqueue '(1 2 3)) => '(1 2 3)
; (q1 'size) => 4
; (define q2 (q1 'copy 'queue2))
; (q2 'name) => 'queue2
; (q2 'empty?) => #f
; (q2 'show) => '((1 2 3) 6 5 4)
; (q1 'equal? q2) => #t
; (q2 'equal q1) => 'invalid-method
; (q2 'equal? q1) => #t
; (q1 'equal? q1) => #t
; (q1 'dequeue) => 4
; (q1 'dequeue) => 5
; (q1 'dequeue) => 6
; (q1 'dequeue) => '(1 2 3)
; (q1 'dequeue) => 'Error:queue-empty
; (q1 'size) => 0

(define (make-queue name (data empty))
  (let ((queue data)
        (size (length data)))
    (lambda (cmd . args)
      (case cmd
        ((name) name)
        ((empty?)
         (null? queue))
        ((copy)
         (if (null? args)
             'Error:usage:copy_queue
	     (make-queue (first args) queue)))
        ((show)
	 queue)
        ((equal?)
         (if (null? args)
             'Error:usage:equal_queue
	     (equal? queue ((first args) 'show))))
	
        ((enqueue)
         (if (null? args)
             'Error:usage:push_element
             (begin
               (set! queue (cons (first args) queue))
               (set! size (+ size 1))
               (first args))))
        ((size) size)
	
        ((peek)
         (if (null? queue)
             'Error:queue-empty
	     (last queue)))
        ((dequeue)
         (if (null? queue)
             'Error:queue-empty
             (let ((result (last queue)))
               (set! queue (take queue (- size 1)))
               (set! size (- size 1))
               result)))
	(else 'invalid-method)
        ))
    ))



;; ANSWER THIS QUESTION:
;; What is the Big-O complexity of enqueue, dequeue, size, and peek?
;; O(1)

;************************************************************
; This is where the test code normally appears.
; For this assignment, write your own tests.  

(define *testing-flag* #t)
(define error display)  ;; turn off error messages

(define (test name got expected)
  (cond (*testing-flag*
	 (let* ((expected (if (procedure? expected)
			      (and (expected got) 'OK-TEST)
			      expected))
		(prefix (if (equal? got expected)
			    '***OK***
			    '---X---)))
	   (list 'testing name prefix 'got: got 'expected: expected)))))

(test 'time-calls (time-calls 10000 + (list 13 14)) '())

(test 'time-calls (time-calls 10000 length (list '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))) 'number)

(define list1 (build-list 100000 (lambda (x) x)))
(define list2 (build-list 200000 (lambda (x) x)))
(define list3 (build-list 300000 (lambda (x) x)))
(define list4 (build-list 400000 (lambda (x) x)))
(define list8 (build-list 800000 (lambda (x) x)))

(define listnum1 (build-list 25 (lambda (x) (random))))
(define listnum2 (build-list 50 (lambda (x) (random))))
(define listnum3 (build-list 100 (lambda (x) (random))))
(define listnum0 (build-list 10 (lambda (x) (random))))
(define listnum4 (build-list 200 (lambda (x) (random))))
(define listnum5 (build-list 400 (lambda (x) (random))))

(define listnumcrazy (build-list 2500 (lambda (x) (random))))
(define listnumcrazy2 (build-list 4000 (lambda (x) (random))))

(test 'time-callsR (time-calls 100 length (list list1)) 'number)
(test 'time-callsR (time-calls 100 length (list list2)) 'number)
(test 'time-callsR (time-calls 100 length (list list3)) 'number)
(test 'time-callsR (time-calls 100 length (list list4)) 'number)
(test 'time-callsR (time-calls 100 length (list list8)) 'number)

(test 'takeR (time-calls 100 take (list list1 50000)) 'number)
(test 'takeR (time-calls 100 take (list list2 100000)) 'number)
(test 'takeR (time-calls 100 take (list list3 150000)) 'number)
(test 'takeR (time-calls 100 take (list list4 200000)) 'number)

(test 'dropR (time-calls 100 drop (list list1 50000)) 'number)
(test 'dropR (time-calls 100 drop (list list2 100000)) 'number)
(test 'dropR (time-calls 100 drop (list list3 150000)) 'number)
(test 'dropR (time-calls 100 drop (list list4 200000)) 'number)

(test 'iSort (time-calls 1000 isort (list < listnum0)) 'number)
(test 'iSort (time-calls 1000 isort (list < listnum1)) 'number)
(test 'iSort (time-calls 1000 isort (list < listnum2)) 'number)
(test 'iSort (time-calls 1000 isort (list < listnum3)) 'number)
;;(test 'iSort (time-calls 1 isort (list < listnumcrazy)) 'number)

(test 'mSort (time-calls 1000 msort (list < listnum0)) 'number)
(test 'mSort (time-calls 1000 msort (list < listnum1)) 'number)
(test 'mSort (time-calls 1000 msort (list < listnum2)) 'number)
(test 'mSort (time-calls 1000 msort (list < listnum3)) 'number)
;;(test 'mSort (time-calls 1000 msort (list < listnum4)) 'number)
;;(test 'mSort (time-calls 1000 msort (list < listnum5)) 'number)
;;(test 'mSort (time-calls 1 msort (list < listnumcrazy2)) 'number)

;********* end of hw8, end of hws! **************************