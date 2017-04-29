#lang racket

(provide 
 random-list time-many-calls
 tr-length ntr-length
 insert merge
 divide-list isort msort
 visort
 count-compares)

; Please do not change lines above this one.

;************************************************************
; CS 201a HW #7  DUE  Wednesday, April 26, 11:59 pm 
; via the submit system on the Zoo.  Without a "Temporary Incomplete"
; from you residential college dean, no homework can be accepted
; after the last day of Reading Period, that is, after May 4.
;************************************************************
; Name: Rishab Ramanathan
; Email address: rishab.ramanathan@yale.edu
;************************************************************

; Computer science topics: running times of programs, insertion sort,
; merge sort.  Racket topics: vectors.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

;************************************************************

; Timing procedures.
; The Racket procedure (current-inexact-milliseconds)
; returns a real (in the sense of Racket) number representing
; the current time in milliseconds since midnight UTC, January 1, 1970.

;************************************************************
; ** problem 1 ** (15 points)

; Write two procedures

; (random-list n)
; (time-many-calls reps proc args)

; (random-list n) takes a non-negative number n
; and returns a list of length n of randomly chosen 
; real numbers between 0 and 1.

; Note that (random) returns a random number between 0 and 1.

; (time-many-calls reps proc args) takes 
; a nonnegative integer, reps,
; a procedure, proc,
; a list of arguments for the procedure, args,
; and returns the amount of time in SECONDS elapsed
; in calling proc on args a number of times equal to reps.

; Recall that we can apply a proc to a list of argument args
; with (apply proc args).
; Note that a millisecond is 1/1000 of a second.

; Example of random-list
;> (random-list 10)
;'(0.8339052213011976
;  0.28715053124523504
;  0.3824988826084341
;  0.6922657643424531
;  0.4919157755371373
;  0.6921387871179888
;  0.24176785286695546
;  0.015492810221040746
;  0.22382546601716827
;  0.7667863239747369)

; The following examples of time-many-calls were run on a Zoo
; workstation, calling the built-in compare procedure (<=)
; a certain number of times on the arguments 13 and 14.
; The times for 10^4, 10^5, 10^6 and 10^7 repetitions
; were about 0.0003 seconds, 0.0057 seconds, 0.0609 seconds, and
; 0.5600 seconds, respectively.

; Except in the first case, multiplying the number of repetitions
; by 10 approximately multiplies the measured time by 10,
; so in the range of repetitions between 10^5 and 10^7,
; constant time (time Theta(1)) seems like a reasonable model
; of the time for the operation (<= 13 14).

;> (time-many-calls 1000 <= '(13 14))
;4.1015625e-05
;> (time-many-calls 10000 <= '(13 14))
;0.000259033203125
;> (time-many-calls 1e5 <= '(13 14))
;0.00566796875
;> (time-many-calls 1e6 <= '(13 14))
;0.06086083984375
;> (time-many-calls 1e7 <= '(13 14))
;0.55958984375

; This second set of timings illustrates random variation in
; these measurements on the same Zoo node.  Timings on your
; laptop may be noticeably different.

;> (time-many-calls 1000 <= '(13 14))
;5.419921875e-05
;> (time-many-calls 10000 <= '(13 14))
;0.0002880859375
;> (time-many-calls 1e5 <= '(13 14))
;0.005796875
;> (time-many-calls 1e6 <= '(13 14))
;0.055732177734375
;> (time-many-calls 1e7 <= '(13 14))
;0.5537509765625

; The following examples show timings (on a Zoo node)
; for creating lists of 100,000 or 200,000 or 300,000
; random numbers.
; Less than a tenth of a second suffices in the last case.

;> (time-many-calls 1 random-list '(100000))
;0.014049072265625
;> (time-many-calls 1 random-list '(200000))
;0.02800390625
;> (time-many-calls 1 random-list '(300000))
;0.06631005859375

;******************************(******************************

(define (random-list n)
  (cond
    [(equal? n 0) '()]
    [else (cons (random) (random-list (- n 1)))]))

(define (time-calls-helper reps proc args start val)
  (cond
    [(equal? reps 0) (/ (- (current-inexact-milliseconds) start) 1000)]
    [(equal? reps 0.0) (/ (- (current-inexact-milliseconds) start) 1000)]
    [else (time-calls-helper (- reps 1) proc args start (apply proc args))]))

(define (repeater proc args count)
  (for ((i (in-range count))) (apply proc args)))

(define (time-many-calls reps proc args)
  (let ((now (current-inexact-milliseconds)))
    (for ((i (in-range reps))) (apply proc args))
    (/ (- (current-inexact-milliseconds) now) 1000)))

;(define (time-many-calls reps proc args)
;  (time-calls-helper reps proc args (current-inexact-milliseconds) 0))
  
;************************************************************
; ** problem 2 ** (15 points)
; For this problem, you will use your procedures
; random-list and time-many-calls
; to time the built-in Racket procedures:

; length, vector-ref, list-ref

; reporting the requested measurements, and answering the related questions.

; *Comment out* your responses with semicolons.
; Also, please *comment out* with semicolons the creation of
; long vectors and lists, or your procedures may fail the timeouts
; and lose credit!

; For length, create lists of lengths k*100,000 for k = 1,2,3,4 
; and report measurements of 1000 repetitions of calling length
; on these lists.

; QUESTION: what can you say about how the time for (length lst)
; grows as a function of the length of lst?

; For vector-ref and list-ref, create vectors and lists of
; lengths 100,000 and 200,000 elements, and for each,
; measure the time for 1000 repetitions of accessing the elements
; at the beginning, 25%, 50% and 75% of the length, and at the end.

; QUESTION: what can you say about how the times for 
; (vector-ref v index) and (list-ref lst index) 
; depend on the length of the structure and the index accessed?
;************************************************************


;************************************************************
; ** problem 3 ** (10 points)
; Write two procedures

; (tr-length lst)
; (ntr-length lst)

; that each take a list lst and return its length.
; tr-length should be implemented using tail recursion
; ntr-length should be implemented recursively but without
;            using tail recursion.

; QUESTION: Compare the time taken by the built-in length procedure
; with the time taken by your tr-length and ntr-length procedures.
; Comment out with semicolons your data and conclusions.
;************************************************************

(define (tr-length lst)
  "not done yet")
  
(define (ntr-length lst)
  "not done yet")

;************************************************************

; Now we turn to sorting a list of elements with respect 
; to a given comparison operator.

;************************************************************
; ** problem 4 ** (10 points)
; Write two procedures

; (insert compare? item lst)
; (merge compare? lst1 lst2)

; For these procedures, compare? is a comparison procedure
; for a total ordering of the values item and the elements of lst,
; or the elements of lst1 and lst2.  For example, for lists of
; numbers, compare? might be <= or >=.  The procedure compare?
; takes two arguments and returns #t or #f.

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
;> (insert <= 3 '(1 2 4 5))
;'(1 2 3 4 5)
;> (insert string>=? "hello" (list "the" "best" "arrangment"))
;'("the" "hello" "best" "arrangment")
;> (merge >= '(10 7 4 2 1) '(22 9 5))
;'(22 10 9 7 5 4 2 1)
;> (merge string<=? (list "a" "novel" "thought") (list "more" "predictive"))
;'("a" "more" "novel" "predictive" "thought")
;************************************************************

(define (insert compare? item lst)
  "not done yet")
  
(define (merge compare? lst1 lst2)
  "not done yet")
  
;************************************************************
; ** problem 5 ** (15 points)
; Write three procedures

; (divide-list lst)
; (isort compare? lst)
; (msort compare? lst)

; (divide-list lst) returns a list of two lists,
; the first of which consists of the first half of the elements of lst and
; the second of which consists of the second half of the elements of lst.
; If lst has an odd number of elements, then the first of the returned
; lists should have one more element than the second.
; This procedure should run in time that is (in principle) proportional
; to the length of lst.  (Please recall what you learned about
; the running time of length in previous problems.)

; The procedures isort and msort take a total order comparison predicate 
; compare? and a list lst of items, and returns a list of all 
; the elements in lst (duplicates preserved) arranged so that 
; they are sorted with respect to compare?

; (isort compare? lst) should use (insert compare? item lst) and
; should implement insertion sort.

; (msort compare? lst) should use (divide-list lst) and (merge lst1 lst2) 
; and should implement merge sort.

; Examples
; (divide-list '(a b c d e)) => '((a b c) (d e))
; (divide-list '(12 3 6 4 9 3 2)) => '((12 3 6 4) (9 3 2))
;> (isort string<=? (list "predictive" "novel" "a" "more" "thought"))
;'("a" "more" "novel" "predictive" "thought")
;> (msort string>=? (list "predictive" "novel" "a" "more" "thought"))
;'("thought" "predictive" "novel" "more" "a")
;************************************************************

(define (divide-list lst)
  "not done yet")

(define (isort compare? lst)
  "not done yet")
  
(define (msort compare? lst)
  "not done yet")

;************************************************************
; ** problem 6 ** (15 points)

; QUESTION: By using sufficiently long lists of numbers (integer or real)
; and the comparison operation <=,
; possibly repeating and averaging measurements, 
; give empirical evidence for the claims that:
; (1) your implementation of insertion sort (isort, above) has best
; case time Theta(n) and worst case time of Theta(n^2).
; (2) your implementation of merge sort (msort, above) has best case and
; worst case times of Theta(n log n).

; QUESTION: Describe the inputs that give best and worst cases for your
; implementations of isort and msort.

; QUESTION: Roughly what is the longest list of random numbers that your isort
; procedure can sort in 10 seconds?  Same question for your msort procedure?

; Because of memory caching and other effects, the timing behaviors will not
; necessarily uniform over the whole range of feasible input lengths.
;************************************************************


;************************************************************
; ** problem 7 ** (10 points)
; Write one procedure

; (visort vec)

; that takes as input a vector vec of numbers and sorts them
; into increasing order using the algorithm described in
; pseudo-code in the Wikipedia article on insertion sort
; (either 2014 version, given in the lecture notes for 2014,
; or current 2017 version at Wikipedia).  The algorithm
; needs to be slightly corrected, of course.

; Use vector-ref, vector-set! to sort "in place" in vec.

; QUESTION: How does the time for visort compare with the times
; for isort and msort to sort the same inputs (as a vector or list).
;************************************************************

(define (visort vec)
  "not done yet")
  
;************************************************************
; ** problem 8 ** (10 points)
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
;> (count-compares msort <= (random-list 10))
;23
;> (count-compares msort <= (random-list 10))
;22
;> (count-compares isort <= (random-list 10))
;34
;************************************************************

(define (count-compares sort compare? lst)
  "not done yet")

;************************************************************
;********* end of hw7, end of hws! **************************



