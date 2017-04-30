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

(define (time-many-calls reps proc args)
  (let ((now (current-inexact-milliseconds)))
    (for ((i (in-range reps))) (apply proc args))
    (/ (- (current-inexact-milliseconds) now) 1000)))
  
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

;NOTE: Repeated test multiple times for each to make sure data was reliable.

;length:
;Time taken for (length lst) grows proportional to n: it has runtime complexity of O(n).
;Measurements;
;k = 1: 0.25273095703125
;k = 2: 0.535097900390625
;k = 3: 0.8512548828125
;k = 4: 1.16872705078125

;vector-ref:
;Time taken for (vector-ref v index) stays constant (constant time): it has runtime complexity of O(1).
;Measurements;
;0%: 6.9091796875e-05
;25%: 6.884765625e-05
;50%: 6.8115234375e-05
;75%: 6.787109375e-05
;100%: 6.591796875e-05

;list-ref:
;Time taken for (list-ref lst index) grows proportional to n: it has runtime complexity of O(n).
;Measurements;
;0%: 6.9091796875e-05
;25%: 0.08693896484375
;50%: 0.1795400390625
;75%: 0.27877001953125
;100%: 0.400529052734375

; (vector-ref v index) uses vectors. A vector is a fixed-length array with constant-time access and
; update of the vector slots, which are numbered from 0 to one less than the number of slots in the vector. Therefore,
; even when the length of the vector increases, it still takes only one step to access any element in the vector. This
; is why it runs in constant time.
; (list-ref lst index) uses lists. A list can be used as a single-valued sequence.
; The elements of the list serve as elements of the sequence. Therefore, when accessing
; an element at a specific index, the function must traverse through the list starting
; from the beginning. This is why the function runs with time proportional to n. It
; could, in the worst case, need to access every element to arrive at the desired element (the last one).


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

(define (len1 lis sofar)
 (if
  (null? lis) sofar
  (len1 (cdr lis) (+ sofar 1))))

(define (tr-length lst)
  (len1 lst 0))
  
(define (ntr-length lst)
  (cond
    [(empty? lst) 0]
    [else (+ 1 (ntr-length (rest lst)))]))

; The time taken for the non-tail recursive function are as follows:
; > (time-many-calls 1000 ntr-length (list (random-list 100)))
; 0.005880126953125
; > (time-many-calls 1000 ntr-length (list (random-list 1000)))
; 0.057385986328125
; > (time-many-calls 1000 ntr-length (list (random-list 10000)))
; 0.582383056640625

; The time taken for the tail recursive function are as follows:
; > (time-many-calls 1000 tr-length (list (random-list 100)))
; 0.002474853515625
; > (time-many-calls 1000 tr-length (list (random-list 1000)))
; 0.027369140625
; > (time-many-calls 1000 tr-length (list (random-list 10000)))
; 0.2612509765625

; Conclusion: Both tail recursive and non-tail recursive length functions run with time complexity O(n).
; The tail recursive function just runs twice as fast as the non-tail recursive function because it doesn't
; need to return to the previous call. 

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
  (cond
    [(null? lst) (list item)]
    [(compare? item (first lst)) (cons item lst)]
    [else (cons (first lst) (insert compare? item (rest lst)))]))
  
(define (merge compare? lst1 lst2)
  (cond
    [(null? lst1) lst2]
    [(null? lst2) lst1]
    [else
     (if (compare? (first lst1) (first lst2))
         (cons (first lst1) (merge compare? (rest lst1) lst2))
         (cons (first lst2) (merge compare? (rest lst2) lst1)))]))
  
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
  (let ((len (length lst)))
    (cond
      [(even? len) (list (take lst (/ len 2)) (drop lst (/ len 2)))]
      [else (list (take lst (/ (+ (length lst) 1) 2)) (drop lst (/ (+ (length lst) 1) 2)))])))

(define (isort compare? lst)
  (cond
    [(null? lst) '()]
    [else
     (insert compare? (first lst)
             (isort compare? (rest lst)))]))
  
(define (msort compare? lst)
  (cond
    [(null? lst) lst]
    [(null? (rest lst)) lst]
    [else
     (let ((div-lst (divide-list lst)))
       (merge compare?
            (msort compare? (first div-lst))
            (msort compare? (second div-lst))))]))

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

;NOTE: Repeated test multiple times and took their average for each sort to make sure data was reliable.

;insertion sort - BEST CASE:
;Time complexity for the insertion sort in the best case is Theta(n), based on data observed below.
;> (time-many-calls 1000 isort (list <= (build-list 10 values)))
;0.001154052734375
;> (time-many-calls 1000 isort (list <= (build-list 100 values)))
;0.0119638671875
;> (time-many-calls 1000 isort (list <= (build-list 1000 values)))
;0.15160302734375
;> (time-many-calls 1000 isort (list <= (build-list 10000 values)))
;1.704490966796875
; the time taken here clearly grows proportional to n. The list is already sorted, so it only needs to access each element once.

;insertion sort - WORST CASE:
;Time complexity for the insertion sort in the worst case is Theta(n^2), based on data observed below.
;> (time-many-calls 1000 isort (list <= (reverse (build-list 10 values))))
;0.005881103515625
;> (time-many-calls 1000 isort (list <= (reverse (build-list 20 values))))
;0.032839111328125
;> (time-many-calls 1000 isort (list <= (reverse (build-list 40 values))))
;0.119636962890625
;> (time-many-calls 1000 isort (list <= (build-list 80 values)))
;0.43073291015625
; the time taken here clearly grows proportional to n^2. The list is sorted in the opposite direction.
; The data shows that every time the list is expanded by a factor of 2, the time taken increases by a factor of 2^2 = 4.
; This proves the time complexity is Theta(n^2)

;merge sort - BEST CASE:
;Time complexity for the merge sort in the best case is Theta(n log n), based on data observed below.
;> (time-many-calls 100 msort (list <= (build-list 10 values)))
;0.000552978515625
;> (time-many-calls 100 msort (list <= (build-list 100 values)))
;0.00889208984375
;> (time-many-calls 100 msort (list <= (build-list 1000 values)))
;0.1450859375
; the time taken here clearly grows proportional to n log n. Every time the list is expanded by a factor of 10,
; the time taken increases by 10 * (log n / log (n/10)). Going from list size 10 to 100, this factor is roughly ~20,
; which is what is seen.

;merge sort - WORST CASE:
;Time complexity for the merge sort in the worst case is Theta(n log n), based on data observed below.
;> (time-many-calls 100 msort (list <= (reverse (build-list 10 values))))
;0.000507080078125
;> (time-many-calls 100 msort (list <= (reverse (build-list 100 values))))
;0.008720947265625
;> (time-many-calls 100 msort (list <= (reverse (build-list 1000 values))))
;0.142096923828125
; the time taken here clearly grows proportional to n log n. Same empirical reasoning as given for best case.

;INPUTS FOR ISORT & MSORT:
; best case -> the input is a list that is sorted with the <= comparator already. 
; worst case -> the input is a list that is sorted with the >= comparator.

;LONGEST INPUT FOR ISORT & MSORT TO PROCESS IN 10 SECS:
; I am assuming by "longest list", you mean the best case list of numbers, because this is what the "longest list" would be.
; isort -> ~ (10,000/1.7044)*1,000 ~ 5,867,167. It took 1.7044 seconds to sort a list of 10,000 numbers with 1,000 repetitions.
; On testing, however, this number was closer to ~2,000,000. 
; msort -> ~ 1,700,000. Since msort always runs in Theta(n log n) time, and one repetition of list size 10,000 resulted in 0.0379
; seconds, n for 10 seconds turned out to be around 1,700,000. On testing this, it proved to be correct.


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

; for testing purposes
;(define test (make-vector 5))
;(vector-set! test 1 3)
;(vector-set! test 3 7)
;(vector-set! test 4 1)

(define (while-repl vec x j)
  (cond
    [(and (> j 0) (> (vector-ref vec (- j 1)) x))
     (vector-set! vec j (vector-ref vec (- j 1)))
     (while-repl vec x
                 (- j 1))]
    [else (list vec j)]))

(define (visort vec)
  (let ((len (vector-length vec)))
    (for ((i (in-range len)))
      (let ((x (vector-ref vec i))
            (j i))
        (let ((new-v (while-repl vec x j))) 
          (set! vec (first new-v))
          (vector-set! vec (second new-v) x)))))
  vec)

;Time comparison -
;BEST CASE:
;> (time-many-calls 100 visort (list (list->vector (build-list 10 values))))
;0.00018896484375, merge sort: 0.000552978515625, insertion sort: 0.00012890625
;> (time-many-calls 100 visort (list (list->vector (build-list 100 values))))
;0.001677001953125, merge sort: 0.01019384765625, insertion sort: 0.001738037109375
;> (time-many-calls 100 visort (list (list->vector (build-list 1000 values))))
;0.03047802734375, merge sort: 0.15077197265625, insertion sort: 0.015029052734375

;WORST CASE:
;> (time-many-calls 100 visort (list (list->vector (reverse (build-list 10 values)))))
;0.000191162109375, merge sort: 0.000507080078125, insertion sort: 0.000544189453125
;> (time-many-calls 100 visort (list (list->vector (reverse (build-list 100 values)))))
;0.001927001953125, merge sort: 0.008720947265625, insertion sort: 0.079635009765625
;> (time-many-calls 100 visort (list (list->vector (reverse (build-list 1000 values)))))
;0.047733154296875, merge sort: 0.142096923828125, insertion sort: 7.8504189453125

;From the data shown above, we can see that the visort is much more efficient when scaling than
;either of the other two sort functions. In both best case and worst case, time taken for visort
;grows proportional to n. It has runtime complexity Theta(n) for both best and worst.
  
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

;to keep track of count and comparer type
(define count 0)
(define comparer <=)

;compare? wrapper
(define (comp-wrapper i1 i2)
  (set! count (+ count 1))
  (comparer i1 i2))

(define (count-compares sort compare? lst)
  (set! comparer compare?)
  (sort comp-wrapper lst)
  (let ((n count))
    (set! count 0)
    n))

;************************************************************
;********* end of hw7, end of hws! **************************

