#lang racket

(provide 
         entry entry? entry-key entry-value
         e-not e-not? e-not-arg
         e-and e-and? e-and-args
         e-or e-or? e-or-args
         tt tt-vars tt-rows tt?
	 hours
         lookup
         f-not f-and f-or
         boolean-exp?
	 all-vars
	 eval-in-env
	 all-keys
	 truth-table	 	 	 
         classify equivalent?
	 find-exp
         simplify)

; Please do not modify lines above this one.

; ****************************************************************
; CS 201 HW #4  DUE 11:59 pm Wednesday, March 1, 2017
; using the submit command on the Zoo.
; ****************************************************************
; Name: Rishab Ramanathan
; Email address: rishab.ramanathan@yale.edu
; ****************************************************************
; ** problem 0 ** (1 easy point)
; Please modify the following definition to reflect the number of
; hours you spent on this assignment.  (Must be non-zero to earn credit.)

(define hours 5)

; ****************************************************************
; Please DO NOT use require or mutators (set! and its relatives)
; in your programs.  Otherwise you may use any Racket constructs.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problems.  Please include
; a comment for each one explaining its input and results.

; Topics:
; Racket: deep recursion on a recursively defined data structure.
; Computer Science: Boolean functions, expressions, environments,
; truth tables, logical equivalence, tautology, contradiction,
; contingency, simplification.

; ****************************************************************
; We define a table as a list of entries,
; where each entry is given by the following structure.

(struct entry (key value) #:transparent)

; Recall that a struct defines a constructor, selector(s), and a type predicate.
; In this case, the constructor is entry, the selectors are
; entry-key and entry-value, and the type predicate is entry?.

; Here are two examples of tables.

(define table1
  (list
   (entry "second" 2)
   (entry "first" 1)
   (entry "fifth" 5)))

(define table2
  (list
   (entry 'x 0)
   (entry 'z 1)
   (entry 'y 1)
   (entry 'z 0)))

; ****************************************************************
; ** problem 1 ** (5 points)
; Write a procedure to deal with tables as follows.

; (lookup key table)

; returns #f if no entry in the table has a key equal? to key
; otherwise, returns the value of the first entry whose key is equal? to key.

; Examples
; (lookup "first" table1) => 1
; (lookup "third" table1) => #f
; (lookup 'z table2) => 1
; ****************************************************************

(define (lookup key table)
  (cond
    [(empty? table) #f]
    [else (if (equal? (entry-key (first table)) key) (entry-value (first table)) (lookup key (rest table)))]
    ))

; ****************************************************************
; ** problem 2 ** (5 points)
; Write three procedures to compute Boolean functions as follows.

; (f-not val)
; (f-and lst)
; (f-or lst)

; (f-not val) takes a single Boolean value represented by 0 or 1
; and returns the negation (NOT) of it.

; (f-and lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the conjunction (AND) of them all.  If lst is empty, then
; the value returned should be 1.

; (f-or lst) takes a list lst of Boolean values represented by 0 or 1
; and returns the disjunction (OR) of them all.  If lst is empty, then
; the value returned should be 0.

; Examples
; (f-not 0) => 1
; (f-and '()) => 1
; (f-and '(1 1 0 1)) => 0
; (f-or '()) => 0
; (f-or '(1)) => 1
; ****************************************************************

(define (f-not val)
  (if (= val 1) 0 1))

(define (f-and lst)
  (cond
    [(empty? lst) 1]
    [else
     (if (= 0 (first lst)) 0 (f-and (rest lst)))]))

(define (f-or lst)
  (cond
    [(empty? lst) 0]
    [else
     (if (= 1 (first lst)) 1 (f-or (rest lst)))]))

; ****************************************************************
; Our representation of Boolean expressions will use the following
; struct definitions, for the operations of NOT, AND, OR.

(struct e-not (arg) #:transparent)
(struct e-and (args) #:transparent)
(struct e-or (args) #:transparent)

; These define constructors, selectors, and type predicates as follows
; e-not, e-not-arg, e-not?
; e-and, e-and-args, e-and?
; e-or, e-or-args, e-or?

; A Boolean expression is defined recursively as follows.
; 1) the constants 0 and 1 are Boolean expressions
; 2) any identifier is a Boolean expression, where the variable x
; is represented by the symbol 'x
; 3) if <E> is any Boolean expression, its negation (NOT) is represented
; by (e-not <E>).
; 4) if <E1>, ..., <En> are any Boolean expressions, their conjunction (AND)
; is represented by (e-and (list <E1> ... <En>)).  If the list is empty,
; then the AND expression is equivalent to the constant 1.
; 5) if <E1>, ..., <En> are any Boolean expressions, their disjunction (OR)
; is represented by (e-or (list <E1> ... <En>)).  If the list is empty,
; then the OR expression is equivalent to the constant 0.

; Some examples of Boolean expressions:

; The expression 0'
(define exp1 (e-not 0))

; The expression (x + y)
(define exp2 (e-or (list 'x 'y)))

; The expression (x * y * z)
(define exp3 (e-and (list 'x 'y 'z)))

; The expression (w * (x' + 0 + y)))
(define exp4 (e-and (list 'w (e-or (list (e-not 'x) 0 'y)))))

; The expression (x + x')
(define exp5 (e-or (list 'x (e-not 'x))))

; ****************************************************************
; ** problem 3 ** (10 points)
; Write a procedure

; (boolean-exp? val)

; (boolean-exp? val) takes an arbitrary Racket value val
; and tests to see whether it is a Boolean expression according
; to the definition above, returning #t if so and #f if not.

; The predicate (symbol? arg) will test whether its argument
; is an identifier.

; Hint: deep recursion on the structure of Boolean expressions.

; Examples
; (boolean-exp? 0) => #t
; (boolean-exp? 2) => #f
; (boolean-exp? exp1) => #t
; (boolean-exp? (e-and (list 'x "hi!"))) => #f
; (boolean-exp? (e-and (list 'x 0 'y))) => #t
; (boolean-exp? (e-or (list 'x (e-and (list 'y #t))))) => #f
; ****************************************************************

(define (boolean-exp? val)
  (cond
    [(e-not? val) (boolean-exp? (e-not-arg val))]
    [(e-or? val) (boolean-exp? (e-or-args val))]
    [(e-and? val) (boolean-exp? (e-and-args val))]
    [(equal? 1 val) #t]
    [(equal? 0 val) #t]
    [(symbol? val) #t]
    [(list? val) (if (empty? val) #t (if (boolean-exp? (first val)) (boolean-exp? (rest val)) #f))]
    [else #f]))
  
; ****************************************************************
; ** problem 4 ** (10 points)
; Write a procedure

; (all-vars bexp)

; that takes a Boolean expression bexp 
; and makes a list containing all the variables
; that occur in bexp.  The list should not contain duplicates,
; and should have the variables in the order of their
; first appearance in bexp (scanning left to right.)

; Hint: deep recursion on the structure of Boolean expressions.

; Note that there is a Racket procedure: remove-duplicates

; Examples

; (all-vars 0) =>'()
; (all-vars 'x) => '(x)
; (all-vars (e-not (e-and (list 'x (e-or (list 'y (e-not 'z) 'x)))))) => '(x y z)
; (all-vars (e-and (list 1 (e-or (list 0 (e-not 'u)))))) => '(u)
; (all-vars (e-and (list (e-or (list 'x 'y)) (e-or (list (e-not 'y) (e-not 'x)))))) => '(x y)
; (all-vars (e-or (list 'c 'b 'a (e-and (list 'a 'b 'c))))) => '(c b a)
;*************************************************

(define (all-vars bexp)
    (cond
    [(e-not? bexp) (all-vars (e-not-arg bexp))]
    [(e-or? bexp) (all-vars (e-or-args bexp))]
    [(e-and? bexp) (all-vars (e-and-args bexp))]
    [(symbol? bexp) (cons bexp '())]
    [(list? bexp) (if (empty? bexp) '() (remove-duplicates (append (all-vars (first bexp)) (all-vars (rest bexp)))))]
    [else '()]))

; ****************************************************************
; We represent an environment as table each entry of which
; has a key that is a Racket symbol and a value that is 0 or 1,
; which specifies the truth value of that variable in the environment.
; For example:

(define environ1
  (list
   (entry 'x 0) (entry 'y 1) (entry 'z 0)))
  
(define environ2
  (list
   (entry 'u 0) (entry 'x 1) (entry 'w 1) (entry 'y 0) (entry 'z 1)))

; ****************************************************************
; ** problem 5 ** (10 points)
; Write a procedure 

; (eval-in-env bexp env)

; that takes a Boolean expression bexp and an environment env
; (represented as described above) and returns 0 or 1 giving 
; the value of the expression in the environment.

; If the Boolean expression contains variables that do not
; occur in the environment, (eval-in-env bexp env) should
; return the string: "variable unspecified in environment".
; (You may want to check for this condition first.)

; Hint: deep recursion on the structure of Boolean expressions.

; Examples

; (eval-in-env 1 environ1) => 1
; (eval-in-env (e-or (list 0 0 0)) '()) => 0
; (eval-in-env 'x environ1) => 0
; (eval-in-env 'x environ2) => 1
; (eval-in-env (e-not 'z) environ1) => 1
; (eval-in-env (e-or (list 'y (e-not 'x))) environ2) => 0
; (eval-in-env (e-and (list (e-or (list 'y 'x)) (e-or (list 'w 'y)))) environ2) => 1
; (eval-in-env exp5 environ1) => 1
; (eval-in-env (e-and (list 'x 'y 'z)) (list (entry 'x 1) (entry 'z 0))) => "variable unspecified in environment"
; ****************************************************************

(define (check-vars vars env)
    (cond
      [(empty? vars) #t]
      [else (if (equal? #f (lookup (first vars) env)) #f
                (check-vars (rest vars) env))]))

(define (eval-in-env bexp env)
    (if (boolean-exp? bexp) (if (check-vars (all-vars bexp) env)
        (cond
          [(symbol? bexp) (lookup bexp env)]
          [(equal? 0 bexp) 0]
          [(equal? 1 bexp) 1]
          [(e-not? bexp) (f-not (eval-in-env (e-not-arg bexp) env))]
          [(e-or? bexp) (f-or (eval-in-env (e-or-args bexp) env))]
          [(e-and? bexp) (f-and (eval-in-env (e-and-args bexp) env))]
          [(list? bexp) (if (empty? bexp) '() (cons (eval-in-env (first bexp) env)
                                                    (eval-in-env (rest bexp) env)))]
          [else "invalid"])
        "variable unspecified in environment") "invalid"))

; ****************************************************************
; We define a truth table as represented by the following struct

(struct tt (vars rows) #:transparent)

; whose fields contain the following
; (1) a (possibly empty) list of n distinct variables, and
; (2) a table containing an entry for each row of the truth table:
; the key of an entry is a list of n 0's and 1's, and the value is the
; Boolean value (0 or 1) of the function on that row of the table.

; Note that the entries in a truth table should be in increasing order of
; their keys, considered as binary numbers.

; Examples of truth tables for the functions given by
; x', (x * y), (a NAND b), (u XOR v)

(define tt-not (tt '(x)
                   (list
                    (entry '(0) 1)
                    (entry '(1) 0))))

(define tt-and (tt '(x y)
                   (list 
                    (entry '(0 0) 0)
                    (entry '(0 1) 0)
                    (entry '(1 0) 0)
                    (entry '(1 1) 1))))
                    
 (define tt-nand (tt '(a b)
                   (list
                    (entry '(0 0) 1)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))
  
(define tt-xor (tt '(u v)
                   (list
                    (entry '(0 0) 0)
                    (entry '(0 1) 1)
                    (entry '(1 0) 1)
                    (entry '(1 1) 0))))

; Here is a truth table for a function of three arguments a, b, c.

(define tt-f1 (tt '(a b c)
                  (list
                   (entry '(0 0 0) 0)
                   (entry '(0 0 1) 0)
                   (entry '(0 1 0) 1)
                   (entry '(0 1 1) 1)
                   (entry '(1 0 0) 0)
                   (entry '(1 0 1) 1)
                   (entry '(1 1 0) 0)
                   (entry '(1 1 1) 1))))

; ****************************************************************
; ** problem 6 ** (10 points)
; Write a procedure 

; (all-keys n)

; that takes a non-negative integer n and creates the list of all 
; lists of n 0's or 1's in the *specific order* required for
; a truth table.  In other words, the lists, interpreted as
; binary numbers, should be in increasing order.

; Hint: if a recursive call gives the correct answer
; for (all-keys 2), what needs to happen to it
; to give the correct answer for (all-keys 3)?
; (Compare bit-strings from lecture and all-subsets from hw #2.)

; Use let or let* to avoid recomputing the recursive call!

; Examples
; (all-keys 0) => '(())
; (all-keys 1) => '((0) (1))
; (all-keys 2) => '((0 0) (0 1) (1 0) (1 1))
; (all-keys 3) => '((0 0 0) (0 0 1) (0 1 0) (0 1 1) (1 0 0) (1 0 1) (1 1 0) (1 1 1))
; ****************************************************************

(define (put-first x lsts)
	    (map (lambda (lst)
	           (cons x lst))
                 lsts))

(define (all-keys n)
    (cond
      [(= n 0) '(())]
      [(= n 1)
       '((0) (1))]
      [else (let ((previous (all-keys (- n 1))))
              (append
               (put-first 0 previous)
               (put-first 1 previous)))]))

; ****************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (truth-table bexp)

; that takes a Boolean expression bexp and returns the truth table for bexp
; where the variables for the table are extracted from bexp using all-vars, 
; and the function value for each row is obtained by evaluating bexp 
; in the corresponding environment.  Notice that all-vars specifies
; the order of variables for the truth table.

; Examples:

;> (truth-table exp1)
;(tt '() (list (entry '() 1)))

;> (truth-table exp2)
;(tt
; '(x y)
; (list (entry '(0 0) 0) (entry '(0 1) 1) (entry '(1 0) 1) (entry '(1 1) 1)))

;>  (truth-table exp4)
;(tt
; '(w x y)
; (list
;  (entry '(0 0 0) 0)
;  (entry '(0 0 1) 0)
;  (entry '(0 1 0) 0)
;  (entry '(0 1 1) 0)
;  (entry '(1 0 0) 1)
;  (entry '(1 0 1) 1)
;  (entry '(1 1 0) 0)
;  (entry '(1 1 1) 1)))
; ****************************************************************

(define (convert vars pseudo-env)
    (cond
      [(empty? pseudo-env) '()]
      [else (cons (entry (first vars) (first pseudo-env)) (convert (rest vars) (rest pseudo-env)))]))

(define (truth-table bexp)
  (let ((vars (all-vars bexp)))
    (let ((keys (all-keys (length vars))))
      (tt vars (map (lambda (x) (entry x (eval-in-env bexp (convert vars x)))) keys)))))

; ****************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (classify bexp)
; (equivalent? bexp1 bexp2)

; (classify bexp) takes a Boolean expression and classifies it, returning one of the
; identifiers: 'tautology, 'contradiction, or 'contingent.
; The expression is a tautology if it is true in every environment,
; a contradiction if it is false in every environment, and contingent
; if it is true in some environments and false in some environments.

; (equivalent? bexp1 bexp2) takes two Boolean expressions and returns #t if
; they are logically equivalent, and #f if they are not logically equivalent.
; Two expressions are logically equivalent if, for every environment that
; assigns Boolean values to ALL the variables that appear in either expression,
; the two expressions have the same value.

; For example, the expression 'a is not equivalent to the expression 'b,
; because in the environment (list (entry 'a 0) (entry 'b 1)),
; the first expression takes the value 0, but the second expression takes the value 1.

; These procedures will be tested on expressions with few enough
; variables that generating truth tables WILL BE a feasible approach.

; Examples
; (classify 0) => 'contradiction
; (classify (e-or (list 'x (e-not 'x)))) => 'tautology
; (classify exp2) => 'contingent
; (classify exp3) => 'contingent
; (classify (e-and '())) => 'tautology

; (equivalent? 0 (e-and (list 'a 0))) => #t
; (equivalent? 'a 'b) => #f
; (equivalent? (e-not (e-or (list 'a 'b 'c))) (e-and (list (e-not 'c) (e-not 'b) (e-not 'a)))) => #t
; ****************************************************************

(define (check_class lst type)
  (cond
    [(empty? lst) type]
    [(equal? 'contradiction type) (if (= (first lst) 0) (check_class (rest lst) type) 'contingent)]
    [(equal? 'tautology type) (if (= (first lst) 1) (check_class (rest lst) type) 'contingent)]
    [else (if (= (first lst) 1) (check_class (rest lst) 'tautology) (check_class (rest lst) 'contradiction))]
    ))

(define (classify bexp)
  (let ((t (tt-rows (truth-table bexp))))
    (check_class (map (lambda (x) (entry-value x)) t) 0)))

(define (truth-table-sorted bexp)
  (let ((vars (sort (all-vars bexp) symbol<?)))
    (let ((keys (all-keys (length vars))))
      (tt vars (map (lambda (x) (entry x (eval-in-env bexp (convert vars x)))) keys)))))

(define (equivalent? bexp1 bexp2)
  (let ((c1 (classify bexp1)) (c2 (classify bexp2)))
    (if (equal? c1 c2)
      (cond
        [(equal? 'contingent c1)
         (let ((t1 (truth-table-sorted bexp1))
               (t2 (truth-table-sorted bexp2)))
           (if (equal? t1 t2) #t #f))]
        [else #t]) #f)))
  
; ****************************************************************
; ** problem 9 ** (20 points)
; Write a procedure

; (find-exp tt)

; This procedure takes a truth table
; and returns a Boolean expression 
; for the given truth table.

; You may choose to use the sum-of-products algorithm
; from lecture, or some other method, but it must
; return a Boolean expression with the given truth table.

; Examples
; (find-exp tt-and) => (e-or (list (e-and '(x y))))

; (find-exp tt-nand) => (e-or
;                        (list
;                         (e-and (list (e-not 'a) (e-not 'b)))
;                         (e-and (list (e-not 'a) 'b))
;                         (e-and (list 'a (e-not 'b)))))

; (find-exp tt-xor) =>(e-or (list (e-and (list (e-not 'u) 'v)) (e-and (list 'u (e-not 'v))))

; (find-exp tt-f1) => (e-or
;                      (list
;                       (e-and (list (e-not 'a) 'b (e-not 'c)))
;                       (e-and (list (e-not 'a) 'b 'c))
;                       (e-and (list 'a (e-not 'b) 'c))
;                       (e-and '(a b c))))

; ****************************************************************

(define (sum-of-products lst vars)
  (cond
    [(empty? lst) empty]
    [else
     (e-and (map (lambda (x y) (if (= 1 x) y (e-not y))) lst vars))]))

(define (find-exp tt)
  (cond
    [(empty? (tt-rows tt)) empty]
    [else (let ((rows (tt-rows tt)) (vars (tt-vars tt)))
       (e-or (map (lambda (y) (sum-of-products (entry-key y) vars)) (filter (lambda (x) (= 1 (entry-value x))) rows))))]))

; ****************************************************************
; ** problem 10 ** (9 points)
; Write a procedure

; (simplify bexp)

; that takes a Boolean expression bexp and returns
; an equivalent Boolean expression that is
; simplified as much as possible using the following rules:

; x + 0 -> x
; 0 + x -> x
; x + 1 -> 1
; 1 + x -> 1
; x * 0 -> 0
; 0 * x -> 0
; x * 1 -> x
; 1 * x -> x
; 0' -> 1
; 1' -> 0
; (x')' -> x

; Also note that
; (e-or '()) should be simplified to 0
; (e-and '()) should be simplified to 1
; (e-or (list bexp)) should be simplified to bexp
; (e-and (list bexp)) should be simplified to bexp

; Examples
; (simplify 0) => 0
; (simplify 'x) => 'x
; (simplify (e-not (e-not 'x))) => 'x
; (simplify (e-not 'y)) => (e-not 'y)
; (simplify (e-or (list 0 (e-and (list 1 (e-not 0) 1))))) => 1
; (simplify (e-and (list 'x 1))) => 'x
; (simplify (e-or (list 0 'z 0))) => 'z
; (simplify (e-or (list (e-and (list 'x 1)) (e-or (list (e-not 1) 'z))))) => (e-or '(x z))
; ****************************************************************

(define (check-args-or args val)
  (cond
    [(empty? args) val]
    [(equal? 1 (first args)) 1]
    [(equal? 0 (first args)) (check-args-or (rest args) 0)]
    [else (check-args-or (rest args) val)]))

(define (check-args-and args val)
  (cond
    [(empty? args) val]
    [(equal? 0 (first args)) 0]
    [(equal? 1 (first args)) (check-args-and (rest args) 1)]
    [else (check-args-and (rest args) val)]))

(define (simplify bexp)
  (cond
    [(e-not? bexp)
     (cond
       [(equal? 0 (e-not-arg bexp)) 1]
       [(equal? 1 (e-not-arg bexp)) 0]
       [(e-not? (e-not-arg bexp)) (simplify (e-not-arg (e-not-arg bexp)))]
       [else (e-not (simplify (e-not-arg bexp)))])]
    [(e-or? bexp)
     (cond
       [(= 1 (length (e-or-args bexp))) (first (e-or-args bexp))]
       [(= 1 (check-args-or (e-or-args bexp) 2)) 1]
       [(= 0 (check-args-or (e-or-args bexp) 2))
        (let ((new (filter (lambda (x) (not (equal? 0 x))) (e-or-args bexp))))
          (if (empty? new) 0 (let ((simple (simplify new))) (if (equal? simple (e-or-args bexp)) (e-or simple) (simplify (e-or simple))))))]
       [else (let ((simple (simplify (e-or-args bexp)))) (if (equal? simple (e-or-args bexp)) (e-or simple) (simplify (e-or simple))))])]
    [(e-and? bexp)
     (cond
       [(= 1 (length (e-and-args bexp))) (first (e-and-args bexp))]
       [(= 1 (check-args-and (e-and-args bexp) 2))
        (let ((new (filter (lambda (y) (not (equal? 1 y))) (e-and-args bexp))))
          (if (empty? new) 1 (let ((simple (simplify new))) (if (equal? simple (e-and-args bexp)) (e-and simple) (simplify (e-and simple))))))]
       [(= 0 (check-args-and (e-and-args bexp) 2)) 0]
       [else (let ((simple (simplify (e-and-args bexp)))) (if (equal? simple (e-and-args bexp)) (e-and simple) (simplify (e-and simple))))])]
    [(equal? 1 bexp) 1]
    [(equal? 0 bexp) 0]
    [(symbol? bexp) bexp]
    [(list? bexp) (if (empty? bexp) '() (cons (simplify (first bexp)) (simplify (rest bexp))))]
    [else #f]))

; **************** end of hw #4 *********************************
