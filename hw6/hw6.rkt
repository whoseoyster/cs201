#lang racket

(provide con con? con-arg1 con-arg2
         alt alt? alt-arg1 alt-arg2
         rep rep? rep-arg
         ques ques? ques-arg
         reg-exp?
         flip try-flips pick
         reg-exp-gen
         dfa dfa? dfa-alph dfa-states dfa-start dfa-acc dfa-trans
         entry entry? entry-key entry-value
         dfa-accepts?
         cfg cfg? cfg-nonterminals cfg-start cfg-rules
         rule rule? rule-lhs rule-rhs
         cfg-gen
         my-cfg
         reg-exp->cfg)


; Please do not change lines above this one.

;************************************************************
; CS 201a HW #6  DUE Wednesday, April 19 at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name:
; Email address:
;************************************************************

; Computer science topics: strings, languages, regular expressions,
; deterministic finite state acceptors and context free grammars.
; Racket: operations on strings and characters.

; You may write auxiliary procedure(s) in addition to
; the one(s) specified in the problem.  Please include
; a comment for each one explaining its input and results.

; The following structs are used in the representation of Regular Expressions.

(struct con (arg1 arg2) #:transparent)
(struct alt (arg1 arg2) #:transparent)
(struct rep (arg) #:transparent)
(struct ques (arg) #:transparent)

; A string is a Racket string.  Consult the Racket Reference
; for information and procedures for strings and characters.

; A Regular Expression is defined recursively as follows:
; (1) a Racket string is a Regular Expression,
; (2) If exp1 and exp2 are Regular Expressions, then so are
; the following:
; (con exp1 exp2), the concatenation of exp1 and exp2
; (alt exp1 exp2), the "or" of exp1 and exp2, that is (exp1 | exp2)
; (rep exp1), zero or more repetitions of exp1, that is, (exp1)*
; (ques exp1), zero or one repetitions of exp1, that is (exp1)?

; Examples of Regular Expressions

(define exp1 "")
(define exp2 "abbab")
(define exp3 (con "ab" (alt "c" "d")))
(define exp4 (rep (alt "00" "11")))
(define exp5 (con (alt "a" "the")
                  (con (alt " mouse" " cat") 
                       (alt " ran" " slept"))))
(define exp6 (con "yog" (con (ques "h") "urt")))
;************************************************************
; ** problem 1 ** (10 points)
; Write a procedure

; (reg-exp? value)

; The procedure (reg-exp? value) takes an arbitrary Racket value
; and returns #t if it is a Regular Expression according to the
; definition given above, and #f otherwise.
; Hint: write a recursive procedure patterned after the recursive definition

; Examples
; (reg-exp? exp1) => #t
; and similarly for exp2 through exp6
; (reg-exp? 'a) => #f
; (reg-exp? '(a b b a b)) => #f
;****************************

(define (reg-exp? value)
  "not done yet")

;************************************************************
; ** problem 2 ** (10 points)
; Write three procedures

; (flip bias)
; (try-flips count bias)
; (pick lst)

; The procedure (flip bias) simulates flipping a biased coin, 
; where the bias is specified as a number between 0 and 1,
; and the result is #t with probability equal to the bias 
; and #f with probability (1 - bias). 

; The procedure (try-flips count bias)
; calls (flip bias) the number of times specified by count
; and returns a list of two integers:
; the first is how many times the flip returned #t
; and the second is how many times the flip returned #f

; Note that the racket procedure (random) returns a random
; number between 0 and 1.

; The procedure (pick lst) takes a list lst and returns
; a randomly chosen element of lst.  If lst is empty, the
; value returned should be #f.  You can test it by picking 10000
; times from a list with 10 elements, and making sure that
; each element is picked about 1000 times.

;Examples (your results will randomly vary)
;> (flip .9)
;#f
;> (flip .9)
;#t
;> (try-flips 1000 .4)
;'(427 573)
;> (try-flips 1000 .69)
;'(686 314)
;> (pick '(a b c d e))
;'c
;> (pick '(a b c d e))
;'e
;************************************************************

(define (flip bias)
  "not done yet")

(define (try-flips count bias)
  "not done yet")

(define (pick lst)
  "not done yet")

;************************************************************
; ** problem 3 ** (20 points)
; Write a procedure

; (reg-exp-gen exp)

; that takes a Regular Expression exp and
; returns a random string in the language
; denoted by exp.  Every string in the language
; must have a positive probability of being chosen,
; and every string not in the language must have a
; probability of 0 of being chosen.

; Examples (yours may randomly differ):
;> (reg-exp-gen exp1)
;""
;> (reg-exp-gen exp2)
;"abbab"
;> (reg-exp-gen exp3)
;"abc"
;> (reg-exp-gen exp4)
;"001111110000111111111100"
;> (reg-exp-gen exp4)
;""
;> (reg-exp-gen exp5)
;"the mouse slept"
;> (reg-exp-gen exp6)
;"yoghurt"
;> (reg-exp-gen exp6)
;"yogurt"

;************************************************************

(define (reg-exp-gen exp)
  "not done yet")

;************************************************************
; A (possibly incomplete) Deterministic Finite State Acceptor (DFA)
; is represented by the following struct.

(struct dfa (alph states start acc trans) #:transparent)

; where 
; alph is a list of Racket characters -- the symbols of the alphabet
; states is a list of Racket symbols
; start is one of the elements of states (the start state)
; acc is a list containing some of the elements of states (the accepting states)
; and trans is a table whose entries
;    have a key that is a list containing a state and a member of the alphabet
;         a value that is a state

(struct entry (key value) #:transparent)

; Examples of DFAs.
; Here is a DFA for the language of all strings of a's and b's with
; an odd number of a's and any number of b's.

(define odd-as
  (dfa
    '(#\a #\b)
    '(even odd)
    'even
    '(odd)
    (list
     (entry '(even #\a) 'odd)
     (entry '(even #\b) 'even)
     (entry '(odd #\a) 'even)
     (entry '(odd #\b) 'odd))))

; Here is an (incomplete) DFA to accept the language of the
; regular expression c(a|d)(a|d)*r

(define car-cdr
  (dfa
   '(#\a #\c #\d #\r)
   '(start saw-c saw-a-or-d saw-r)
   'start
   '(saw-r)
   (list
    (entry '(start #\c) 'saw-c)
    (entry '(saw-c #\a) 'saw-a-or-d)
    (entry '(saw-c #\d) 'saw-a-or-d)
    (entry '(saw-a-or-d #\a) 'saw-a-or-d)
    (entry '(saw-a-or-d #\d) 'saw-a-or-d)
    (entry '(saw-a-or-d #\r) 'saw-r))))

;************************************************************
; ** problem 4 ** (20 points)
; Write a procedure 

; (dfa-accepts? mach str)

; to take a DFA mach and a Racket string str and determine whether the
; DFA accepts the string.  Note that if an undefined transition
; is encountered, the string is rejected.

; Examples
;> (dfa-accepts? odd-as "aababa")
;#f
;> (dfa-accepts? odd-as "bbabbb")
;#t
;> (dfa-accepts? car-cdr "cadar")
;#t
;> (dfa-accepts? car-cdr "card")
;#f
;> (dfa-accepts? odd-as "what?")
;#f
;************************************************************

(define (dfa-accepts? mach str)
  "not done yet")
     
;************************************************************
; A Context Free Grammar (CFG) is represented using the following.

(struct cfg (nonterminals start rules) #:transparent)

(struct rule (lhs rhs) #:transparent)

; where
; nonterminals is a list of Racket symbols
; start is an element of the nonterminal list
; rules is a list of rule structs -- each of which has
; a lhs that is an element of the nonterminals list, and
; a rhs that is a list of elements that may be from the nonterminals list
;   or may be Racket strings

; Examples of CFGs.
; Here is an example CFG from lecture.

(define grammar-mcd
  (cfg
   '(s np vp det n pn vi vt v3)
   's
   (list
    (rule 's '(np vp))
    (rule 'np '(det n))
    (rule 'np '(pn))
    (rule 'det '("a"))
    (rule 'det '("the"))
    (rule 'n '("mouse"))
    (rule 'n '("cat"))
    (rule 'n '("dog"))
    (rule 'pn '("it"))
    (rule 'vp '(vi))
    (rule 'vp '(vt np))
    (rule 'vp '(v3 "that" s))
    (rule 'vi '("slept"))
    (rule 'vi '("swam"))
    (rule 'vt '("chased"))
    (rule 'vt '("evaded"))
    (rule 'v3 '("dreamed"))
    (rule 'v3 '("believed")))))

; Here is the grammar for the set of strings consisting of
; n a's followed by n b's, for all nonnegative integers n.

(define grammar-anbn
  (cfg
   '(s)
   's
   (list
    (rule 's '(""))
    (rule 's '("a" s "b")))))

; Here is a grammar that generates some statements in MiniJava
; This is not the same as the MiniJava grammar distributed in class.

(define grammar-mj
  (cfg
   '(block statements statement expression boolean-expression term identifier integer-literal)
   'block
   (list
    (rule 'block '("{" statements "}"))
    (rule 'statements '(statement))
    (rule 'statements '(statement statements))
    (rule 'statement '(identifier "=" expression ";"))
    (rule 'statement '("while" "(" boolean-expression ")" block))
    (rule 'statement '("System.out.prntln" "(" expression ")" ";"))
    (rule 'expression '(expression "+" term))
    (rule 'expression '(expression "-" term))
    (rule 'expression '(term))
    (rule 'boolean-expression '(term "<" term))
    (rule 'term '(identifier))
    (rule 'term '(integer-literal))
    (rule 'identifier '("sum"))
    (rule 'identifier '("n"))
    (rule 'integer-literal '("0"))
    (rule 'integer-literal '("1"))
    (rule 'integer-literal '("6")))))
 
;************************************************************
; ** problem 5 ** (20 points)
; Write a procedure

; (cfg-gen grammar)

; that takes a CFG grammar and produces a randomly chosen element of the language of the grammar.
; Every element in the language of the grammar should have a non-zero
; probability of being generated, and every element not in the language
; should have probability 0 of being generated.

; Hint: one way to approach this is to write an auxiliary procedure
; that takes a grammar and a list of nonterminal symbols and strings, and
; (1) returns the list of strings if there are no nonterminal symbols
; (2) calls itself recursively after replacing the leftmost
; nonterminal in the list by the righthand side of 
; a randomly chosen grammar rule which has that nonterminal as its lefthand side.

; Examples
;> (cfg-gen grammar-mcd)
;'("it" "dreamed" "that" "a" "mouse" "slept")
;> (cfg-gen grammar-anbn)
;'("a" "" "b")
;> (cfg-gen grammar-mj)
;'("{" "System.out.prntln" "(" "0" "-" "6" ")" ";" "}")

; For better-looking outputs:
;> (string-join (cfg-gen grammar-mcd))
;"it evaded it"
;> (apply string-append (cfg-gen grammar-anbn))
;""
;> (apply string-append (cfg-gen grammar-anbn))
;"ab"
;> (apply string-append (cfg-gen grammar-anbn))
;"aaaaabbbbb"
;> (string-join (cfg-gen grammar-mj))
;"{ System.out.prntln ( 6 ) ; while ( n < 1 ) { System.out.prntln ( 1 + 1 - sum + sum ) ; n = sum ; } }"

;************************************************************

(define (cfg-gen grammar)
  "not done yet")

;************************************************************
; ** problem 6 ** (10 points)
; Define your own CFG named my-cfg of complexity at least as great as 
; that of grammar-mcd.  Give (as comments) some examples of sentences 
; generated by your grammar. 
; (Please do more than just copy grammar-mcd with a few changes!!!)
;************************************************************

(define my-cfg
  (cfg "not" "done" "yet"))

;************************************************************
; ** problem 7 ** (10 points)
; Write a procedure

; (reg-exp->cfg exp)

; that takes a regular expression exp
; returns a context free grammar for the language denoted by exp

; Examples (yours may differ randomly)

;> (apply string-append (cfg-gen (reg-exp->cfg exp1)))
;""
;> (apply string-append (cfg-gen (reg-exp->cfg exp2)))
;"abbab"
;> (apply string-append (cfg-gen (reg-exp->cfg exp3)))
;"abd"
;> (apply string-append (cfg-gen (reg-exp->cfg exp4)))
;"1100"
;> (apply string-append (cfg-gen (reg-exp->cfg exp5)))
;"the mouse ran"
;> (apply string-append (cfg-gen (reg-exp->cfg exp6)))
;"yogurt"
;************************************************************

(define (reg-exp->cfg exp)
  "not done yet")
  
;************************************************************