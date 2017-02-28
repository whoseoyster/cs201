#lang racket

(provide hours
         all-distinct?
         list-minus
         sum-of pick-random roll
         ok-move? make-move
         all-subsets
         possible-moves next-states
         choose-random-move choose-short-move
         game-play
         one-game many-games
         my-move-chooser)

; Please do not modify the lines above this one.

; CS 201b HW #2  DUE Wednesday, February 8, 2017 at 11:59 pm
; electronically, using the submit system on the Zoo.
;**************************************************************
; Name: Rishab Ramanathan
; Email address: rishab.ramanathan@yale.edu
;**************************************************************
; If you are asked to write a procedure, unless the problem specifies
; otherwise, you MAY write auxiliary procedures. 
; For each of your auxiliary procedures, 
; please include a succinct comment explaining what it does and giving
; at least one example of its behavior.

; You MAY use procedures you have written on previous
; assignments.  They only need to be included once in this file.
; Every procedure you define should have been originally written by you.

; Please do not use mutators (set! and its relatives.)

; Topics: Racket, the game of shut-the-box

;**************************************************************
; ** problem 0 ** (1 easy point)
; Please change 0 in the definition below to
; reflect how long you spent on this assignment.

(define hours 5)

;**************************************************************
; ** problem 1 ** (9 points)
; Write one procedure:

; (all-distinct? lst)

; that takes a list lst and
; returns #t if no pair of top-level elements of lst are equal?
; and returns #f otherwise.
; Hint: think about the base case.

; Examples
; (all-distinct? '(1 4 6)) => #t
; (all-distinct? '(3 1 2 1)) => #f
; (all-distinct? '((1 2) (2 1) (1 1) (2 2))) => #t
; (all-distinct? (list (+ 3 1) (quotient 6 2) (* 2 2))) => #f
;**************************************************************

(define (all-distinct? lst) 
  (cond
     [(empty? lst) #t] 
     [(member (first lst) (rest lst)) #f]
     [else (all-distinct? (rest lst)) ])) 

;**************************************************************
; ** problem 2 ** (10 points)
; Write one procedure:

; (list-minus lst1 lst2)

; that takes two lists as input and returns the
; list of all the elements of lst1 that are not equal? to
; some top-level element of lst2, preserving the original
; order of the elements in lst1.

; Examples
; (list-minus '(1 2 3 4) '(4 2)) => '(1 3)
; (list-minus '(2 2 3 3 3) '(3 4 4)) => '(2 2)
; (list-minus '((1 2) (2 1) (2 1) (2 2)) '((1 1) (2 1))) => '((1 2) (2 2))
;**************************************************************

(define (list-minus lst1 lst2)
  (cond
     [(empty? lst1) empty] 
     [else
         (if (not (member (first lst1) lst2)) (cons (first lst1) 
         (list-minus (rest lst1) lst2)) (list-minus (rest lst1) lst2)) ])) 

;**************************************************************
; For the remaining problems, we consider the game of "shut the box,"
; which will be (or was) demonstrated in lecture.

; The traditional verson of the game has 9 flappers numbered 1 to 9, 
; each of which may be *up* or *down*.  Initially they are all *up*.

; In the two-player game, player 1 starts from the initial configuration
; and repeatedly rolls two dice and manipulates the flappers until 
; the score for player 1 is determined.  
; Then all the flappers are again placed in the *up* position,
; and player 2 repeatedly rolls two dice and manipulates the flappers
; until the score for player 2 is determined.
; The player with the LOWER score wins (or there is a tie
; if the scores are equal.)

; The rules for rolling dice and manipulating flappers follow.
; The player rolls the dice and notes their sum.
; The player then flips *down* any combination of *up* flappers 
; whose sum is equal to the sum of the dice rolled.
; This process repeats until there is no combination
; of *up* flappers whose sum equal to the sum of the dice rolled, 
; at which point the player's score is the sum of the remaining
; *up* flappers.

; An example of determining the score for one player follows.
; Initially *up* flappers: 1 2 3 4 5 6 7 8 9
; Player rolls 2 and 6, and decides to flip *down* 8
; The *up* flappers are now: 1 2 3 4 5 6 7 9
; Player rolls 6 and 6, and decides to flip *down* 3 and 9
; The *up* flappers are now: 1 2 4 5 6 7
; Player rolls 1 and 3, and decides to flip *down* 4
; The *up* flappers are now: 1 2 5 6 7
; Player rolls 6 and 6, and decides to flip *down* 5 and 7
; The *up* flappers are now: 1 2 6
; Player rolls 5 and 6, and there is no way to make a sum of 11,
; so the final score for player is 1+2+6 = 9

; If a player succeeds in flipping down ALL
; the flappers, the player is said to have "shut the box",
; and that player's final score is 0, which is best possible.

;**************************************************************
; The following defines the identifier standard-flappers in the
; top-level environment to be the list of the numbers on
; the flappers in the standard game.

(define standard-flappers '(1 2 3 4 5 6 7 8 9))

; A die (singular of dice) is represented by a non-empty
; list of positive integers in non-decreasing order,
; representing the numbers on the faces of the die.
; The following is the standard die; there may
; also be *non-standard dice* that your procedures
; must deal with.

(define standard-die '(1 2 3 4 5 6))

; The standard set of dice is a list with two copies of the standard die.  
; Your procedures should be able to deal with a non-empty list
; of one, two, three or more possibly different dice.

(define standard-dice (list standard-die standard-die))

; A state of the game is represented the list of *up* flappers
; in *increasing order*.
; The initial state in the standard game is '(1 2 3 4 5 6 7 8 9).
; In the example above, the state at the end of the game is '(1 2 6).

;**************************************************************
; ** problem 3 ** (10 points)
; Write three procedures:

; (sum-of lst)
; (pick-random lst)
; (roll dice)

; (sum-of lst) takes a list lst of numbers, and returns their sum.
; (Note that the sum of an empty list of numbers is 0.)

; (pick-random lst) takes a list lst and returns a uniformly randomly
; chosen top-level element of lst.  If lst is empty, it returns '().

; (roll dice) returns the list of values obtained by chosing a 
; random face of each die in the list of dice, in order.

; To provide randomness, please use the built-in Racket procedure 
; (random n), which takes a positive integer n and returns
; a randomly-chosen integer greater than or equal to 0 and less than n.

; You may also want to investigate the built-in procedure list-ref.

; Examples
; (Note that for examples that involve randomness, your results may differ.)
; (sum-of '()) => 0
; (sum-of '(1 4 5 6)) => 16
; (pick-random '(1 2 3 4 5 6)) => 6
; (pick-random '(1 2 3 4 5 6)) => 4
; (pick-random '()) => '()
; (roll standard-dice) => '(5 3)
; (roll '((1 2 3) (1 1 4) (2 2 2))) => '(2 4 2)
;**************************************************************

(define (sum-of lst)
  (cond
    [(empty? lst) 0]
    [(empty? (rest lst)) (first lst)]
    [else
      (+ (first lst) (sum-of (rest lst)))]))

(define (pick-random lst)
  (if (empty? lst) '() (list-ref lst (random (length lst)))))

(define (roll dice)
  (cond
    [(empty? dice) empty]
    [else
     (cons (pick-random (first dice)) (roll (rest dice)))]))

;**************************************************************
; ** problem 4 ** (10 points)
; A move consists of a list of positive integers in *increasing order*
; indicating that flappers with those numbers should be flipped *down*.

; Write two procedures

; (ok-move? move state)
; (make-move move state)

; where state is a possible state of the flappers and move 
; is a move as indicated above.

; (ok-move? move state)
; given a move and a state
; returns #t if the move is possible in the state and #f otherwise.
; A move is possible if we can flip down a flapper in state corresponding 
; to each of the numbers in move.

; (make-move move state)
; given a move and a state
; returns the state that results from making the given move
; (ie, flipping down the indicated flappers.)
; You may assume that the move is possible in the given state.

; Examples
; (ok-move? '(1 2 6) '(1 2 3 5 6 9)) => #t
; (ok-move? '(3 7) '(1 2 3 5 6 9)) => #f
; (ok-move? '(4 6) '(4 5 6)) => #t
; (ok-move? '(2 4) '(1 2 3)) => #f
; (make-move '(1 2 6) '(1 2 3 5 6 9)) => '(3 5 9)
; (make-move '() '(3 4 5)) => '(3 4 5)
; (make-move '(3 4 5) '(3 4 5)) => '()
; (make-move '(4 5) '(1 2 3 4 5)) => '(1 2 3)
;**************************************************************

(define (ok-move? move state)
  (cond
     [(empty? move) #t] 
     [(not (member (first move) state)) #f]
     [else (ok-move? (rest move) state) ])) 
  
(define (make-move move state)
    (list-minus state move))

;**************************************************************
; ** problem 5 ** (10 points)
; Write a procedure:

; (all-subsets lst)

; that takes a list lst of positive integers in increasing order
; and returns a list of lists, one for each subset of the elements
; of lst -- each list in the output must have numbers in increasing
; order, but the order of the lists may be different from what is
; shown in the examples.

; Examples
; (all-subsets '()) => '(())
; (all-subsets '(2)) => '(() (2))
; (all-subsets '(2 5)) => '(() (5) (2) (2 5))
; (all-subsets '(2 5 8)) => '(() (8) (5) (5 8) (2) (2 8) (2 5) (2 5 8))

; Hint: suppose you have the answer
; '(() (8) (5) (5 8)) for (all-subsets '(5 8)).
; What needs to be done to this to get the correct answer 
; for (all-subsets '(2 5 8))?
;**************************************************************

(define (all-subsets state)
    (if (empty? state) '(())
        (let ((subsets-rest-state (all-subsets (rest state))))
          (append subsets-rest-state
              (map (lambda (subset) (cons (first state) subset))
                  subsets-rest-state)))))

;**************************************************************
; ** problem 6 ** (10 points)
; Write two procedures:

; (possible-moves sum state)
; (next-states sum state)

; where sum is the sum of a dice roll
; and state is a possible game state.

; (possible-moves sum state)
; returns the list of all possible moves whose elements sum 
; to the value sum in the given state.
; The moves may be listed in *any order*, but there should not be duplicates.

; (next-states sum state)
; returns a list of all the states reachable from the given state 
; by making a possible move with value equal to sum.
; The states may be listed in *any order*, but there should be no duplicates.

; Examples
; (possible-moves 12 '(3 4 5 7 9)) => '((5 7) (3 9) (3 4 5))
; (possible-moves 6 '(1 2 3 4 5 6)) => '((6) (2 4) (1 5) (1 2 3))
; (possible-moves 11 '(1 2 6)) => '()
; (next-states 12 '(3 4 5 7 9)) => '((3 4 9) (4 5 7) (7 9))
; (next-states 11 '(1 2 6)) => '()
;**************************************************************

(define (moves sum state)
    (cond
      ((empty? state) empty)
      (else (if (= sum (sum-of (first state))) (cons (first state) (moves sum (rest state)))
                (moves sum (rest state))))))

(define (possible-moves sum state)
  (let ((all-states (all-subsets state)))
    (moves sum all-states)))

(define (nexts state possibilities)
    (cond
      ((empty? possibilities) empty)
      (else (cons (list-minus state (first possibilities)) (nexts state (rest possibilities))))))

(define (next-states sum state)
     (let ((possibilities (possible-moves sum state)))
       (nexts state possibilities)))

;**************************************************************
; ** problem 7 ** (10 points)
; Write two procedures:

; (choose-random-move sum state)
; (choose-short-move sum state)

; where sum is the sum of a dice roll
; and state is a possible game state.
; Each procedure returns one
; of the moves that is possible in the given state
; when the sum of the dice is as indicated.
; If there is no legal move, '() is returned.
; They differ in how the move is chosen.

; (choose-random-move sum state)
; chooses uniformly at random among all the possible moves
; in the given state with the given sum.

; (choose-short-move sum state)
; returns a move that is shortest (in terms of list length) from
; all the possible moves in the given state with the given sum.
; If there are several possible shortest moves, it chooses 
; uniformly at random among them.

; Examples
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => '(1 2 3)
; (choose-random-move 6 '(1 2 3 4 5 6 7 8 9)) => '(6)
; (choose-random-move 2 '(3 4 5)) => '()
; (choose-short-move 6 '(1 2 3 4 5 6 7 8 9)) => '(6)
; (choose-short-move 7 '(1 2 3 4 5)) => '(3 4)
; (choose-short-move 7 '(1 2 3 4 5)) => '(2 5)
;**************************************************************

(define (choose-random-move sum state)
  (let ((possibles (possible-moves sum state)))
    (if (empty? possibles) '() (pick-random possibles))))

(define (choose-shortest lst shortest)
  (cond
    ((empty? shortest) (choose-shortest (rest lst) (list (first lst))))
    ((empty? lst) (pick-random shortest))
    ((empty? (rest lst))
     (cond
       ((= (length (first lst)) (length (first shortest))) (pick-random (append lst shortest)))
       ((< (length (first lst)) (length (first shortest))) (first lst))
       ((> (length (first lst)) (length (first shortest))) (pick-random shortest))))
    ((= (length (first lst)) (length (first shortest)))
              (choose-shortest (rest lst) (append (list (first lst)) shortest)))
    ((< (length (first lst)) (length (first shortest)))
              (choose-shortest (rest lst) (list (first lst))))
    (else (choose-shortest (rest lst) shortest))))
    
(define (choose-short-move sum state)
  (let ((possibles (possible-moves sum state)))
    (if (empty? possibles) '() (choose-shortest possibles '()))))
    

;**************************************************************
; ** problem 8 ** (10 points)
; Write a procedure:

; (game-play move-chooser state dice)

; that takes a procedure move-chooser,
; a game state, and a list of dice,
; and simulates playing shut the box starting
; the given game state using the given dice
; and the procedure move-chooser to choose each next move.

; It repeatedly
; (1) rolls the dice and finds their sum,
; (2) calls move-chooser with the sum and the current game state,
; (3) if move-chooser returns '(), then game-play returns a list of 
;     game states (as specified below),
; (4) otherwise, game-play continues from (1) with the new current state resulting
;     from making the move

; The output list should consist of a list giving the successive
; game states, starting with the given one and ending with the final one.

; Like the procedures choose-random-move and choose-short-move,
; the procedure move-chooser takes as input
; the sum of a dice roll and a game state and returns either
; a move that is possible in the given state and sums to the given sum,
; or '() if there is no move possible for the given sum in the given state.

; You may assume the move-chooser behaves correctly, that is,
; always returns either a move or '().  If it returns a move, then the move 
; is possible in the given state and sums to the given sum.
; If it returns '(), then there is no move possible
; in the given state that sums to the given sum.

; Examples
; (Your choose-random-move and choose-short-move
; need to be working to run these tests.  
; Because of randomness, your results may differ.)

; Examples
; (game-play choose-random-move standard-flappers standard-dice) =>
; '((1 2 3 4 5 6 7 8 9) (2 3 4 5 6 8 9) (4 6 8 9))
; (game-play choose-random-move standard-flappers standard-dice) =>
; '((1 2 3 4 5 6 7 8 9) (1 4 5 6 7 8 9) (5 6 7 8 9) (5 7 8 9) (7 8 9) (8 9) (8))
; (game-play choose-short-move standard-flappers standard-dice)
; '((1 2 3 4 5 6 7 8 9) (1 2 4 5 6 7 8 9) (1 2 4 5 6 7 9) (1 2 4 6 7 9) (1 2 6 7 9))

; Following are non-standard flappers and non-standard dice

; (game-play choose-random-move '(1 2 3 4 5 6) '((1 2) (1 2) (1 2))) =>
; '((1 2 3 4 5 6) (1 2 3 5 6) (1 5 6) (1 6))
; (game-play choose-short-move '(1 2 3 4 5 6) '((1 2) (2 3) (1 3))) =>
; '((1 2 3 4 5 6) (1 2 3 4 6) (1 3 4) (1))
;**************************************************************

(define (game-play move-chooser state dice)
  (let ((sum (sum-of (roll dice))))
    (let ((move (move-chooser sum state)))
      (if (empty? move)
          (if (empty? state) empty (list state))
          (cons state (game-play move-chooser (list-minus state move) dice))))))

;**************************************************************
; ** problem 9 ** (10 points)
; Write two procedures

; (one-game move-chooser1 move-chooser2)
; (many-games n move-chooser1 move-chooser2)

; that simulate either one game or many games
; between the strategies of move-chooser1 and move-chooser2
; with the standard flappers and standard pair of dice.

; The result for one-game should be
; "win" if move-chooser1 wins the game between the two
; "tie" if they tie
; "lose" if move-chooser1 loses the game between the two

; The result for many-games should simulate n games
; between the two strategies and return a list
; containing the number of games won by
; move-chooser1, the number of ties, and the
; number of games won by move-chooser2

; Recall that the SMALLER score wins!

; Examples:
; (one-game choose-random-move choose-random-move) => "win"
; (one-game choose-random-move choose-random-move) => "lose"
; (many-games 100 choose-random-move choose-random-move) => '(44 2 54)
; (many-games 100 choose-short-move choose-random-move) => '(75 5 20)

; This last result suggests choose-short-move has a noticeable advantage
; over choose-random-move.
;**************************************************************

(define (one-game move-chooser1 move-chooser2)
  (let ((result1 (sum-of (first (reverse (game-play move-chooser1 standard-flappers standard-dice)))))
        (result2 (sum-of (first (reverse (game-play move-chooser2 standard-flappers standard-dice))))))
  (cond
    ((> result1 result2) "lose")
    ((< result1 result2) "win")
    (else "tie"))))

(define (scores n w t l move-chooser1 move-chooser2)
  (if (= n 0) (list w t l)
      (let ((result (one-game move-chooser1 move-chooser2)))
        (cond
        ((equal? result "win") (scores (- n 1) (+ w 1) t l move-chooser1 move-chooser2))
        ((equal? result "lose") (scores (- n 1) w t (+ l 1) move-chooser1 move-chooser2))
        (else (scores (- n 1) w (+ t 1) l move-chooser1 move-chooser2))))))
        

(define (many-games n move-chooser1 move-chooser2)
  (scores n 0 0 0 move-chooser1 move-chooser2))

;**************************************************************
; ** problem 10 ** (10 points)
; Write one procedure 

; (my-move-chooser sum state)

; that chooses a possible move for sum and state, and
; try to make its performance better (using standard-flappers
; and standard-dice) than both choose-random-move and choose-short-move
; from problem #8.  You may find it useful to test my-move-chooser against
; those two procedures using the many-games procedure of problem #9.
;**************************************************************

(define (choose-with-lowest lst lowest)
  (cond
    ((empty? lst) lowest)
    ((> (first (reverse (first lst))) (first (reverse lowest))) (choose-with-lowest (rest lst) (first lst)))
    (else (choose-with-lowest (rest lst) lowest))))

(define (choose-best lst shortest)
  (cond
    ((empty? shortest) (choose-best (rest lst) (list (first lst))))
    ((empty? lst) (choose-with-lowest shortest (first shortest)))
    ((empty? (rest lst))
     (cond
       ((= (length (first lst)) (length (first shortest))) (choose-with-lowest (append lst shortest) (first (append lst shortest))))
       ((< (length (first lst)) (length (first shortest))) (first lst))
       ((> (length (first lst)) (length (first shortest))) (choose-with-lowest shortest (first shortest)))))
    ((= (length (first lst)) (length (first shortest)))
              (choose-best (rest lst) (append (list (first lst)) shortest)))
    ((< (length (first lst)) (length (first shortest)))
              (choose-best (rest lst) (list (first lst))))
    (else (choose-best (rest lst) shortest))))
    
(define (my-move-chooser sum state)
  (let ((possibles (possible-moves sum state)))
    (if (empty? possibles) '() (choose-best possibles '()))))

;******************  end of hw #2  ****************************