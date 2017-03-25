#lang racket

(provide hours
         entry entry? entry-key entry-value
         symbol-table
         bin->n tc->n n->bin n->tc
         assemble-one assemble
         ram-read ram-write equal-rams?
         conf conf? conf-cpu conf-ram
         cpu cpu? cpu-acc cpu-pc cpu-rf cpu-aeb
         equal-configs? addr->pc add-to-pc 
         acc->mem mem->acc
         sum diff
         do-input do-output
         next-config
         init-config simulate run
         prog-sort-two prog-reverse)

;************************************************************
; CS 201 HW #5 due Wednesday, April 5, 2017 at 11:59 pm, 
; via the submit system on the Zoo.
;************************************************************
; Name:
; Email address:
;************************************************************
; Computer science topics: TC-201 assembler and simulator,
; assembly language programs for sorting two numbers and
; reading in and printing out a list of numbers in reverse order.
; 
; ** You may solve the problem using any Racket constructs 
;  except mutators (set! and its relatives.)  Please do not use require.
; ** You may write auxiliary procedure(s) in addition to the one(s) 
; specified in the problem.  Please include a succint comment for
; each one specifying its intended inputs and return values.
; ** Please make your code as clear and readable as possible.

;************************************************************
; ** problem 0 ** (1 point)
; Modify the following definition to reflect the number of
; hours you spent on this assignment.

(define hours 0)

;************************************************************

; A table is a list of entries, where each entry has two fields: key and value.
; The constructor for entries is entry, the type predicate is entry?, and the
; two selectors are entry-key and entry-value.

(struct entry (key value) #:transparent)

;************************************************************
; We'll start by writing a TC-201 assembler, that is,
; a procedure that takes a symbolic assembly language
; program as input and returns as output the corresponding
; list of 16-bit words representing TC-201 instructions and data.

; As an example, here is a version of the program we wrote in lecture
; to sum up a zero-terminated sequence of numbers, output the sum, and halt.
; Note that it initializes sum to 0 before beginning the read loop.

(define prog-sum
  '((start:  load constant-0)
   (         store sum)
   (next:    input)
   (         skipzero)
   (         jump add-num)
   (         load sum)
   (         output)
   (         halt)
   (add-num: add sum)
   (         store sum)
   (         jump next)
   (sum:     data 0)
   (constant-0: data 0)))

; Here is the result of assembling this program

;> (assemble prog-sum)
;'((0 0 0 1 0 0 0 0 0 0 0 0 1 1 0 0)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0)
;  (1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 1 1 1 0 0 0 0 0 0 0 0 1 0 0 0)
;  (0 0 0 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 1 1 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 1)
;  (0 1 1 1 0 0 0 0 0 0 0 0 0 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; Here are two simpler test programs.
; First, a program with only instructions, 
; numeric addresses, and no labels.

(define prog1
  '((load 3)
    (store 4)
    (halt)))

; Second, a program with only data statements, three labels, and both numeric
; and symbolic data values

(define prog2
  '((x: data 7)
    (y: data -6)
    (z: data y)))

; Here are the values returned by assemble on these two programs.

;> (assemble prog1)
;'((0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;  (0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

;> (assemble prog2)
;'((0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)
;  (1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0)
;  (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1))

; Note the two's complement representation of -6, and
; the fact that the label y translates to the number 1
; in this example.

;************************************************************
; A symbolic assembly-language program is a list of lists, with
; each list representing one instruction or data statement.
; An instruction or data statement may optionally have a label,
; that is, a symbol ending in colon (:), which is the first
; element of the list.  The next symbol must be one of the
; opcodes (in the table opcode-table, below) or the symbol 'data.

; For the opcodes load, store, add, sub, jump, loadi, storei,
; there is one more field, the address field, which may be 
; a label (defined somewhere in the program) or a 
; decimal number between 0 and 4095 inclusive.
; For the other opcodes, there is no additional field.
; For the data directive, there is one more field, the
; value, which may be a label (defined somewhere in the
; program) or a decimal number between -32768 and 32767
; inclusive.

;************************************************************
; ** problem 1 ** (9 points)
; Write a procedure

; (symbol-table prog)

; that takes a symbolic assembly-language program prog
; as input, and returns a table with entries listing
; (in order) the labels defined in the program and their
; corresponding numeric values (instructions and data
; statements are numbered from 0.)

; Note that when they are defined, the labels have a colon (:)
; at the end, and when they are in the symbol table the
; final colon is removed.

; You will probably want to write one or more auxiliary
; procedures to deal with labels.  The procedures
; symbol->string, string->symbol, string-length, string-ref
; and substring will be useful, together with character representations.
; (See the Racket documentation.)

; Examples
;> (symbol-table prog1)
;'()

;> (symbol-table prog2)
;(list (entry 'x 0) (entry 'y 1) (entry 'z 2))

;> (symbol-table prog-sum)
;(list (entry 'start 0) (entry 'next 2) (entry 'add-num 8) (entry 'sum 11) (entry 'constant-0 12))
;************************************************************

(define (symbol-table prog)
  "not done yet")

;************************************************************
; Next we look at converting between decimal numbers and
; lists of binary digits representing integers in unsigned
; binary and two's complement representations.

;************************************************************
; ** problem 2 ** (10 points)
; Write four procedures:

; (bin->n lst)
; takes a list of binary digits and returns the nonnegative
; integer that they represent in unsigned binary in base 2.

; (tc->n lst)
; takes a list of k binary digits and returns the negative, zero, or
; positive number that they represent in k-bit two's complement representation.
; You may assume k is at least 2.

; (n->bin n len)
; takes a nonnegative integer n and returns a list of len binary digits
; representing n in unsigned binary.  If necessary, the representation 
; is padded on the left with 0's.  If the number n cannot be represented
; correctly in unsigned binary using len bits, the symbol 'error is returned.

; (n->tc n len)
; If the negative, zero, or positive integer n can be correctly represented
; in two's complement binary representation with len bits, a list of binary digits
; giving that representation is returned.  Otherwise, the symbol 'error is returned.

; Examples
;> (bin->n '(0 0 1 1))
;3
;> (bin->n '(1 1 1 1 1))
;31
;> (tc->n '(0 0 1 1))
;3
;> (tc->n '(1 1 0 1))
;-3
;> (tc->n '(1 1 1 1 1 1 1 1 1 1 1 1 1 0 0 1))
;-7
;> (n->bin 13 5)
;'(0 1 1 0 1)
;> (n->bin 13 3)
;'error
;> (n->tc 13 16)
;'(0 0 0 0 0 0 0 0 0 0 0 0 1 1 0 1)
;> (n->tc -6 16)
;'(1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 0)
;> (n->tc 17 4)
;'error
;************************************************************

(define (bin->n lst)
  "not done yet")

(define (tc->n lst)
  "not done yet")

(define (n->bin n len)
  "not done yet")

(define (n->tc n len)
  "not done yet")

;************************************************************
; Now we create a procedure to assemble one
; line of a program (given the symbol table), and use that
; to assemble the whole program.

;************************************************************
; ** problem 3 ** (10 points)
; Write two procedures:

; (assemble-one line table)
; takes one line (instruction or data statement) from a program
; and a symbol table for the program
; and returns a list of 16 bits representing that line of the program.

; (assemble prog)
; takes a symbolic assembly-language program prog and returns
; a list of 16-bit lists, one for each line of the program, giving
; the machine language version of the program.

;************************************************************
; Here is a useful table of the TC-201 opcodes and their
; corresponding 4-bit representations.

(define opcode-table
  (list
   (entry 'halt '(0 0 0 0))
   (entry 'load '(0 0 0 1))
   (entry 'store '(0 0 1 0))
   (entry 'add '(0 0 1 1))
   (entry 'sub '(0 1 0 0))
   (entry 'input '(0 1 0 1))
   (entry 'output '(0 1 1 0))
   (entry 'jump '(0 1 1 1))
   (entry 'skipzero '(1 0 0 0))
   (entry 'skippos '(1 0 0 1))
   (entry 'skiperr '(1 0 1 0))
   (entry 'loadi '(1 0 1 1))
   (entry 'storei '(1 1 0 0))))
;************************************************************

(define (assemble-one line table)
  "not done yet")

(define (assemble prog)
  "not done yet")

 ;************************************************************
; Now that we can produce machine language from symbolic assembly-language
; programs, we'll create a simulator that can execute the machine
; language instructions step by step.  First, we specify a representation of
; of the random access memory (RAM) and procedures to read
; and write it.

;************************************************************
; Random access memory (RAM)

; The contents of RAM are represented by a table
; in which the key is a nonnegative integer in the range
; 0 through 4095 (the memory address), and the value is a list of 16 bits
; (the bits stored by the register with that address.)
; No address may appear twice.  The contents of any register
; whose address does not appear as a key is assumed to contain 16 zeroes.

;************************************************************
; ** problem 4 ** (10 points)
; Write three procedures

; (ram-read address ram)
; takes a memory address and a ram
; and returns a list of 16 bits giving the contents
; of the memory register in ram with the given address.

; (ram-write address contents ram)
; takes a memory address (address), a list of 16 bits (contents) and a ram,
; and returns a ram representing the result of copying the contents 
; into the memory register of ram specified by the memory address.

; (equal-rams? ram1 ram2)
; takes two rams and compares their contents, returning
; #t if they are equal and #f if they are unequal.

; Examples
(define ram1
  (list
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))))

(define ram2
  (list
   (entry 1 '(0 0 1 0  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 4 '(0 0 0 1  0 0 1 1  0 1 1 1  1 1 1 1))
   (entry 0 '(0 0 0 1  0 0 0 0  0 0 0 0  0 0 1 1))
   (entry 3 '(1 1 1 1  0 0 0 0  1 1 1 1  0 0 0 0))))

(define ram3
  (list
   (entry 0 '(1 0 1 1  0 0 0 0  0 0 0 0  0 1 0 0))
   (entry 1 '(1 1 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
   (entry 2 '(0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0))
   (entry 4 '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 1 1))
   (entry 5 '(0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0))
   (entry 7 '(1 1 1 1  1 1 1 1  0 0 0 0  0 0 0 0))
   (entry 10 '(1 0 1 0  0 0 0 0  0 1 0 1  1 1 1 1))))

;> (ram-read 0 ram1)
;'(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1)
;> (ram-read 2 ram2)
;'(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
;> (ram-write 5 '(1 1 0 0  0 0 1 1  1 1 0 0  0 0 1 1) ram2)
;(list
; (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
; (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
; (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
; (entry 5 '(1 1 0 0 0 0 1 1 1 1 0 0 0 0 1 1)))
;> (ram-write 10 '(1 1 1 1  1 1 1 1  1 1 1 1  1 1 1 1) ram3)
;(list
; (entry 0 '(1 0 1 1 0 0 0 0 0 0 0 0 0 1 0 0))
; (entry 1 '(1 1 0 0 0 0 0 0 0 0 0 0 0 1 0 1))
; (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
; (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))
; (entry 5 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
; (entry 7 '(1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0))
; (entry 10 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
;> (equal-rams? ram1 ram2)
;#t
;> (equal-rams? ram2 ram3)
;#f

;************************************************************

(define (ram-read address ram)
  "not done yet")

(define (ram-write address contents ram)
  "not done yet")

(define (equal-rams? ram1 ram2)
  "not done yet")

;************************************************************
; For the TC-201 Central Processing Unit (CPU), 
; the contents of the registers are represented by a struct with 4 fields 
; giving the values of the CPU registers:

; the accumulator (acc)
; the program counter (pc)
; the run flag (rf)
; the arithmetic error bit (aeb)

(struct cpu (acc pc rf aeb) #:transparent)

; Each field contains a list of bits of the correct length
; giving the value of the corresponding register; 16 bits for
; the acc, 12 bits for the pc, 1 bit each for the rf and the aeb.
; The constructor is cpu, the type predicate is cpu?, and
; the selectors are cpu-acc, cpu-pc, cpu-rf, cpu-aeb.

; Examples

; The accumulator has value 15, the program counter has value 7,
; the run flag is 1 and the arithmetic error bit is 0.
(define cpu1 
  (cpu
   '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(0)))

; The accumulator has value -3, the program counter has value 7,
; the run flag is 1 and the arithmetic error bit is 1.
(define cpu2 
  (cpu
   '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1)
   '(0 0 0 0 0 0 0 0 0 1 1 1)
   '(1)
   '(1)))

;************************************************************
; A configuration of the TC-201 is a struct with two fields:
; (1) the contents of the CPU registers, in the above format, and
; (2) the contents of the RAM, in the format of problem 4.

(struct conf (cpu ram) #:transparent)

; Note that the constructor is conf, the type-predicate
; is conf?, and the selectors are conf-cpu, conf-ram.

;************************************************************
; ** problem 5 ** (10 points)
; Write three procedures

; (equal-configs? config1 config2)
; takes two configurations config1 and config2, and returns
; #t if they represent the same contents of the RAM and the CPU registers,
; and returns #f otherwise.

; (addr->pc addr config)
; takes a configuration and a memory address addr (a number
; in the range 0 to 4095 inclusive), and returns a new configuration
; in which the program counter is set to the given address.
; No other registers are changed.

; (add-to-pc n config)
; takes a nonnegative integer n and a TC-201 configuration config
; and returns the TC-201 configuration that is obtained by adding n 
; to the value of pc.  Note that the sum should be taken modulo 4096.  
; (Racket has a modulo procedure.)

; Example configurations

(define config1
  (conf cpu1 ram1))

(define config2
  (conf cpu1 ram2))

(define config3
  (conf cpu2 ram2))

; Examples of procedures

;> (equal-configs? config1 config2)
;#t
;> (equal-configs? config2 config3)
;#f
;> (addr->pc 5 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (addr->pc 1 config3)
;(conf
; (cpu '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (add-to-pc 1 config2)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 1 0 0 0) '(1) '(0))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;> (add-to-pc 4093 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))

;************************************************************

(define (equal-configs? config1 config2)
  "not done yet")

(define (addr->pc addr config)
  "not done yet")

(define add-to-pc
  "not done yet")

;************************************************************
; ** problem 6 ** (10 points)
; Write two procedures

; (acc->mem addr config)
; takes a memory address and a configuration, and
; returns the configuration in which the contents of the accumulator
; are copied to the addressed memory register.
; No other registers change value.

; (mem->acc addr config)
; that takes a memory address and a configuration, and
; returns the configuration in which the contents of the addressed
; memory register are copied to the accumulator.
; No other registers change value.

; Examples
;> (acc->mem 3 config1)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (acc->mem 13 config3)
;(conf
; (cpu '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 13 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))))
;> (mem->acc 4 config1)
;(conf
; (cpu '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 2 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))))
;> (mem->acc 12 config3)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 1 1 1) '(1) '(1))
; (list
;  (entry 1 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 4 '(0 0 0 1 0 0 1 1 0 1 1 1 1 1 1 1))
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 1))
;  (entry 3 '(1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0))))
;************************************************************

(define (acc->mem addr config)
  "not done yet")

(define (mem->acc addr config)
  "not done yet")

;************************************************************
; ** problem 7 ** (10 points)
; Write two procedures

; (sum tc1 tc2)
; takes two lists of bits, tc1 and tc2, of the same length, k,
; representing two numbers in k-bit two's complement,
; and returns two values, in a list.
; If the sum of the two numbers can be correctly represented in k-bit
; two's complement, then the first value in the list is #t and the second
; value is the k-bit two's complement representation of the sum.
; If the sum of the two numbers cannot be correctly represented in k-bit
; two's complement, then the first value in the list is #f and the
; second value is a list of k zeroes.

; (diff tc1 tc2)
; is analogous to (sum tc1 tc2), except that, instead of the
; sum of the numbers represented by tc1 and tc2, the value
; computed is their difference, that is, the number represented
; by tc1 minus the number represented by tc2.
; The format of the result is the same: a list with #t and the
; k-bit two's complement representation of the difference, or
; #f and a list of k zeroes.

; For both procedures, you may assume that tc1 and tc2 are 
; lists of bits of equal length, and that the length is at least 2.

; Examples
; 3 + 2 = 5, correctly representable in two's complement with 4 bits
;>  (sum '(0 0 1 1) '(0 0 1 0))
;'(#t (0 1 0 1))

; -3 + 2 = -1, correctly representable in two's complement with 4 bits
;> (sum '(1 1 0 1) '(0 0 1 0))
;'(#t (1 1 1 1))

; -5 + 5 = 0, correctly representable in two's complement with 16 bits
;> (sum '(1 1 1 1  1 1 1 1  1 1 1 1  1 0 1 1) '(0 0 0 0  0 0 0 0  0 0 0 0  0 1 0 1))
;'(#t (0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))

; 4 + 4 = 8, which is not correctly representable in two's complement with 4 bits
;> (sum '(0 1 0 0) '(0 1 0 0))
;'(#f (0 0 0 0))

; 3 - 2 = 1, correctly representable in two's complement with 4 bits
;>  (diff '(0 0 1 1) '(0 0 1 0))
;'(#t (0 0 0 1))

; 3 - 5 = -2, correctly representable in two's complement with 4 bits
;>  (diff '(0 0 1 1) '(0 1 0 1))
;'(#t (1 1 1 0))

; 3 - (-6) = 9, which is not correctly representable in two's complement with 4 bits
;> (diff '(0 0 1 1) '(1 0 1 0))
;'(#f (0 0 0 0))
;************************************************************

(define (sum tc1 tc2)
  "not done yet")

(define (diff tc1 tc2)
  "not done yet")

;************************************************************
; ** problem 8 ** (10 points)
; Write two procedures

; (do-input config)
; (do-output config)

; Each takes a TC-201 configuration and performs the appropriate action 
; (reading a number from the user or writing a number out to the user)
; AND ALSO **RETURNS** THE RESULTING TC-201 CONFIGURATION.
;
; For input, the new configuration has the value read in the 
; accumulator, and all other registers unchanged.
; To read in a value, you may use the following let construct:

; (let ((value (begin (display "input = ") (read)))) ...)

; If the number provided by the user is not representable
; in two's complement with 16 bits, the returned value should
; be the symbol 'error instead of a new configuration.

; For output, the new configuration is returned UNCHANGED. 
; If the integer value from the accumulator is in 
; value-from-accumulator, then the output to the user can be 
; produced by:

; (display "output = ")
; (display value-from-accumulator)
; (newline)

; Examples

; The lines input = .. and output = .. show the interaction between 
; TC-201 and user.  The TC-201 configuration shows the value
; returned by the procedure.  This assumes init-config is working.

;> (do-input (init-config '()))
;input = 14
;(conf (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())
;> (do-output (do-input (init-config '())))
;input = -34
;output = -34
;(conf (cpu '(1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0)) '())

;************************************************************

(define (do-input config)
  "not done yet")

(define (do-output config)
  "not done yet")

;************************************************************
; ** problem 9 ** (10 points)
; Write one procedure

; (next-config config)

; that takes a TC-201 configuration and returns the next TC-201 configuration,
; after one iteration of the fetch/execute cycle.

; If the run flag (rf) is 0, then the configuration config is returned unchanged,
; because the machine is halted.

; The instructions that should be implemented are:

; halt, load, store, add, sub, input, output, jump
; skipzero, skippos, skiperr, loadi, storei.

; These are opcodes 0000 through 1100, respectively.
; You should intepret an undefined opcode  (1101 through 1111) 
; as a halt instruction.

; For a halt instruction, in the returned configuration 
; the run flag is 0 and all other registers are unchanged.

; Otherwise, the program counter (pc) contains a memory address, and the TC-201 
; instruction at that location is fetched and executed, and the resulting 
; configuration is returned.  Note that all instructions result in a configuration
; being returned, INCLUDING input and output.
;************************************************************

(define (next-config config)
  "not done yet")

;************************************************************
; ** problem 10 ** (10 points)
; Write three procedures

; (init-config lst)
; takes a list lst 16 bit patterns, and returns a TC-201 configuration 
; in which those patterns are loaded into RAM starting with address 0, 
; and the CPU registers are initialized so that the accumulator has
; 16 zeroes, the program counter has 12 zeroes, the run flag has 
; value 1, and the arithmetic error bit has value 0.

; (simulate steps config)
; takes a number of steps and a configuration config of the TC-201
; and simulates (using next-config) the machine until the machine
; halts (that is, the run flag is 0) or the given number of steps
; have been executed, whichever occurs first.  The list
; of successive configurations reached, starting from config, is returned.

; (run steps prog)
; takes a number of steps and a symbolic assembly-language program
; prog, and assembles the program (using assemble), and loads it
; into memory (using init-config) and runs it until either it
; halts or has run for the given number of steps (using simulate).

; Examples

(define patterns
  '((0 0 0 1  0 0 0 0  0 0 0 0  0 1 0 0)
    (0 0 1 1  0 0 0 0  0 0 0 0  0 1 0 1)
    (0 0 1 0  0 0 0 0  0 0 0 0  0 1 1 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  0 0 0 0)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 1 0)
    (1 1 1 1  1 1 1 1  1 1 1 1  1 1 0 1)
    (0 0 0 0  0 0 0 0  0 0 0 0  1 0 0 0)))

;> (init-config patterns)
;(conf
; (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
; (list
;  (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;  (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;  (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;  (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;  (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;  (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;  (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))

;> (simulate 5 (init-config patterns))
;(list
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0) '(0 0 0 0 0 0 0 0 0 0 0 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0) '(0 0 0 0 0 0 0 0 0 0 0 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 0) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(1) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1))))
; (conf
;  (cpu '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1) '(0 0 0 0 0 0 0 0 0 0 1 1) '(0) '(0))
;  (list
;   (entry 0 '(0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 0))
;   (entry 1 '(0 0 1 1 0 0 0 0 0 0 0 0 0 1 0 1))
;   (entry 2 '(0 0 1 0 0 0 0 0 0 0 0 0 0 1 1 0))
;   (entry 3 '(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
;   (entry 4 '(0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0))
;   (entry 5 '(1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1))
;   (entry 6 '(0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1)))))

;> (define configs (run 200 prog-sum))
;input = 3
;input = -14
;input = 55
;input = 0
;output = 44
;************************************************************

(define (init-config lst)
  "not done yet")

(define simulate
  "not done yet")

(define (run steps prog)
  "not done yet")

;************************************************************
; ** problem 11 ** (10 points)
; Write two programs for the TC-201, in the
; format required by assemble.

; prog-sort-two
; reads in two numbers from the user and
; prints them out again, with the smaller of
; the two printed first, and halts.

; prog-reverse
; reads in a zero-terminated sequence of numbers from
; the user, and then prints the numbers out in the reverse
; order from which they were input (not including the final 0),
; and halts.

; Note that you can do this problem even if your simulator
; is not yet working.  Your programs will be tested with
; the reference simulator.

; Examples (showing the user interaction, not the configurations returned.)

;> (define configs (run 200 prog-sort-two))
;input = 13
;input = 6
;output = 6
;output = 13

;> (define configs (run 200 prog-sort-two))
;input = -11
;input = -1
;output = -11
;output= -1

;> (define configs (run 200 prog-sort-two))
;input = 32767
;input = -32768
;output = -32768
;output = 32767

;> (define configs (run 200 prog-reverse))
;input = 7
;input = 2
;input = 15
;input = -88
;input = 0
;output = -88
;output = 15
;output = 2
;output = 7
;> 
;************************************************************

(define prog-sort-two
  "not done yet")
               
(define prog-reverse
  "not done yet")

;********************** end of hw6.scm **********************
