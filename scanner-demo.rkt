#lang racket
(require "scanner.rkt")  ;; N.B. scanner.rkt must be in same dir as this file, to run.

;; To use scanner.rkt for reading input:
;; Think of the input as a stack of tokens.
;;
;;   create-scanner : -> scanner      Returns a scanner reading from stdin.
;;   create-scanner : string -> scanner   Returns a scanner reading from the literal string.
;;
;;   pop! : scanner -> (union string number eof-object)  returns the next item and pops the stack;
;;   peek : scanner -> (union string number eof-object)  returns the next item but doesn't pop the stack.
;; rather than methods like "hasNextInt()" in Java, we use:
;;   (integer? (peek))
;;
;; Note that currently, *only* strings, numbers, and end-of-file are returned.
;; (You can test for the latter with `eof-object?`.)
;; You can't pop the eof-object (rather, there are infinitely many of them, conceptually.) 
;; Parentheses are considered tokens (and don't need whitespace
;; around them).
;;
;; A less-commonly used features:
;;   create-scanner : port -> scanner      Returns a scanner reading from the input-port.
;;   push! : scanner any -> (void) pushes the next token back on the 
;; very obscure:
;;   input-ready? : scanner -> boolean; returns true if peek/pop won't block.
;;   NOTE: eof isn't considered blocking (since peek/pop will return eof.
;;
;; Here is an example demonstrating all four methods (which is full of
;; 'display' and 'print' only because its interactive:)

(define s1 (create-scanner))  ; Reads from standard input
(define s2 (create-scanner "hello there := (3 *4 +(5[[[")) ; reads from this particular string

(define (demo-scanner s)
  (begin
    (unless (input-ready? s) (display "Enter any text:\n"))
    (cond [(number? (peek s)) (print (string-append "handling a number: "
                                                    (number->string (pop! s))))]
          [(string? (peek s)) (print (string-append "handling a string: " 
                                                    (pop! s)))]
          [(eof-object? (peek s)) (print "At end-of-input")]
          [else (print (string-append "Impossible?!?!?  some other value: " (pop! s)))])
    (display "\n")
    (when (and (input-ready? s) (not (eof-object? (peek s)))) 
      (demo-scanner s)) ;; loop.
    ))


(demo-scanner s2)


(push! s1 "3 + (4* 5))))])")
(demo-scanner s1)

;; Now it's your turn:
(demo-scanner s1)
