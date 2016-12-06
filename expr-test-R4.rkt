;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname expr-test-R4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "R4.rkt")
(require "scanner.rkt")
(require rackunit) ; use `check-equal?`, not `check-expect`.
;
; We use `check-equal?` because it can be passed as a function (e.g. to `map`).
; The one advantage of `check-expect` we lose, is that all check-expects are
; delayed until the end of the file (hence, you can have a `check-expect`
; *before* the function it tests -- not so, with `check-equal?`.)

;;;;;;;;;;;;;;;;;;; TEST CASES: R0 ;;;;;;;;;;;;;;;;


; Some expressions to test in a non-automated way:
(define e0 "43")
(define e1 "<43>")
(define e2 "|4 3 ;)|")
(define e3 "<< < |4 3 ;)|>>>")
(define e4 "|<43>    | 42 3 xD | ;) |")

(check-equal? (parse! "39") 39)
(check-equal? (eval (parse! e4)) 169)

;;; three types of test we want to make, for many different exprs:
(check-equal? (parse! "|4 3 ;)|") (make-binop 4  3 ";)"))
(check-equal? (eval (parse! "|4 3 ;)|")) 7)
(check-equal? (expr->string (parse! "|4 3 ;)|"))
              "|4 3 ;)|")

(define prog2 ":o x 5 :U |4 x xD|") ;
(check-equal? (parse! prog2)   (make-let-expr "x" 5 (make-binop 4 "x" "xD"))  )
(check-equal? (expr->string (parse! prog2))   prog2   )
(check-equal? (eval (parse! prog2))    20   )

; Make an additional example, where the `:o` is *not* the top-level expression:
; Then, have the three tests for it, as above.
(define prog3 "|3 :o x 5 :U |4 x xD| xD|")
(check-equal? (parse! prog3) (make-binop 3 (make-let-expr "x" 5 (make-binop 4 "x" "xD")) "xD"))
(check-equal? (expr->string (parse! prog3)) prog3)
(check-equal? (eval (parse! prog3)) 60)

; Your answers may look different, if you chose different data representations!
; For example, if you represent your identifiers as a struct-with-one-field,
; then you might have `(make-id "x")` where I have `"x"` above.


; hw08 prolog-intro Queries (5 and 6.a in prolog_basics1.P file)
; b. chaperone(alice,bob,X).
; c. chaperone(X,Y,dee).


;>>>R3
(define prog4 ":o x 3 :U :o x 5 :U |x 3 ;)|")
(define prog5 ":o y 3 :U :o x 5 :U |x y ;)|")
(define prog6 ":o y 3 :U :o x y :U |x y ;)|")
(define prog7 ":o x 5 :U :o y 3 :U |:o x y :U |x y ;)| x ;)|")
(define prog8 ":o x 5 :U <:o x |x 1 ;)| :U |x 2 ;)|>")
(define prog9 ":o y :o z 4 :U <:o y 99 :U z> :U <:o z 5 :U |<:o z 10 :U y> |y z ;)| ;)|>")

;>>>R4 Question #3
(define const ":B x -> 9") ;constant function
(define square ":B x -> |x x xD|") ;sqr function
(define fact ":B x -> |[:0 |x 1 ;(| 1 'recurse' 0:] x xD|") ;factorial function
(define adder ":B x -> |x :B y -> y ;)|") ;make-adder function

(define const-a "! :B x -> 9 1 !")
(define square-a "! :B x -> |x x xD| :o y 3 :U :o x 5 :U |x y ;)| !")
(define fact-a "! :B x -> |[:0 |x 1 ;(| 1 'recurse' 0:] x xD| !")
(define adder-a "! :B x -> |x ! :B y -> y :o x 5 :U |4 x xD| ! ;)| :o y 3 :U :o x y :U |x y ;)| !")

(define prog10 ":o x 5 :U |x ! :B x -> |x x xD| 4 ! ;)|")
(define prog11 "! :B x -> :o x ! :B x -> x 7 ! :U |! :B x -> |x 16 ;(| 24 ! x ;(| 9 !")
(define prog12 ":o x 5 :U <:o x |! :B x -> |x x xD| 8 ! 1 ;)| :U |! :B x -> |x x xD| 4 ! 2 ;)|>")


; The last paragraph of #2 on hw07 mentions that you'll have to do substitution in a tree.
; Although `substitute` returns a *tree* (an Expr), 
; we can use `parse!` (already tested!) to help us generate our expected-results.
;
(check-equal? (subst "x" 9 (parse! "3"))   (parse!   "3"  ) )
(check-equal? (subst "x" 9 (parse! "x"))   (parse!   "9"  ) )
(check-equal? (subst "z" 7 (parse! "x"))   (parse!   "x"  ) )
(check-equal? (subst "z" 7 (parse! "|4 z ;)|"))   (parse!   "|4 7 ;)|"  ) )
(check-equal? (subst "z" 7 (parse! ":o x z :U |x z xD|"))
              (parse!   ":o x 7 :U |x 7 xD|"  ) )
;>>>R3
(check-equal? (subst "x" 3 (parse! ":o x 5 :U |x 3 ;)|"))
              (parse!   ":o x 5 :U |x 3 ;)|"))
(check-equal? (subst "y" 3 (parse! ":o x 5 :U |x y ;)|"))
              (parse!   ":o x 5 :U |x 3 ;)|"))
(check-equal? (subst "y" 3 (parse! ":o x y :U |x y ;)|"))
              (parse!   ":o x 3 :U |x 3 ;)|"))
(check-equal? (subst "x" 5 (parse! ":o y 3 :U | :o x y :U |x y ;)| x ;)|"))
              (parse!   ":o y 3 :U | :o x y :U |x y ;)| 5 ;)|"))
(check-equal? (subst "x" 5 (parse! "<:o x |x 1 ;)| :U |x 2 ;)|>"))
              (parse!   "<:o x |5 1 ;)| :U |x 2 ;)|>"))
(check-equal? (subst "y"
                     (eval (parse! ":o z 4 :U <:o y 99 :U z>"))
                     (parse! "<:o z 5 :U |<:o z 10 :U y> |y z ;)| ;)|>"))
              (parse!   "<:o z 5 :U |<:o z 10 :U 4> |4 z ;)| ;)|>"))
;>>>R4
(check-equal? (subst "x" 5 (parse! ":B x -> |x x xD|"))
              (parse! ":B x -> |x x xD|"))
(check-equal? (subst "y" 5 (parse! ":B x -> |x y xD|"))
              (parse! ":B x -> |x 5 xD|"))
(check-equal? (subst "x" 5 (parse! "! :B x -> |x x xD| 9 !"))
              (parse! "! :B x -> |x x xD| 9 !"))
(check-equal? (subst "x" 5 (parse! "! :B x -> |x x xD| x !"))
              (parse! "! :B x -> |x x xD| 5 !"))



; Even though we are showing the test-cases in terms of strings (and calling
; parse!), you should be able to think about this in terms of the *trees*,
; since our code needs to work on the *trees* (not strings) to work properly.
; Give at least one more interesting tree, to test `substitute` on,
; with parse-tree of height of 2 or more.
; You do *not* need to do `substitute` on a parse tree containing a `:o` inside of it ... yet.
; (But you are encouraged to start thinking about what you want to happen, in that situation.)


(define tests
  ; Each entry in the list is either
  ; [str val] (where val is the result of interpreting the string str), or
  ; [str val expr] (as above, but expr is the internal (struct) representation).
  `{["7" 7 7]
    ["|3 4 ;)|" 7 ,(make-binop 3 4 ";)")]
    ["|3   4 xD|" 12 ,(make-binop 3 4 "xD")]
    ["||3  4 ;)| |  3   4 xD | ;) |" 19]
    ["/ 0 1 2\\" 1 ,(make-parity 0 1 2)]
    ["/ 1 1 2\\" 2 ,(make-parity 1 1 2)]
    ["/ |3 -3 ;)| 1  2\\" 1 ,(make-parity (make-binop 3 -3 ";)") 1 2)]
    ["/ |/ / 0  1  2 \\ 3  4 \\  -3 ;) |   1  2 \\" 
     2
     ,(make-parity (make-binop (make-parity (make-parity 0 1 2) 3 4) -3 ";)") 1 2)]


    ["[:0 3 4 5 0:]" 5 ,(make-if-zero 3 4 5)]  ;>>> R1 if-zero
    ["[:0 0 4 5 0:]" 4 ,(make-if-zero 0 4 5)]
    ["[:0 |-2 3 ;)|  |-2 4 ;)|   |-2 5 ;)|  0:]"
     3
     ,(make-if-zero (make-binop -2 3 ";)")
                    (make-binop -2 4 ";)")
                    (make-binop -2 5 ";)"))]
    ["[:0 |-2 2 ;)|  |-2 4 ;)|   |-2 5 ;)|  0:]"
     2
     ,(make-if-zero (make-binop -2 2 ";)")
                    (make-binop -2 4 ";)")
                    (make-binop -2 5 ";)"))]
    
    ; Further tests, for R1:
    ["|3.0 4.0 ;% |" 3]
    ["|| 5.0 6.0 ;) | 3.0 ;%|" 2]
    ["|8.1 3.0 ;%|" 2.1]
    ["|8.0 3.1 ;%|" 1.8]
    ["|-8.1 3.0 ;%|" 0.9]
    ["|-8.0 3.1 ;%|" 1.3]
    ["|8.1 -3 ;%|" -0.9]
    ["|8.0 -3.1 ;%|" -1.3]
    ["|-8.1 -3.0 ;%|" -2.1]
    ["|-8.0 -3.1 ;%|" -1.8]
    ["|8.0   2.0 ;%|" 0]
    ["|-8.0  2.0 ;%|" 0]
    ["|8.0  -2.0 ;%|" 0]
    ["|-8.0 -2.0 ;%|" 0]
    ["|8.0   3.0 ;%|" 2]
    ["|-8.0  3.0 ;%|" 1]
    ["|8.0  -3.0 ;%|" -1]
    ["|-8.0 -3.0 ;%|" -2]

    ;>>>R2
    [,prog2 20 ,(make-let-expr "x" 5 (make-binop 4 "x" "xD"))]
    [,prog3 60 ,(make-binop 3 (make-let-expr "x" 5 (make-binop 4 "x" "xD")) "xD")]
    [":o z  |2 3 ;)|  :U  :o y  |4 z xD|  :U  /y  |y z ;)|  z\\"
     25
     ,(make-let-expr "z"
                     (make-binop 2 3 ";)")
                     (make-let-expr "y"
                                    (make-binop 4 "z" "xD")
                                    (make-parity "y" (make-binop "y" "z" ";)") "z")))]
    
    ;>>>R3 Question #1
    ; 1.
    ; a. :o y 3 :U :o x 5 :U |x y ;)| ⇒ :o x 5 :U |x 3 ;)| ⇒ |5 3 ;)| ⇒ 8
    ; b. :o y 3 :U :o x y :U |x y ;)| ⇒ :o x 3 :U |x 3 ;)| ⇒ |3 3 ;)| ⇒ 6
    ; c. :o x 5 :U :o y 3 :U | :o x y :U |x y ;)| x ;)| ⇒ :o y 3 :U | :o x y :U |x y ;)| 5 ;)| ⇒ | :o x 3 :U |x 3 ;)| 5 ;)| ⇒ | |3 3 ;)| 5 ;)| ⇒ |6 5 ;)| ⇒ 11
    ;
    ; d. :o x
    ;       5
    ;    :U <:o x
    ;           |x 1 ;)|
    ;        :U |x 2 ;)|>
    ; Expression d evaluates to 8
    ;
    ; e. :o y
    ;       :o z
    ;          4
    ;       :U <:o y
    ;              99
    ;           :U z>
    ;    :U <:o z
    ;           5
    ;        :U |<:o z
    ;                10
    ;             :U y> |y z ;)| ;)|>
    ; Expression e evaluates to 13

    ;>>>R3 Tests
    [,prog4 8 ,(make-let-expr "x" 3 (make-let-expr "x" 5 (make-binop "x" 3 ";)")))]
    [,prog5 8 ,(make-let-expr "y" 3 (make-let-expr "x" 5 (make-binop "x" "y" ";)")))]
    [,prog6 6 ,(make-let-expr "y" 3 (make-let-expr "x" "y" (make-binop "x" "y" ";)")))]
    [,prog7 11 ,(make-let-expr "x" 5 (make-let-expr "y" 3 (make-binop (make-let-expr "x"
                                                                          "y"
                                                                          (make-binop "x"
                                                                                      "y"
                                                                                      ";)"))
                                                           "x"
                                                           ";)")))]
    [,prog8 8 ,(make-let-expr "x" 5 (make-paren (make-let-expr "x"
                                                               (make-binop "x" 1 ";)")
                                                               (make-binop "x" 2 ";)"))))]
    [,prog9 13 ,(make-let-expr "y" (make-let-expr "z" 4 (make-paren (make-let-expr "y" 99 "z")))
                               (make-paren (make-let-expr "z"
                                                          5
                                                          (make-binop (make-paren (make-let-expr "z" 10 "y"))
                                                                      (make-binop "y" "z" ";)")
                                                                      ";)"))))]

    ;>>>R4 FuncExpr Tests
    [,const ,(make-func-expr "x" 9) ,(make-func-expr "x" 9)]
    [,square ,(make-func-expr "x" (make-binop "x" "x" "xD"))
             ,(make-func-expr "x" (make-binop "x" "x" "xD"))]
    [,adder ,(make-func-expr "x" (make-binop "x" (make-func-expr "y" "y") ";)"))
             ,(make-func-expr "x" (make-binop "x" (make-func-expr "y" "y") ";)"))]

    ;>>>R4 FuncApplyExpr Tests
    [,const-a 9 ,(make-func-apply-expr (make-func-expr "x" 9) 1)]
    [,square-a 64
               ,(make-func-apply-expr (make-func-expr "x" (make-binop "x" "x" "xD"))
                                      (make-let-expr "y" 3 (make-let-expr "x" 5 (make-binop "x" "y" ";)"))))]
    [,adder-a 26
              ,(make-func-apply-expr (make-func-expr "x"
                                                     (make-binop "x"
                                                                 (make-func-apply-expr (make-func-expr "y" "y")
                                                                                       (make-let-expr "x"
                                                                                                      5
                                                                                                      (make-binop 4 "x" "xD")))
                                                                 ";)"))
                                     (make-let-expr "y" 3 (make-let-expr "x" "y" (make-binop "x" "y" ";)"))))]
    [,prog10 21 ,(make-let-expr "x"
                                5
                                (make-binop "x"
                                            (make-func-apply-expr (make-func-expr "x" (make-binop "x" "x" "xD")) 4)
                                            ";)"))]
    [,prog11
     1
     ,(make-func-apply-expr (make-func-expr "x"
                                            (make-let-expr "x"
                                                           (make-func-apply-expr (make-func-expr "x" "x")
                                                                                 7)
                                                           (make-binop (make-func-apply-expr (make-func-expr "x"
                                                                                                             (make-binop "x"
                                                                                                                         16
                                                                                                                         ";("))
                                                                                             24)
                                                                       "x"
                                                                       ";(")))
                            9)]
    [,prog12
     18
     ,(make-let-expr "x"
                    5
                    (make-paren (make-let-expr "x"
                                               (make-binop (make-func-apply-expr (make-func-expr "x"
                                                                                                 (make-binop "x" "x" "xD"))
                                                                                 8)
                                                           1 ";)")
                                               (make-binop (make-func-apply-expr (make-func-expr "x"
                                                                                                 (make-binop "x" "x" "xD"))
                                                                                 4)
                                                           2 ";)"))))]
    })

    
;
; For info on backquote, see documentations and/or:
;   http://www.radford.edu/itec380/2016fall-ibarland/Lectures/backquote.html



; string->tokens : string -> (listof string)
; Given a string, return a list of tokens (of R6).
;
(define (string->tokens str)
  ; Don't use scheme's built-in `read`, because our string might contain semicolons etc.
  ; Make an local helper-function named `convert : scanner -> string`
  (letrec {[convert (λ (scnr)
                       (if (eof-object? (peek scnr))
                           empty
                           (cons (pop! scnr) (convert scnr))))]}
    (convert (create-scanner str))))
        ; N.B. We RELY on left-to-right eval here:
        ; the pop happens before looping back.




; test `eval`:
(for-each (λ (t) (check-equal? (eval (parse! (first t)))
                               (second t)))
          tests)


; Test that expr->string and parse! are inverses:
(for-each (λ (t) (check-equal? (string->tokens (expr->string (parse! (first t))))
                               (string->tokens (first t))))
          tests)


; Test the internal representations:
(for-each (λ (t) (check-equal? (parse! (first t))
                               (third t)))
          (filter (λ(t) (>= (length t) 3))
                  tests))










