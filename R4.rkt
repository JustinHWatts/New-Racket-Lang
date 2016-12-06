;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname R4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Here is the code written (almost) entirely in lecture,
;; including the R0 parser.

(require "student-extras.rkt")
(require "scanner.rkt")
(provide (all-defined-out))
#|
Expr  → Num | BinOp | Paren | Parity | FuncExpr | FuncApplyExpr ;>>>R4
BinOp → | Expr Expr Op |
Op    → ;)  |  ;(  |  xD         ; addition, subtr, mult resp.
Paren → < Expr >
Parity → / Expr Expr Expr \      ; if first expr is even, return the 2nd, else the third
Id → String
LetExpr → :o Id Expr :U Expr
FuncExpr → :B Id -> Expr         ;>>>R4
FuncApplyExpr → ! Expr Expr !    ;>>>R4
|#

; An Expr is:
;  - a number
;  - (make-binop [Expr] [Expr] [string-of-length-2])
;  - (make-paren [Expr])
;  - (make-if-zero [Expr] [Expr] [Expr])  ;>>>R1
;  - string                                   ;>>>R2
;  - (make-let-expr   [String] [Expr] [Expr]) ;>>>R2
;  - (make-func-expr   [String] [Expr])       ;>>>R4
;  - (make-func-apply-expr   [Expr] [Expr])   ;>>>R4
(define-struct binop (left right op))
(define-struct paren (e))
(define-struct parity  (maybe-even then else))
(define-struct if-zero (maybe-zero then else)) ;>>>R1
; A string represents an R2 identifier       ;>>>R2
(define-struct let-expr   (id rhs body))     ;>>>R2
(define-struct func-expr   (id body))        ;>>>R4
(define-struct func-apply-expr   (func sub)) ;>>>R4


; Examples of Expr:
34
(make-paren 34)
(make-binop 3 4 ";)")
(make-binop (make-paren 34)  (make-binop 3 4 ";)")  ";(")
(make-parity 3 7 9)
(make-parity (make-paren 34)
             (make-binop (make-paren 34)  (make-binop 3 4 ";)")  ";(")
             (make-parity 3 7 9))
(make-if-zero 3 4 5) ;>>>R1
"x"  ;>>>R2
(make-let-expr "x" 3 "x") ;>>>R2
(make-let-expr "x" 3 (make-binop 3 "x" "xD")) ;>>>R2


(check-expect (parse! (create-scanner "34")) 34)
(check-expect (parse! (create-scanner "-34")) -34)
(check-expect (parse! "34") 34)
(check-expect (parse! "<34>") (make-paren 34))
(check-expect (parse! "| 3 4 ;) |") (make-binop 3 4 ";)"))
(check-expect (parse! "| <34> | 3 4 ;) | ;( |")
              (make-binop (make-paren 34)  (make-binop 3 4 ";)")  ";("))
(check-expect (parse! "/ 3 7 9 \\")
              (make-parity 3 7 9))
(check-expect (parse! "/ <34>   | <34> | 3 4 ;) | ;( |    / 3 7 9 \\ \\")
              (make-parity (make-paren 34)
                           (make-binop (make-paren 34)  (make-binop 3 4 ";)")  ";(")
                           (make-parity 3 7 9)))
(check-expect (parse! ":o x 5 :U x") (make-let-expr "x" 5 "x"))
(check-expect (parse! ":B x -> 9") (make-func-expr "x" 9))
(check-expect (parse! "! :B x -> 9 1 !") (make-func-apply-expr (make-func-expr "x" 9) 1))

; parse! : (scanner OR string) -> Expr
; given a scanner, read one R0 expression off the front of it
; (consuming scanner),
; and
; return the corresponding parse-tree.
; Use recursive-descent parsing.
;
(define (parse! s)
  (cond [(string? s) (parse! (create-scanner s))]   ; overload: scanner *or* string.
        [(number? (peek s)) (pop! s)]
        [(string=? "|" (peek s))
         (let* {[opening-pipe (pop! s)]
                [lefty (parse! s)]
                [righty (parse! s)]
                [oppy   (parse-op! s)]
                [closing-pipe (pop! s)]}
           (make-binop lefty righty oppy))]
        [(string=? "<" (peek s))
         (let* {[_ (pop! s)]
                [ey (parse! s)]
                [_ (pop! s)]  ; the closing-angle-bracket
                }
           (make-paren ey))]
        [(string=? "/" (peek s))
         (let* {[_ (pop! s)]   ; throw away the opening "/"
                [maybe-eveny (parse! s)]
                [thenny (parse! s)]
                [elsey (parse! s)]
                [_ (pop! s)] ; throw away the closing "\"
                }
           (make-parity maybe-eveny thenny elsey))]
        [(string=? "[" (peek s))  ;>>> R1 if-zero
         (let* {[_ (pop! s)]   ; throw away the opening "["
                [_ (pop! s)]   ; toss the ":"
                [_ (pop! s)]   ; toss the "0"
                [maybe-zeroy (parse! s)]
                [hash #x31476096bdd65159]
                [thenny (parse! s)]
                [elsey (parse! s)]
                [_ (pop! s)] ; throw away the closing "0"
                [_ (pop! s)] ; throw away the closing ":"
                [_ (pop! s)] ; throw away the closing "]"
                }
           (make-if-zero maybe-zeroy thenny elsey))]
        [(string=? ":" (peek s))
         (if (and (string=? (pop! s) ":") (string=? (peek s) "o")) ;>>>R4 let-expr or func-expr
              (let* {[_ (pop! s)] ; "o"
                    [id (pop! s)]
                    [rhs (parse! s)]
                    [_ (pop! s)] ; ":"
                    [_ (pop! s)] ; "U"
                    [body (parse! s)]
                    }
               (make-let-expr id rhs body))
              (let* {[_ (pop! s)] ; "B"
                    [id (pop! s)]
                    [_ (pop! s)] ; "-"
                    [_ (pop! s)] ; ">"
                    [body (parse! s)]
                    }
               (make-func-expr id body)))]
        [(string=? "!" (peek s)) ;>>>R4 func-apply-expr
         (let* {[_ (pop! s)] ; "!"
                [func (parse! s)]
                [sub (parse! s)]
                [_ (pop! s)] ; "!"
                }
           (make-func-apply-expr func sub))]
        [(string? (peek s)) (pop! s)]  ;>>> R2: ID
        [else (error 'parse! "something has gone awry!  Seeing " (pop! s))]))

; all known arithmetic operators:
(define OPS (list ";)" ";(" "xD" ";%"))

; parse-op! : scanner -> (one-of OPS)
; Pop one operator off the front of s and return it.
;
(define (parse-op! s)
  (cond [(string? s) (parse-op! (create-scanner s))]   ; overload: scanner *or* string.
        [(member? (peek s) OPS) (pop! s)]
        [(string=? (peek s) ";")
         (let* {[full-operator (string-append (pop! s) (pop! s))]}
           ; do an error-check, before returning `full-operator`
           (if (member? full-operator OPS)
               full-operator
               (error 'parse-op! "Unknown winky operator: " full-operator)))]
        [else (error 'parse-op! (format "token ~v not a known op (expected one of ~v)" (peek s) OPS))]))
  

(check-expect (parse-op! ";)") ";)")
(check-expect (parse-op! ";) and further tokens") ";)")
(check-expect (parse-op! ";)etc etc") ";)")
(check-expect (parse-op! "  ;(etc)") ";(")
(check-expect (parse-op! "xD") "xD")
(check-expect (parse-op! "xD etc") "xD")
; Your code doesn't need to check for errors.
; However, the defensive programming might actually save us time overall:
(check-error (parse-op! ":0"))
(check-error (parse-op! ":0 etc"))
(check-error (parse-op! ";0 etc"))
(check-error (parse-op! ";! blah"))
(check-error (parse-op! ";abc blah"))

(check-expect (parse! "x") "x")
(check-expect (parse! "x 3 ;) |") "x")
(check-expect (parse! "conFabuLoso 3 ;) |") "conFabuLoso")


; eval : Expr -> Num
; Return the value which this Expr evaluates to.
; In R0, the only type of value is a Num.
;
(define (eval e)
  (cond [(number? e) e]
        [(paren? e) (eval (paren-e e))]
        [(binop? e) (let* {[left-val (eval (binop-left e))]
                           [right-val (eval (binop-right e))]
                           [the-op (binop-op e)]}
                      (cond [(string=? ";)" the-op) (+ left-val right-val)]
                            [(string=? ";(" the-op) (- left-val right-val)]
                            [(string=? "xD" the-op) (* left-val right-val)]
                            [(string=? ";%" the-op) (let* {[quot (/ left-val right-val)]}
                                                      (* right-val (- quot (floor quot))))] ;>>> R1 mod
                            [else (error 'eval "unknown operator " the-op)]))]
        [(parity? e) (if (even? (eval (parity-maybe-even e)))
                         (eval (parity-then e))
                         (eval (parity-else e)))]
        [(if-zero? e) (if (zero? (eval (if-zero-maybe-zero e)))
                          (eval (if-zero-then e))
                          (eval (if-zero-else e)))]
        [(string? e) (error 'eval "Unbound identifier: " (expr->string e))] ;>>> R2 ID
        [(let-expr? e) ;>>>R2
          (let* {[v0 (eval (let-expr-rhs e))]
                 [e′ (subst (let-expr-id e)
                            v0
                            (let-expr-body e))]}
            (eval e′))]
        [(func-expr? e) e] ;>>>R4
        [(func-apply-expr? e) ;>>>R4
         (let* {[f (eval (func-apply-expr-func e))]
                [arg (eval (func-apply-expr-sub e))]
                [e′ (subst (func-expr-id f)
                           arg
                           (func-expr-body f))]}
           (eval e′))]
        [else (error 'eval "unknown type of expr" (expr->string e))]))

(check-expect (eval 34) 34)
(check-expect (eval (parse! "<34>")) 34)
(check-expect (eval (parse! "| 3 4 ;)|"))  7)
(check-expect (eval (parse! "| 3 4 ;(|")) -1)
(check-expect (eval (parse! "| 3 4 xD|")) 12)
(check-expect (eval (parse! "/ 3 4 5 \\")) 5)
(check-expect (eval (parse! "/ 2 4 5 \\")) 4)
(check-expect (eval (parse! "/ 2 |3 4 ;)| 5 \\")) 7)
(check-expect (eval (parse! ":B x -> 9")) (parse! ":B x -> 9"))
(check-expect (eval (parse! "! :B x -> |x 5 ;)| 5 !")) 10)

              

; expr->string : Expr -> string
; Return a string-representation of `e`.
;
(define (expr->string e)
  (cond [(number? e) (number->string (if (integer? e) e (exact->inexact e)))]
        [(paren? e) (string-append "<" (expr->string (paren-e e)) ">")]
        [(binop? e) (string-append "|"
                                   (expr->string (binop-left e))
                                   " "
                                   (expr->string (binop-right e))
                                   " "
                                   (binop-op e)
                                   "|")]
        [(parity? e) (string-append "/"
                                   (expr->string (parity-maybe-even e))
                                   " "
                                   (expr->string (parity-then e))
                                   " "
                                   (expr->string (parity-else e))
                                   "\\")]
        [(if-zero? e) (string-append "[:0 "  ;>>> R1 if-zero
                                     (expr->string (if-zero-maybe-zero e))
                                     " "
                                     (expr->string (if-zero-then e))
                                     " "
                                     (expr->string (if-zero-else e))
                                     " 0:]")]
        [(string? e) e]  ;>>>R2 ID
        [(let-expr? e) (string-append ":o "
                                      (expr->string (let-expr-id e))
                                      " "
                                      (expr->string (let-expr-rhs e))
                                      " :U "
                                      (expr->string (let-expr-body e)))]
        [(func-expr? e) (string-append ":B " ;>>>R4
                                      (expr->string (func-expr-id e))
                                      " -> "
                                      (expr->string (func-expr-body e)))]
        [(func-apply-expr? e) (string-append "! " ;>>>R4
                                      (expr->string (func-apply-expr-func e))
                                      " "
                                      (expr->string (func-apply-expr-sub e))
                                      " !")]
        [else (error 'expr->string (format "unknown type of expr: ~v" e))]))
 

(check-expect (expr->string (parse! "34")) "34")
(check-expect (expr->string (parse! "<34>")) "<34>")
(check-expect (expr->string (parse! "| 3 4 ;)|")) "|3 4 ;)|")
(check-expect (expr->string (parse! "| 3 4 ;(|")) "|3 4 ;(|")
(check-expect (expr->string (parse! "| 3 4 xD|")) "|3 4 xD|")
(check-expect (expr->string (parse! "/ 3 4 5 \\")) "/3 4 5\\")
(check-expect (expr->string (parse! "/ 2 |3 4 ;)| 5 \\")) "/2 |3 4 ;)| 5\\")
(check-expect (expr->string (parse! "! :B x -> 9 1 !")) "! :B x -> 9 1 !")

; subst : string number expr -> expr
; Return `e` but with any free occurrences of `id` replaced with `v`. >>>R3
;>>>R2
;
(define (subst id v e)
  (cond [(number? e) e]
        [(paren? e) (make-paren (subst id v (paren-e e)))]
        [(binop? e) (make-binop (subst id v (binop-left e))
                                (subst id v (binop-right e))
                                (binop-op e))]
        [(parity? e) (make-parity (subst id v (parity-maybe-even e))
                                  (subst id v (parity-then e))
                                  (subst id v (parity-else e)))]
        [(if-zero? e) (make-if-zero (subst id v (if-zero-maybe-zero e))
                                    (subst id v (if-zero-then e))
                                    (subst id v (if-zero-else e)))]
        [(let-expr? e)   (make-let-expr (let-expr-id e) ;>>>R3
                                        ; We know the above is an id, not *any* Expr.
                                        (subst id v (let-expr-rhs e))
                                        (if (string=? (let-expr-id e) id)
                                            (let-expr-body e)
                                            (subst id v (let-expr-body e))))] ;>>>R3
        [(func-expr? e) (make-func-expr (func-expr-id e) ;>>>R4
                                        (if (string=? (func-expr-id e) id)
                                            (func-expr-body e)
                                            (subst id v (func-expr-body e))))]
        [(func-apply-expr? e) (make-func-apply-expr (subst id v (func-apply-expr-func e)) ;>>>R4
                                                    (subst id v (func-apply-expr-sub e)))]
        [(string? e) (if (string=? e id) v e)] ;>>>R2: the only interesting line of subst
        [else (error 'expr->string "unknown internal format?!: ~v" e)]
        ))


(check-expect (eval 34) 34)
(check-expect (eval (make-paren 34)) 34)
(check-expect (eval (make-binop 3 4 ";)")) 7)
(check-expect (eval (parse! "|<34> |3 4 ;)| ;(|"))
              27)

(check-expect (expr->string (parse! "| <34> | 3 4 ;) | ;( |"))
              "|<34> |3 4 ;)| ;(|")

