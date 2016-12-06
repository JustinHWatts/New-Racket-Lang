#lang racket

#| To use functions provided here from another file,
   put this file into the same directory,
   and in that file add `(require "student-extras.rkt")`.

   Note that you should *not* cut/paste this into a blank beginner-student tab;
   just download this file and put it into the same directory.
 |#


; See full racket-documentation for these functions/keywords:
;
(provide provide
         require
         all-defined-out
         play-sound
         file-exists?
         let
         let*
         letrec
         module+
         struct     ; example below.
         exn?
         exn:fail?
         
         ; There are more regexp functions in racket/base
         ; We only re-export the most common ones.
         regexp
         pregexp
         regexp-match
         regexp-match*
         regexp-match?
         regexp-split
         regexp-replace
         regexp-replace*
         regexp-quote

         string-downcase
         string-upcase
         )

; (The student languages don't allow `only-in`,
; and requiring the entire library gives a conflict.)
;
(require (only-in racket/gui play-sound))


#| 
; `struct` is like `define-struct` but allows inheritance:
; When using this, include args `#:transparent` and possibly `#:consructor-name make-...` ).
; WARNING: in student-level, (struct foo (...)) objects always print as `make-foo` even though
;    the actual constructor-name is just `foo`.


; Usage example:
(require "student-extras.rkt")

(struct soup (a b) #:transparent)
(define make-soup soup) ; We can bind the traditional name to the *actual* constructor.

(make-soup 2 3)

(struct hotsoup soup (c d e) #:transparent)  ; inherit `soup`s fields.
(hotsoup 2 3 4 5 6) ; N.B. this prints as `make-hotsoup`, in beginner-student.

|#
