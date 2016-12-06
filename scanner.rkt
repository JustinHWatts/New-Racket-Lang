#lang racket

;; TODO:  (define (peek p) (read (peeking-input-port p)))


(read-decimal-as-inexact #f)

;; TODO:
;;  add parse-expect?
;;  Rename "create-scanner" as "string->scanner";
;;  Provide "scanner->string"

(require (lib "pregexp.rkt")
         (lib "13.rkt" "srfi")  ; strings
         (lib "14.rkt" "srfi")  ; char-set
         )

(provide peek
         pop!
         input-ready?
         push! ; Put a token back on the input.
         create-scanner
         )

(define DEFAULT_PUNCTUATION "|/\\;()%:<>[]{}!#$^&~='\"?,#") ; N.B. '.' not punct, since it might occur in the middle of a number.
(define DEFAULT_WHITESPACE  "\n\t ")


(define-struct scanner (port buffer delimiters whitespace)
  #:mutable) 

#| To implement the peekable scanner, we keep a port plus a buffer of what we've
   read-in-advance (usually the next line).
   The buffer will never be just whitespace.  That is,
   the buffer is always one of:
   - eof
   - the empty string
   - a string containing at least one non-white-space token.
   To maintain this, we consume white space immediately after consuming any token
   (and, when re-filling the buffer).

  (Long-term bug: skipping whitespace:
   We should probably consume white space just *before* reading;
   that way if a user calls set-whitespace!, they see their results instantly.
   Since we're not exporting set-whitespace!, that's not currently an issue.)
|#


;; create-scanner: string-> scanner  (reads from the given string)
;; create-scanner: port -> scanner   (reads from the given port)
;; create-scanner: -> scanner        (reads from stdin)
(define (create-scanner . args)
  (let* {[port (cond [(empty? args) (current-input-port)]
                     [(string? (first args)) (open-input-string (first args))]
                     [(input-port? (first args)) (first args)]
                     [else (raise-type-error 'create-scanner "(or/c string? port?)" (first args))])]}
  (make-scanner port
                "" ; Nothing buffered yet.
                (string->char-set DEFAULT_PUNCTUATION)
                (string->char-set DEFAULT_WHITESPACE))))

                
(define (set-delimiters! s str)
  (set-scanner-delimiters! s (string->char-set str)))
(define (set-whitespace! s str)
  (set-scanner-whitespace! s (string->char-set str))
  (skip-white! s))
  ; See note "Long-term bug: skipping whitespace"



;; input-ready? : scanner -> boolean
;; Will 'pop!' or 'peek' block?
;; Note that the buffer being eof counts
;; as non-blocking (since eof will be returned immediately).
;;
(define (input-ready? s)
  (or (eof-object? (scanner-buffer s)) ; we won't block in this case, so #t.
      (not (string=? (scanner-buffer s) ""))))


;; If necessary, read any add'l characters (if present).
;;
(define (refresh-buffer s)
  (when (not (input-ready? s))
    (set-scanner-buffer! s (read-line (scanner-port s)))
    (unless (eof-object? (scanner-buffer s)) (skip-white! s))
    ; Check whether the entire line was whitespace:
    (refresh-buffer s)))

;; peek/help : scanner boolean -> (or/c number string)
;; Return the next token, either consuming it or not.
;;
(define (peek/help s consume?)
  (refresh-buffer s)
  (define buff (scanner-buffer s))
  (cond [(eof-object? buff) 
         (if consume? (error 'peek/help "attempted to pop EOF") eof)]
        [else
         (define pos (string-index buff
                                   (char-set-union (scanner-delimiters s)
                                                   (scanner-whitespace s))))
         (define stop-at (if (number? pos)
                             (max pos 1)
                             (string-length buff)))
         (define token (substring buff 0 stop-at))
         (when consume?
           (set-scanner-buffer! s (substring buff stop-at))
           (skip-white! s))
         (or (string->number token) token)]))
        
;; pop! : scanner -> (or/c number string)
;; Read the next token of input, consuming from the input.
(define (pop! s) (peek/help s true))

;; peek : scanner -> (or/c number string)
;; Read the next token of input, leaving it on the input.
(define (peek s) (peek/help s false))

;;; BUG -- doesn't handle pushing onto EOF, or when " " isn't white.
(define (push! s item)
  (set-scanner-buffer! s (format "~a~a~a" item " " (scanner-buffer s))))

;; skip-white : scanner -> (void)
;; Remove any whitespace at the front of any (buffered) input.
(define (skip-white! s)
  (set-scanner-buffer! s (string-trim (scanner-buffer s) (scanner-whitespace s))))
