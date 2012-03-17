#lang racket

(provide (struct-out IRCmsg))
(provide parse-irc-line to-string to-bytes get-nick)
(provide parser-tests)

(require srfi/1)
(require srfi/13)
(require rackunit)

(struct IRCmsg 
  (prefix command params tail))

(define (starts-with-colon? i)
  (if (null? i)
      #f
      (= (car i) 58)))

(define (next-word line)
  (let-values ([(word rst) (span (lambda (x) (not (= x 32))) line)])
    (if (null? rst)
        (values word '())
        (values word (rest rst)))))

(define (get-prefix line)
  "We start with the full line here."
  (if (starts-with-colon? line)
      (let-values ([(prfx wopf) (next-word line)])
        (values (rest prfx) wopf))      
      (values '() line)))

(define (get-tail line)
  "line should already be stripped of prefix, command and parameters here."
  (if (starts-with-colon? line)
      (rest line)
      '()))

(define (get-parameters line)
  "I assume 'line' has already been stripped of prefix and command here."
  ;(displayln "in get-parameters")
  (define (join l w)
    (if (null? l)
        (append l w)
        (append l (list 32) w)))
  (define (get-next-words ln acc)
    ;(displayln "in get-next-words ")
    (let-values ([(n l) (next-word ln)])
      (cond [(starts-with-colon? n) (values acc ln)]
            [(null? n) (values '() '())]
            [else (get-next-words l (join acc n))])))
  (get-next-words line '()))

(define (is-tail-ctcp? tl)
  (if (null? tl)
      #f
      (= 1 (car tl) (last tl))))

(define (intlist->string x)
  (list->string (map (λ (y) (integer->char y)) x)))

(define (type-of-ctcp c)
  "replacing PRIVMSG with CTCP and NOTICE with CTCPREPLY"
  (cond [(= (car c) 80) '(67 84 67 80)] ;; CTCP
        [(= (car c) 65) '(65 67 84 73 79 78)] ;; ACTION
        [(= (car c) 78) '(67 84 67 80 82 69 80 76 89)])) ;; CTCPREPLY

(define (parse-line line)
  (let*-values ([(pfx rst1) (get-prefix line)]
                [(cmd rst2) (next-word rst1)]
                [(params rst3) (get-parameters rst2)])
    (let ([tail (get-tail rst3)])
      (if (is-tail-ctcp? tail)
          (IRCmsg pfx (type-of-ctcp cmd) params (remove (λ (x) (= x 1)) tail))
          (IRCmsg pfx cmd params tail)))))

(define (parse-irc-line line)
  (let ([b (bytes->list line)])
    (all-to-strings (parse-line b))))

(define (all-to-strings msg)
  (IRCmsg
   (intlist->string (IRCmsg-prefix msg))
   (intlist->string (IRCmsg-command msg))
   (intlist->string (IRCmsg-params msg))
   (intlist->string (IRCmsg-tail msg))))

;(define ctcp-time '(58 68 101 118 78 117 108 108 33 115 101 114 118 105 99 101 115 64 100 121 110 97 115 116 121 110 101 116
;                       46 110 101 116 32 80 82 73 86 77 83 71 32 66 108 97 114 114 32 58 1 84 73 77 69 1))
; this really isn't the best way to define IRC lines in the code. I should be able to do it with normal strings? Then again, the '1' char.

(define (show-irc-msg IRCmsg)
  (displayln (IRCmsg-prefix IRCmsg))
  (displayln (IRCmsg-command IRCmsg))
  (displayln (IRCmsg-params IRCmsg))
  (displayln (IRCmsg-tail IRCmsg)))

;(show-irc-msg (all-to-strings (parse-line ctcp-time))

(define (get-nick msg)
  "Gets the nickname from the prefix of the message"
  (let ([exclamation (string-index (IRCmsg-prefix msg) #\!)])
    (if exclamation
        (substring (IRCmsg-prefix msg) 0 exclamation)
        #f)))

(define (not-empty? ircmsgpart)
  (and (not (null? ircmsgpart))
       (not (string=? ircmsgpart ""))))

(define (to-string ircmsg)
  (define (get-prefix ircmsg)
    (if (not-empty?(IRCmsg-prefix ircmsg))
        (string-append ":" (IRCmsg-prefix ircmsg))
        ""))
  (define (get-params ircmsg)
    (if (not-empty? (IRCmsg-params ircmsg))
        (string-append " " (IRCmsg-params ircmsg))
        ""))
  (define (get-tail ircmsg)
    (if (not-empty? (IRCmsg-tail ircmsg))
        (string-append " :" (IRCmsg-tail ircmsg) "\n")
        "\n"))
  (string-append (get-prefix ircmsg)
                 (string-append (IRCmsg-command ircmsg) " ")
                 (get-params ircmsg)
                 (get-tail ircmsg)))

(define (to-bytes ircmsg)
  (define (get-prefix ircmsg)
    (string->bytes/utf-8 
     (if (not-empty? (IRCmsg-prefix ircmsg))
         (string-append ":" (IRCmsg-prefix ircmsg) " ")
         "")))
  (define (get-params ircmsg)
    (string->bytes/utf-8 (if (not-empty? (IRCmsg-params ircmsg))
                             (string-append (IRCmsg-params ircmsg) " ")
                             "")))
  (define (get-tail ircmsg)
    (string->bytes/utf-8 
     (if (not-empty? (IRCmsg-tail ircmsg))
         (IRCmsg-tail ircmsg)
         "")))
  (let* [(cmd (IRCmsg-command ircmsg))
         (is-ctcp-req (string=? cmd "CTCP"))
         (is-ctcp-rep (string=? cmd "CTCPREPLY"))
         (is-action (string=? cmd "ACTION"))
         (is-ctcp (or is-ctcp-req is-ctcp-rep is-action))]
    (bytes-append (get-prefix ircmsg)
                  (cond [is-ctcp-req (string->bytes/utf-8 "PRIVMSG ")]
                        [is-ctcp-rep (string->bytes/utf-8 "NOTICE ")]
                        [is-action (string->bytes/utf-8 "ACTION ")]
                        [(not is-ctcp) (string->bytes/utf-8 " ")])
                  (get-params ircmsg)
                  (if is-ctcp
                      (bytes-append #" :" #"\1" (get-tail ircmsg) #"\1\n")
                      (bytes-append #" :" (get-tail ircmsg) #"\n")))))


(define parser-tests
  (test-suite 
   "Tests for the parser"
   (let ([ctcp-msg '(58 68 101 118 78 117 108 108 33 115 101 114 118 105 99 101 115 64 100 121 110 97 115 116 121 110 101 116 46 110 101 116 32 80 82 73 86 77 83 71 32 66 108 97 114 114 32 58 1 84 73 77 69 1)]
         [ircmsg (IRCmsg "" "NICK" "mynick" "")])
     (check-equal? (starts-with-colon? '(58 1 2 3 4)) #t)
     (check-equal? (starts-with-colon? '(59 1 2 3 4)) #f)
     (let-values ([(prfx wopf) (next-word '(1 2 3 32 4 5 6))])
       (check-equal? prfx '(1 2 3)))
     (check-equal? (is-tail-ctcp? '(2 3 4 5 6 7)) #f)
     (check-equal? (is-tail-ctcp? '(1 2 3 4 5 1)) #t)
     (check-equal? (is-tail-ctcp? '(0 2 3 4 5 1)) #f)
     (check-equal? (is-tail-ctcp? '(1 2 3 4 5 9)) #f)
     (check-equal? (to-string ircmsg) "NICK mynick\n")
     )))


