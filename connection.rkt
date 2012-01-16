#lang racket
(require racket/async-channel)
(require "parser.rkt")
(require "handler.rkt")

(define irc-recv (make-async-channel))
(define irc-send (make-async-channel))

(define loggedin #f)

(define (connect-to-irc host port rcv snd)
  (displayln "In connect-to-irc")
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode out 'line)
    (thread (lambda () (irc-router out irc-recv irc-send)))
    (displayln "starting receive loop")
    (for ([line (in-bytes-lines in)])
      (async-channel-put rcv line))))

(define (irc-router out-port rcv snd)
  (let ([ln (async-channel-try-get rcv)]
        [msg (async-channel-try-get snd)])
    (when ln      
      (apply-handlers ln))
    (when msg 
      (if (IRCmsg? msg)
          (begin
            (display (string-append "-> " (to-string msg)))
            (write-bytes (to-bytes msg) out-port))
          (begin
            (displayln (string-append "-> " msg))
            (write-bytes (bytes-append (to-bytes msg) #"\n") out-port))))
    (when (and (not ln) (not msg))
      (sleep 0.05))
    (irc-router out-port rcv snd)))

(define (reply msg)
  (async-channel-put irc-send msg))
  
(define (rawreply line)
  (reply (parse-irc-line (string->list line))))

(define (login nick)
  (when (not loggedin)
    (reply (IRCmsg "" "NICK" nick ""))
    (reply (IRCmsg "" "USER" (string-append nick " " nick " " nick) nick))
    (set! loggedin #t)))

(add-handler "PING" (lambda (msg) 
                      (reply (IRCmsg "" "PONG" "" (IRCmsg-tail msg)))))
(add-handler "NOTICE" (lambda (msg)
                        (when (string=? (IRCmsg-params msg) "*")
                          (displayln "loggin in")
                          (login "yflbot"))))

;; 376 = the 'end of MOTD' command.
(add-handler "376" (lambda (msg)
                     (reply (IRCmsg "" "JOIN" "#yfl" ""))))

(add-handler "CTCP" 
             (lambda (msg)
               (when (string=? (IRCmsg-tail msg) "VERSION")
                 (reply (IRCmsg "" "CTCP" "#yfl" "ACTION was version'd"))
                 (displayln "version request received!"))))

(connect-to-irc "irc.freenode.net" 6667 irc-recv irc-send)

