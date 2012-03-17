#lang racket
(require racket/async-channel)
(require "parser.rkt")
(require "handler.rkt")

(provide reply login connect-to-freenode)

(define irc-recv (make-async-channel))
(define irc-send (make-async-channel))

(define loggedin #f)

(define (read-and-queue in rcv)
  "Reads from the in-port and puts the lines into the rcv queue"
  (when (port-closed? in)
    (error "port closed"))
  (async-channel-put rcv (read-bytes-line in 'any))
  (read-and-queue in rcv))

(define (connect-to-irc host port rcv snd)
  (displayln "Connecting")
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode out 'line)
    (thread (lambda () (irc-router out irc-recv irc-send)))
    (displayln "starting receive loop")
    (login "dingding")
    (with-handlers ([exn:fail? (lambda (exn) (connect-to-irc host port rcv snd))])
      (read-and-queue in rcv))))

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
    (displayln "Logging in")
    (reply (IRCmsg "" "NICK" nick ""))
    (reply (IRCmsg "" "USER" (string-append nick " " nick " " nick) nick))    
    (reply (IRCmsg "" "JOIN" "#yfl" ""))
    (set! loggedin #t)))

(define (connect-to-freenode)
  (connect-to-irc "irc.freenode.net" 6667 irc-recv irc-send))