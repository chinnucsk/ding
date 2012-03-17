#lang racket

(require "parser.rkt")
(require "connection.rkt")
(require "handler.rkt")

(add-handler "PING" (lambda (msg) 
                      (reply (IRCmsg "" "PONG" "" (IRCmsg-tail msg)))))

;(add-handler "NOTICE" (lambda (msg)
;                        (when (string=? (IRCmsg-params msg) "*")
;                          (displayln "loggin in")
;                          (login "yflbot"))))

;; 376 = the 'end of MOTD' command.
(add-handler "376" (lambda (msg)
                     (reply (IRCmsg "" "JOIN" "#yfl" ""))))

(add-handler "CTCP" 
             (lambda (msg)
               (when (string=? (IRCmsg-tail msg) "VERSION")
                 (reply (IRCmsg "" "CTCPREPLY" (get-nick msg) "VERSION Ding IRC Bot. v0.1 -- Racket version.")))))


(connect-to-freenode)