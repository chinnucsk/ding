#lang racket
(require "parser.rkt")
(require "handler.rkt")

(provide irc login connect-to-freenode reply)

(define (read-write in out caller-thread)
  (when (port-closed? in)
    (displayln "Port closed!")
    (error "port closed"))
  (when (byte-read? in)
    (displayln "'in' is ready, reading line.")
    (thread-send caller-thread (read-bytes-line in 'any)))
  (let ([outmsg (thread-try-receive)])
    (when outmsg
      (displayln "Got outmsg, appending newline and sending")
      (write-bytes (bytes-append outmsg #"\n") out)))
  (sleep 0.05)
  (read-write in out caller-thread))
       
(define (connect host port nick caller-thread)
  (let-values ([(in out) (tcp-connect host port)])
    (file-stream-buffer-mode out 'line)
    (read-write in out (current-thread))))

(define (get-and-parse sock-handler)
  (displayln "Waiting for next message")
  (let* ((raw (thread-receive))
         (msg (parse-irc-line raw)))
    (apply-handlers msg)))

(define (irc host port nick)
  (let ((sock-handler (thread (lambda () ((connect host port nick (current-thread)))))))
    (get-and-parse sock-handler)))

(define (reply msg)
  
    
         