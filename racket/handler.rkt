#lang racket

(require "parser.rkt")

(provide add-handler do-handlers apply-handlers)

(define handlers '())

(define (add-handler cmd handlerfunc)
  (displayln (string-append "adding handler for " cmd))
  (set! handlers (cons `(,cmd ,handlerfunc) handlers)))

(define (apply-handler hndlr msg)
  ;(displayln "Applying handler")
  (let ((cmd (IRCmsg-command msg)))
    (if (string=? (car hndlr) (IRCmsg-command msg))
        (begin
          (displayln (string-append "Applying handler: " (car hndlr)))
          ((cadr hndlr) msg))
        '())))

(define (do-handlers msg)
  ;(displayln "in do-handlers")
  (for-each (lambda (x) (apply-handler x msg)) handlers))

(define (apply-handlers line)
  (displayln line)
  (for-each (lambda (x) (apply-handler x (parse-irc-line line))) handlers))



