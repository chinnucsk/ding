#lang racket

(require "parser.rkt")

(define lines (file->bytes-lines "/home/gert/src/ding/raw.log"))

(define msgs (map to-bytes (map parse-irc-line lines)))


