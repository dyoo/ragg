#lang racket/base
(require "python-grammar.rkt"
         parser-tools/lex)


(define (tokenize/1 ip)
  (default-lex/1 ip))

token-names

(token-NAME "foobar")

(tokenize/1 (open-input-string "exec"))
(tokenize/1 (open-input-string ">=="))
(tokenize/1 (open-input-string "%"))



(grammar "hello.py" tokenize/1)