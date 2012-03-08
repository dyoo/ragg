#lang racket/base
(require "python-grammar.rkt"
         parser-tools/lex)


(define (tokenize/1 ip)
  (lambda ()
    'fixme))


(grammar "hello.py" tokenize/1)