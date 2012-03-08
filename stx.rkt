#lang racket/base

(require "parser.rkt"
         syntax/strip-context)

(provide rules->stx)


;; rules->stx: (listof rule) -> syntax
(define (rules->stx rules)
  (strip-context
   ;; fixme
   #'(rules)))


(define (rule->stx a-rule)
  #'(rule))


(define (pattern->stx a-pattern)
  #'(pattern))
