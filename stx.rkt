#lang racket/base

(require "parser.rkt"
         syntax/strip-context)

(provide rules->stx)


;; rules->stx: (listof rule) -> syntax
(define (rules->stx source rules)
  (define rule-stxs
    (for-each (lambda (stx) (rule->stx source stx)) rules))
  (with-syntax ([(rule ...) rule-stxs])
    (strip-context
     #'(rules rule-stxs ...))))


(define (rule->stx source a-rule)
  (define id-stx (datum->syntax source (rule-lhs a-rule)))
  (define pattern-stx (pattern->stx source (rule-pattern a-rule)))
  (datum->syntax #f
                 `(rule ,id-stx ,pattern-stx)
                 (list source line column position span)))


(define (pattern->stx source a-pattern)
  (datum->syntax #f
                 (match a-pattern
                   [(struct ...)])
                 (list source line column position span)))
