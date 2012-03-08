#lang racket/base

(require "rule-structs.rkt"
         parser-tools/lex
         racket/match
         syntax/strip-context)

(provide rules->stx)

;; Given a sequence of rules, we translate these to syntax objects.

;; rules->stx: (listof rule) -> syntax
(define (rules->stx source rules)
  (define rule-stxs
    (map (lambda (stx) (rule->stx source stx))
         rules))
  (with-syntax ([(rule ...) rule-stxs])
    (strip-context
     #'(rules rule ...))))

(define (rule->stx source a-rule)
  (define id-stx
    (datum->syntax #f
                   (string->symbol (lhs-id-val (rule-lhs a-rule)))
                   (list source
                         (position-line (lhs-id-start (rule-lhs a-rule)))
                         (position-col (lhs-id-start (rule-lhs a-rule)))
                         (position-offset (lhs-id-start (rule-lhs a-rule)))
                         (if (and (number? (position-offset (lhs-id-start (rule-lhs a-rule))))
                                  (number? (position-offset (lhs-id-end (rule-lhs a-rule)))))
                             (- (position-offset (lhs-id-end (rule-lhs a-rule)))
                                (position-offset (lhs-id-start (rule-lhs a-rule))))
                             #f))))
  (define pattern-stx (pattern->stx source (rule-pattern a-rule)))
  (define line (position-line (rule-start a-rule)))
  (define column (position-col (rule-start a-rule)))
  (define position (position-offset (rule-start a-rule)))
  (define span (if (and (number? (position-offset (rule-start a-rule)))
                        (number? (position-offset (rule-end a-rule))))
                   (- (position-offset (rule-end a-rule))
                      (position-offset (rule-start a-rule)))
                   #f))
  (datum->syntax #f
                 `(rule ,id-stx ,pattern-stx)
                 (list source line column position span)))

(define (pattern->stx source a-pattern)
  (define recur (lambda (s) (pattern->stx source s)))
  
  (define line (position-line (pattern-start a-pattern)))
  (define column (position-col (pattern-start a-pattern)))
  (define position (position-offset (pattern-start a-pattern)))
  (define span (if (and (number? (position-offset (pattern-start a-pattern)))
                        (number? (position-offset (pattern-end a-pattern))))
                   (- (position-offset (pattern-end a-pattern))
                      (position-offset (pattern-start a-pattern)))
                   #f))
  (define source-location (list source line column position span))
  (datum->syntax #f
                 (match a-pattern
                   [(struct pattern-id (start end val))
                    `(id ,(datum->syntax #f (string->symbol val) source-location))]
                   [(struct pattern-lit (start end val))
                    `(lit ,(datum->syntax #f val source-location))]
                   [(struct pattern-token (start end val))
                    `(token ,(datum->syntax #f (string->symbol val) source-location))]
                   [(struct pattern-choice (start end vals))
                    `(choice ,@(map recur vals))]
                   [(struct pattern-repeat (start end min val))
                    `(repeat ,min ,(recur val))]
                   [(struct pattern-maybe (start end val))
                    `(maybe ,(recur val))]
                   [(struct pattern-seq (start end vals))
                    `(seq ,(map recur vals))])
                 source-location))
