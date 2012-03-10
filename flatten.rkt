#lang racket/base
(require "stx-types.rkt"
         (for-syntax racket/base))

(provide flatten-rule
         prim-rule)



;; Translates rules to lists of primitive rules.



;; flatten-rule: rule -> (listof primitive-rule)
(define (flatten-rule a-rule #:fresh-name [fresh-name (lambda () (gensym 'rule))])
  (syntax-case a-rule (rule)
    [(rule name pat)
     (syntax-case #'pat (id lit token choice repeat maybe seq)
       [(id val)
        (list #'(prim-rule name [pat]))]
       [(lit val)
        (list #'(prim-rule name [pat]))]
       [(token val)
        (list #'(prim-rule name [pat]))]
       [(choice sub-pat ...)
        (cond
         [(andmap primitive-pattern? (syntax->list #'(sub-pat ...)))
          (list #'(prim-rule name [sub-pat] ...))]
         [else
          (error 'choice-unfinished)])]
       [(repeat min sub-pat)
        (cond [(primitive-pattern? #'sub-pat)
               (cond [(= (syntax-e #'min) 0)
                      (list #'(prim-rule name
                                         [sub-pat (id name)]
                                         []))]
                     [(= (syntax-e #'min) 1)
                      (list #'(prim-rule name
                                         [sub-pat (id name)]
                                         [sub-pat]))])]
              [else
               (error 'repeat-unfinished)])]
       [(maybe sub-pat)
        (cond [(primitive-pattern? #'sub-pat)
               (list #'(prim-rule name
                                  [sub-pat]
                                  []))]
              [else
               (error 'maybe-unfinished)])]
       [(seq sub-pat ...)
        (cond
         [(andmap primitive-pattern? (syntax->list #'(sub-pat ...)))
          (list #'(prim-rule name [sub-pat ...]))]
         [else
          (error 'seq-unfinished)])])]))




;; Given a pattern, return a key appropriate for a hash.
(define (pattern->hash-key a-pat)
  (syntax->datum a-pat))



;(define (flatten-pattern a-pat)
;  (syntax-case a-pat ()
;    ...))


;; Returns true if the pattern looks primitive
(define (primitive-pattern? a-pat)
  (syntax-case a-pat (id lit token choice repeat maybe seq)
    [(id val)
     #t]
    [(lit val)
     #t]
    [(token val)
     #t]
    [(choice sub-pat ...)
     #f]
    [(repeat min val)
     #f]
    [(maybe sub-pat)
     #f]
    [(seq sub-pat ...)
     #f]))


(define-syntax (prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))