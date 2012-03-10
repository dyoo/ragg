#lang racket/base
(require "stx-types.rkt"
         (for-syntax racket/base))

(provide flatten-rule
         flatten-rules
         prim-rule)



;; Translates rules to lists of primitive rules.


(define (flatten-rules rules)
  (define ht (make-hash))
  (apply append (map (lambda (a-rule) (flatten-rule a-rule #:ht ht)) rules)))


;; flatten-rule: rule -> (listof primitive-rule)
(define (flatten-rule a-rule
                      #:fresh-name [fresh-name (lambda () (gensym 'rule))]

                      ;; ht: (hashtableof pattern-hash-key pat)
                      #:ht [ht (make-hash)])

  (let recur ([a-rule a-rule]
              [inferred? #f])

    ;; lift-nonprimitive-pattern: pattern -> (values (listof primitive-rule) pattern)
    ;; Turns non-primitive patterns into primitive patterns, and produces a set of
    ;; derived rules.
    (define (lift-nonprimitive-pattern a-pat)
      (cond
       [(primitive-pattern? a-pat)
        (values '() a-pat)]
       [(hash-has-key? ht (pattern->hash-key a-pat))
        (values '() (hash-ref ht (pattern->hash-key a-pat)))]
       [else
        (define new-name (datum->syntax #f (fresh-name) a-pat))
        (define new-inferred-id #`(inferred-id  #,new-name))
        (hash-set! ht (pattern->hash-key a-pat) new-inferred-id)
        (values (recur #`(rule #,new-name #,a-pat) #t)
                new-inferred-id)]))

    (define (lift-nonprimitive-patterns pats)
      (define-values (rules patterns)
        (for/fold ([inferred-rules '()]
                   [patterns '()])
                  ([p (in-list pats)])
          (define-values (new-rules new-p)
            (lift-nonprimitive-pattern p))
          (values (append new-rules inferred-rules)
                  (cons new-p patterns))))
      (values (reverse rules) (reverse patterns)))
          
    (with-syntax ([head (if inferred? #'inferred-prim-rule #'prim-rule)])
      (syntax-case a-rule (rule)
        [(rule name pat)
         (syntax-case #'pat (id lit token choice repeat maybe seq)

           ;; The primitive types stay as they are:
           [(id val)
            (list #'(head name [pat]))]
           [(lit val)
            (list #'(head name [pat]))]
           [(token val)
            (list #'(head name [pat]))]

           
           ;; Everything else might need lifting:
           [(choice sub-pat ...)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-patterns (syntax->list #'(sub-pat ...))))
              (with-syntax ([(sub-pat ...) new-sub-pats])
                (append (list #'(head name [sub-pat] ...))
                        inferred-rules)))]

           [(repeat min sub-pat)
            (begin
              (define-values (inferred-rules new-sub-pat)
                (lift-nonprimitive-pattern #'sub-pat))
              (with-syntax ([sub-pat new-sub-pat])
                (cons (cond [(= (syntax-e #'min) 0)
                             #`(head name
                                          [#,(if inferred? #'(inferred-id name) #'(id name)) sub-pat]
                                          [])]
                            [(= (syntax-e #'min) 1)
                             #`(head name
                                          [#,(if inferred? #'(inferred-id name) #'(id name)) sub-pat]
                                          [sub-pat])])
                      inferred-rules)))]

           [(maybe sub-pat)
            (begin
              (define-values (inferred-rules new-sub-pat)
                (lift-nonprimitive-pattern #'sub-pat))
              (with-syntax ([sub-pat new-sub-pat])
                (cons #'(head name
                              [sub-pat]
                              [])
                      inferred-rules)))]

           [(seq sub-pat ...)
            (begin
              (define-values (inferred-rules new-sub-pats)
                (lift-nonprimitive-patterns (syntax->list #'(sub-pat ...))))
              (with-syntax ([(sub-pat ...) new-sub-pats])
                (cons #'(head name [sub-pat ...])
                      inferred-rules)))])]))))


;; Given a pattern, return a key appropriate for a hash.
(define (pattern->hash-key a-pat)
  (syntax->datum a-pat))


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

(define-syntax (inferred-prim-rule stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))

(define-syntax (inferred-id stx)
  (raise-syntax-error #f "internal error: should not be macro expanded" stx))