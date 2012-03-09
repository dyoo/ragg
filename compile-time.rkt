#lang racket/base

(require (for-template racket/base
                       "runtime.rkt"))

(provide rules-codegen
         rule
         id
         lit
         token
         choice
         repeat
         maybe
         seq)


(define (rules-codegen stx)
  (syntax-case stx ()
    [(_ r ...)
     (begin
       (define rules (syntax->list #'(r ...)))
       
       (define-values (implicit-tokens explicit-tokens)
         (rules-collect-token-types rules))
       
       (with-syntax ([(toplevel-token-constructors ...)
                      '()]
                     [grammar-defn
                      (rules->grammar-defn rules)])
         (syntax/loc stx
           (begin
             (require parser-tools/lex
                      parser-tools/yacc)

             (provide grammar
                      toplevel-token-constructors ...)
             
             (define grammar
               grammar-defn)
             
             ;; the token types
             
             ;; the provides

             ;; the lexer

             ;; the parser         
             (void)))))]))



;; collect-token-types: (listof rule-syntax) -> (values (listof identifier) (listof identifier))
;;
;; Given a rule, automatically derive the list of implicit and explicit rules
;; we need to generate.
(define (rules-collect-token-types rules)
  (for/fold ([implicit '()]
             [explicit '()])
      ([r (in-list rules)])
    (rule-collect-token-types r implicit explicit)))

(define (rule-collect-token-types a-rule implicit explicit)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     (values (pattern-collect-implicit-token-types #'a-pattern implicit)
             (cons #'id explicit))]))

(define (pattern-collect-implicit-token-types a-pattern acc)
  (let loop ([a-pattern a-pattern]
             [acc acc])
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       acc]
      [(lit val)
       (cons #'val acc)]
      [(token val)
       acc]
      [(choice vals ...)
       (foldl loop acc (syntax->list #'(vals ...)))]
      [(repeat min val)
       (loop #'val acc)]
      [(maybe val)
       (loop #'val acc)]
      [(seq vals ...)
       (foldl loop acc (syntax->list #'(vals ...)))])))


(define (rules->grammar-defn rules)
  #'(let ([THE-GRAMMAR
           (lambda (tokenizer)
             'the-grammar)])
      (lambda (source tokenizer)
        (parameterize ([current-source source])
          (THE-GRAMMAR tokenizer)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; These are just here to provide bindings for Check Syntax.
;; Otherwise, we should never hit these, as the toplevel rules-codegen
;; should eliminate all uses of these if it does the right thing.
(define (rule stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (id stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (lit stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (token stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (choice stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (repeat stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (maybe stx) (raise-syntax-error #f "Used out of context of rules" stx))
(define (seq stx) (raise-syntax-error #f "Used out of context of rules" stx))