#lang racket/base

(require (for-template racket/base "runtime.rkt")
         racket/set)

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
    [(_)
     (raise-syntax-error #f "The set of grammatical rules can't be empty." stx)]

    [(_ r ...)
     (begin
       (define rules (syntax->list #'(r ...)))
       
       (define-values (implicit-tokens   ;; (listof string-stx)
                       explicit-tokens)  ;; (listof identifier-stx)
         (rules-collect-token-types rules))

       ;; (listof string)
       (define implicit-token-types
         (set->list (list->set (map syntax-e implicit-tokens))))

       ;; (listof symbol)
       (define explicit-token-types
         (set->list (list->set (map syntax-e explicit-tokens))))

       
       (define token-types
         (set->list (list->set (append (map (lambda (x) (string->symbol (syntax-e x)))
                                            implicit-tokens)
                                       (map syntax-e explicit-tokens)))))
       
       (with-syntax ([(token-types ...)
                      token-types]
                     [(token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           token-types)]

                     [(explicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           explicit-token-types)]
                     [(implicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           implicit-token-types)]

                     [(explicit-token-types ...) explicit-token-types]
                     [(implicit-token-types ...) implicit-token-types])

         (syntax/loc stx
           (begin
             (require parser-tools/lex
                      parser-tools/yacc)

             (provide grammar
                      default-lex/1
                      tokens

                      all-token-names

                      token-EOF
                      explicit-token-type-constructor ...
                      implicit-token-type-constructor ...)

             (define all-token-names '(EOF explicit-token-types ...
                                           implicit-token-types ...))
             (define-tokens tokens (EOF token-types ...))

             (define default-lex/1
               (lexer [implicit-token-types
                       (implicit-token-type-constructor lexeme)]
                      ...
                      [(eof) (token-EOF eof)]))
             
             (define grammar
               (let ([THE-GRAMMAR
                      (lambda (tokenizer)
                        'the-grammar)])
                 (lambda (source tokenizer)
                   (parameterize ([current-source source])
                     (THE-GRAMMAR tokenizer)))))
             
             ;; the token types
             
             ;; the provides

             ;; the lexer

             ;; the parser         
             (void)))))]))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; collect-token-types: (listof rule-syntax) -> (values (listof identifier) (listof identifier))
;;
;; Given a rule, automatically derive the list of implicit and
;; explicit token types we need to generate.
;; 
(define (rules-collect-token-types rules)
  (define-values (implicit explicit)
    (for/fold ([implicit '()]
               [explicit '()])
        ([r (in-list rules)])
      (rule-collect-token-types r implicit explicit)))
  (values (reverse implicit) (reverse explicit)))
  
(define (rule-collect-token-types a-rule implicit explicit)
  (syntax-case a-rule (rule)
    [(rule id a-pattern)
     (pattern-collect-implicit-token-types #'a-pattern implicit explicit)]))

(define (pattern-collect-implicit-token-types a-pattern implicit explicit)
  (let loop ([a-pattern a-pattern]
             [implicit implicit]
             [explicit explicit])
    (syntax-case a-pattern (id lit token choice repeat maybe seq)
      [(id val)
       (values implicit explicit)]
      [(lit val)
       (values (cons #'val implicit) explicit)]
      [(token val)
       (values implicit (cons #'val explicit))]
      [(choice vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))]
      [(repeat min val)
       (loop #'val implicit explicit)]
      [(maybe val)
       (loop #'val implicit explicit)]
      [(seq vals ...)
       (for/fold ([implicit implicit]
                  [explicit explicit])
                 ([v (in-list (syntax->list #'(vals ...)))])
         (loop v implicit explicit))])))


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