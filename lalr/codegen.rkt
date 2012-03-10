#lang racket/base

(require (for-template racket/base)
         racket/list
         racket/set
         "../stx-types.rkt"
         "flatten.rkt")

(provide rules-codegen)



(define (rules-codegen stx)
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f "The set of grammatical rules can't be empty." stx)]

    [(_ r ...)
     (begin
       ;; (listof stx)
       (define rules (syntax->list #'(r ...)))

       ;; We flatten the rules so we can use the yacc-style ruleset that parser-tools
       ;; supports.
       (define flattened-rules (flatten-rules rules))

       (define generated-rule-codes (map flat-rule->yacc-rule flattened-rules))
       
       ;; The first rule, by default, is the start rule.
       (define start-id (syntax-case (first rules) (rule)
                          [(rule id pattern)
                           #'id]))
       
       (define-values (implicit-tokens    ;; (listof string-stx)
                       explicit-tokens)   ;; (listof identifier-stx)
         (rules-collect-token-types rules))

       ;; (listof string)
       (define implicit-token-types
         (set->list (list->set (map syntax-e implicit-tokens))))

       ;; (listof symbol)
       (define explicit-token-types
         (set->list (list->set (map syntax-e explicit-tokens))))

       ;; (listof (U symbol string))
       (define token-types
         (set->list (list->set (append (map (lambda (x) (string->symbol (syntax-e x)))
                                            implicit-tokens)
                                       (map syntax-e explicit-tokens)))))
       
       (with-syntax ([start-id start-id]

                     [(token-types ...)
                      token-types]

                     [(explicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           explicit-token-types)]
                     [(implicit-token-type-constructor ...)
                      (map (lambda (x) (string->symbol (format "token-~a" x)))
                           implicit-token-types)]

                     [(explicit-token-types ...) explicit-token-types]
                     [(implicit-token-types ...) implicit-token-types]

                     [(generated-rule-code ...) generated-rule-codes])

         (syntax/loc stx
           (begin
             (require parser-tools/lex
                      parser-tools/yacc)

             (provide parse
                      default-lex/1
                      tokens

                      all-token-names

                      token-EOF
                      explicit-token-type-constructor ...
                      implicit-token-type-constructor ...

                      current-source
                      current-parser-error-handler
                      [struct-out exn:fail:parsing])

             (define all-token-names '(EOF explicit-token-types ...
                                           implicit-token-types ...))
             (define-tokens tokens (EOF token-types ...))

             (define default-lex/1
               (lexer-src-pos [implicit-token-types
                               (implicit-token-type-constructor lexeme)]
                              ...
                              [(eof) (token-EOF eof)]))
             
             ;; During parsing, we should define the source of the input.
             (define current-source (make-parameter #f))

             ;; When bad things happen, we need to emit errors with source location.
             (struct exn:fail:parsing exn:fail (srclocs)
                     #:transparent
                     #:property prop:exn:srclocs (lambda (instance)
                                                   (exn:fail:parsing-srclocs instance)))

             (define current-parser-error-handler
               (make-parameter
                (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                  (raise (exn:fail:parsing
                          (format "Encountered error while parsing, near: ~e [line=~a, column=~a, position=~a]"
                                  tok-value
                                  (position-line start-pos)
                                  (position-col start-pos)
                                  (position-offset start-pos))
                          (current-continuation-marks)
                          (list (srcloc (current-source)
                                        (position-line start-pos)
                                        (position-col start-pos)
                                        (position-offset start-pos)
                                        (if (and (number? (position-offset end-pos))
                                                 (number? (position-offset start-pos)))
                                            (- (position-offset end-pos)
                                               (position-offset start-pos))
                                            #f))))))))

             ;; If someone feeds us a token that has no positional information,
             ;; just force it into the right shape.
             (define (coerse-to-position-token t)
               (cond
                [(position-token? t)
                 t]
                [else
                 (position-token t
                                 (position #f #f #f)
                                 (position #f #f #f))]))

             (define parse
               (let (
                      [THE-GRAMMAR
                       (parser
                        (tokens tokens)
                        (src-pos)
                        (start start-id)
                        (end EOF)
                        (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                                 ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))
                        (grammar
                         generated-rule-code ...))]
                     )
                 (lambda (source tokenizer)
                   (parameterize ([current-source source])
                     (THE-GRAMMAR (lambda ()
                                    (coerse-to-position-token (tokenizer))))))))))))]))


;; Given a flattened rule, returns a syntax for the code
;; that preserves as much as possible.
(define (flat-rule->yacc-rule a-flat-rule)
  (syntax-case a-flat-rule (prim-rule inferred-prim-rule)
    [(prim-rule name clauses ...)
     (with-syntax ([(translated-clause ...)
                    (map (lambda (c) (translate-clause #'name c))
                         (syntax->list #'(clauses ...)))])
       #`[name translated-clause ...])]
    
    [(inferred-prim-rule info name clauses ...)
     (with-syntax ([(translated-clause ...)
                    (map (lambda (c) (translate-clause #f c
                                                       #:inferred-pattern-type #'info))
                         (syntax->list #'(clauses ...)))])
     #`[name translated-clause ...])]))


  

;; translates a single primitive rule clause.
;; A clause is a simple list of ids, lit, vals, and inferred-id elements.
;; The action taken depends on the pattern type.
(define (translate-clause rule-name/false a-clause
                          #:inferred-pattern-type (inferred-pattern-type #f))
  (define translated-patterns
    (let loop ([primitive-patterns (syntax->list a-clause)])
      (cond
       [(empty? primitive-patterns)
        '()]
       [else
        (cons (syntax-case (first primitive-patterns) (id lit token inferred-id)
                [(id val)
                 #'val]
                [(lit val)
                 (datum->syntax #f (string->symbol (syntax-e #'val)) #'val)]
                [(token val)
                 #'val]
                [(inferred-id val reason)
                 #'val])
              (loop (rest primitive-patterns)))])))
  
  (define translated-actions
    (for/list ([translated-pattern (in-list translated-patterns)]
               [primitive-pattern (syntax->list a-clause)]
               [pos (in-naturals 1)])
      (with-syntax ([$X (datum->syntax translated-pattern (string->symbol (format "$~a" pos)))]
                    [$X-start-pos (datum->syntax translated-pattern (string->symbol (format "$~a-start-pos" pos)))]
                    [$X-end-pos (datum->syntax translated-pattern (string->symbol (format "$~a-end-pos" pos)))])
        (with-syntax ([primitive-loc
                       #'(list (current-source)
                               (position-line $X-start-pos)
                               (position-col $X-start-pos)
                               (position-offset $X-start-pos)
                               (if (and (number? (position-offset $X-start-pos))
                                        (number? (position-offset $X-end-pos)))
                                   (- (position-offset $X-end-pos)
                                      (position-offset $X-start-pos))
                                   #f))])
          (syntax-case primitive-pattern (id lit token inferred-id)
            [(id val)
             #`(list (datum->syntax #f $X primitive-loc))]
            [(inferred-id val reason)
             (syntax-case #'reason (choice repeat maybe seq)
               [choice
                #'(list $X)]
               [repeat
                #'(list $X)]
               [maybe
                #'(if (null? (syntax-e $X))
                      (list #f)
                      (list (datum->syntax #f $X primitive-loc)))]
               [seq
                #'$X])]
            [(lit val)
             #`(list (datum->syntax #f $X primitive-loc))]
            [(token val)
             #`(list (datum->syntax #f $X primitive-loc))])))))
  
  (with-syntax ([(translated-pattern ...) translated-patterns]
                [(translated-action ...) translated-actions])
    (cond
     [rule-name/false
      #`[(translated-pattern ...)
         (datum->syntax #f
                        (append (list '#,rule-name/false) translated-action ...)
                        (list (current-source) #f #f #f #f))]]
     [else
      (syntax-case inferred-pattern-type (choice repeat maybe seq)
        [choice
         #`[(translated-pattern ...)
            (datum->syntax #f (append translated-action ...))]]
        [repeat
         #`[(translated-pattern ...)
            (datum->syntax #f (append translated-action ...))]]

        [maybe
         #`[(translated-pattern ...)
            (datum->syntax #f (append translated-action ...))]]

        [seq
         #`[(translated-pattern ...)
            (datum->syntax #f (append translated-action ...))]])])))





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


