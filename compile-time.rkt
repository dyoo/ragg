#lang racket/base

(require (for-template racket/base)
         racket/list
         racket/set
         "stx-types.rkt")

(provide rules-codegen
         (all-from-out "stx-types.rkt"))


(define (rules-codegen stx)
  (syntax-case stx ()
    [(_)
     (raise-syntax-error #f "The set of grammatical rules can't be empty." stx)]

    [(_ r ...)
     (begin
       ;; (listof stx)
       (define rules (syntax->list #'(r ...)))

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
                     )

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
                      [struct-out exn:fail:parse-grammar])

             (define all-token-names '(EOF explicit-token-types ...
                                           implicit-token-types ...))
             (define-tokens tokens (EOF token-types ...))

             (define default-lex/1
               (lexer [implicit-token-types
                       (implicit-token-type-constructor lexeme)]
                      ...
                      [(eof) (token-EOF eof)]))

             ;; During parsing, we should define the source of the input.
             (define current-source (make-parameter #f))

             ;; When bad things happen, we need to emit errors with source location.
             (struct exn:fail:parse-grammar exn:fail (srclocs)
                     #:transparent
                     #:property prop:exn:srclocs (lambda (instance)
                                                   (exn:fail:parse-grammar-srclocs instance)))

             (define current-parser-error-handler
               (make-parameter
                (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                  (raise (exn:fail:parse-grammar
                          (format "Error while parsing grammar near: ~e [line=~a, column~a, position=~a]"
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

             (define parse
               (let (
                     ;; [THE-GRAMMAR
                     ;;  (parser
                     ;;   (tokens tokens)
                     ;;   (src-pos)
                     ;;   (start start-id)
                     ;;   (end EOF)
                     ;;   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                     ;;            ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))
                     ;;   (grammar
                     ;;    generated-rule  ...))]
                     )
                 (lambda (source tokenizer)
                   (parameterize ([current-source source])
                     'fixme
                     ;(THE-GRAMMAR tokenizer)
                     ))))))))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Genereate all the rules, both the explicit rules and the implicit rules.
;; ;; Implicit rules come from the use of 'choice, 'repeat, and 'maybe
;; ;; pattern types.
;; ;; generate-rules: (listof stx) -> (listof stx)
;; (define (generate-rules rules)
;;   (define implicit-ht (make-hasheq))

;;   (define (generate-rule a-rule)
;;     (syntax-case a-rule (rule)
;;       [(rule id pattern)
;;        (with-syntax ([generated-pattern-code (generate-pattern-code #'pattern)])
;;          (syntax/loc a-rule
;;            [id generated-pattern-code]]))

;;   (define (generate-pattern-code a-pattern)
;;     (syntax-case a-pattern (id lit token choice repeat maybe seq)
;;       [(id val)
;;        ...]
;;       [(lit val)
;;        ...]
;;       [(token val)
;;        ...]
;;       [(choice vals)
;;        ...]
;;       [(repeat min val)
;;        ...]
;;       [(maybe val)
;;        ...]
;;       [(seq vals)
;;        ...])))






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


