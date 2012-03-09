#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         racket/list
         "rule-structs.rkt")

;; A parser for grammars.

(provide tokens
         token-LPAREN
         token-RPAREN
         token-LBRACKET
         token-RBRACKET
         token-PIPE
         token-REPEAT
         token-RULE_HEAD
         token-ID
         token-LIT
         token-EOF
         grammar-parser

         current-source
         current-parser-error-handler
         
         [struct-out rule]
         [struct-out lhs-id]
         [struct-out pattern]
         [struct-out pattern-id]
         [struct-out pattern-lit]
         [struct-out pattern-token]
         [struct-out pattern-choice]
         [struct-out pattern-repeat]
         [struct-out pattern-maybe]
         [struct-out pattern-seq])

(define-tokens tokens (LPAREN
                       RPAREN
                       LBRACKET
                       RBRACKET
                       PIPE
                       REPEAT
                       RULE_HEAD
                       ID
                       LIT
                       EOF))

;; grammar-parser: (-> token) -> (listof rule)
(define grammar-parser
  (parser
   (tokens tokens)
   (src-pos)
   (start rules)
   (end EOF)

   (grammar
    [rules
     [(rules*) $1]]

    [rules*
     [(rule rules*)
      (cons $1 $2)]
     [()
      '()]]

    ;; I have a separate token type for rule identifiers to avoid the
    ;; shift/reduce conflict that happens with the implicit sequencing
    ;; of top-level rules.  i.e. the parser can't currently tell, when
    ;; it sees an ID, if it should shift or reduce to a new rule.
    [rule
     [(RULE_HEAD pattern)
      (begin 
        (define trimmed (regexp-replace #px"\\s*:$" $1 ""))
        (rule $1-start-pos
              $2-end-pos
              (lhs-id $1-start-pos
                      (position (+ (position-offset $1-start-pos)
                                   (string-length trimmed))
                                (position-line $1-start-pos)
                                (position-col $1-start-pos))
                      trimmed)
              $2))]]

    [pattern
     [(implicit-pattern-sequence PIPE pattern)
      (if (pattern-choice? $3)
          (pattern-choice $1-start-pos
                          $3-end-pos
                          (cons $1 (pattern-choice-vals $3)))
          (pattern-choice $1-start-pos
                          $3-end-pos
                          (list $1 $3)))]
     [(implicit-pattern-sequence)
      $1]]

    [implicit-pattern-sequence
     [(repeatable-pattern implicit-pattern-sequence)
      (if (pattern-seq? $2)
          (pattern-seq $1-start-pos $2-end-pos (cons $1 (pattern-seq-vals $2)))
          (pattern-seq $1-start-pos $2-end-pos (list $1 $2)))]
     [(repeatable-pattern)
      $1]]

    [repeatable-pattern
     [(atomic-pattern REPEAT)
      (cond [(string=? $2 "*")
             (pattern-repeat $1-start-pos $2-end-pos 0 $1)]
            [(string=? $2 "+")
             (pattern-repeat $1-start-pos $2-end-pos 1 $1)]
            [else
             (error 'grammar-parse "unknown repetition operator ~e" $2)])]
     [(atomic-pattern)
      $1]]

    [atomic-pattern
     [(LIT)
      (pattern-lit $1-start-pos $1-end-pos
                   (substring $1 1 (sub1 (string-length $1))))]
     
     [(ID)
      (if (token-id? $1)
          (pattern-token $1-start-pos $1-end-pos $1)
          (pattern-id $1-start-pos $1-end-pos $1))]

     [(LBRACKET pattern RBRACKET)
      (pattern-maybe $1-start-pos $3-end-pos $2)]
     
     [(LPAREN pattern RPAREN)
      (if (pattern-seq? $2)
          (pattern-seq $1-start-pos $3-end-pos (pattern-seq-vals $2))
          (pattern-seq $1-start-pos $3-end-pos (list $2)))]])

   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))))




; token-id: string -> boolean
;; Produces true if the id we see should be treated as the name of a token.
;; By convention, tokens are all upper-cased.
(define (token-id? id)
  (string=? (string-upcase id)
            id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
