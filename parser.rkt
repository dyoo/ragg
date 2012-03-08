#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         racket/list)

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
         [struct-out rhs-id]
         [struct-out rhs-lit]
         [struct-out rhs-token]
         [struct-out rhs-choice]
         [struct-out rhs-repeat]
         [struct-out rhs-maybe]
         [struct-out rhs-seq])

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
     [(RULE_HEAD rhs)
      (begin 
        (define trimmed (regexp-replace #px"\\s*:$" $1 ""))
        (rule (lhs-id (position-offset $1-start-pos)
                    (+ (position-offset $1-start-pos)
                       (string-length trimmed))
                    trimmed)
            $2))]]

    [rhs
     [(implicit-rhs-sequence PIPE rhs)
      (if (rhs-choice? $3)
          (rhs-choice (position-offset $1-start-pos)
                      (position-offset $3-end-pos)
                      (cons $1 (rhs-choice-vals $3)))
          (rhs-choice (position-offset $1-start-pos)
                      (position-offset $3-end-pos)
                      (list $1 $3)))]
     [(implicit-rhs-sequence)
      $1]]

    [implicit-rhs-sequence
     [(repeatable-rhs implicit-rhs-sequence)
      (if (rhs-seq? $2)
          (rhs-seq (position-offset $1-start-pos) (position-offset $2-end-pos) (cons $1 (rhs-seq-vals $2)))
          (rhs-seq (position-offset $1-start-pos) (position-offset $2-end-pos) (list $1 $2)))]
     [(repeatable-rhs)
      $1]]

    [repeatable-rhs
     [(atomic-rhs REPEAT)
      (cond [(string=? $2 "*")
             (rhs-repeat (position-offset $1-start-pos) (position-offset $2-end-pos) 0 $1)]
            [(string=? $2 "+")
             (rhs-repeat (position-offset $1-start-pos) (position-offset $2-end-pos) 1 $1)]
            [else
             (error 'grammar-parse "unknown repetition operator ~e" $2)])]
     [(atomic-rhs)
      $1]]

    [atomic-rhs
     [(LIT)
      (rhs-lit (position-offset $1-start-pos) (position-offset $1-end-pos) $1)]
     
     [(ID)
      (if (token-id? $1)
          (rhs-token (position-offset $1-start-pos) (position-offset $1-end-pos) $1)
          (rhs-id (position-offset $1-start-pos) (position-offset $1-end-pos) $1))]

     [(LBRACKET rhs RBRACKET)
      (rhs-maybe (position-offset $1-start-pos) (position-offset $3-end-pos) $2)]
     
     [(LPAREN rhs RPAREN)
      (if (rhs-seq? $2)
          (rhs-seq (position-offset $1-start-pos) (position-offset $3-end-pos) (rhs-seq-vals $2))
          (rhs-seq (position-offset $1-start-pos) (position-offset $3-end-pos) (list $2)))]])

   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))))


(struct rule (lhs rhs)
        #:transparent)

(struct lhs-id (val start end)
        #:transparent)


;; A rhs can be one of the following:
(struct rhs (start end)
        #:transparent)

(struct rhs-id rhs (val)
        #:transparent)

(struct rhs-token rhs (val)
        #:transparent)

(struct rhs-lit rhs (val)
        #:transparent)

(struct rhs-choice rhs (vals)
        #:transparent)

(struct rhs-repeat rhs (min ;; either 0 or 1
                        val)
        #:transparent)

(struct rhs-maybe rhs ( val)
        #:transparent)

(struct rhs-seq rhs (vals)
        #:transparent)



; token-id: string -> boolean
;; Produces true if the id we see should be treated as the name of a token.
;; By convention, tokens are all upper-cased.
(define (token-id? id)
  (string=? (string-upcase id)
            id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; During parsing, we can define the source of the input.
(define current-source (make-parameter #f))


;; When bad things happen, we need to emit errors:
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
