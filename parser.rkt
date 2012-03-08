#lang racket/base
(require parser-tools/yacc
         parser-tools/lex)

;; A parser for grammars.

(provide tokens
         token-LPAREN
         token-RPAREN
         token-COLON
         token-PIPE
         token-REPEAT
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
         [struct-out rhs-choice])

(define-tokens tokens (LPAREN
                       RPAREN
                       COLON
                       PIPE
                       REPEAT
                       ID
                       LIT
                       EOF))



(define grammar-parser
  (parser
   (tokens tokens)
   (src-pos)
   (start rules)
   (end EOF)
   (precs [left REPEAT]
          [left PIPE])

   (grammar
    [rules
     [(rules*) $1]]

    [rules*
     [(rule rules*)
      (cons $1 $2)]
     [()
      '()]]

    [rule
     [(ID COLON rhs)
      (rule (lhs-id $1 (position-offset $1-start-pos) (position-offset $1-end-pos))
            $3)]]

    [rhs     
     [(LIT)
      (rhs-lit $1 (position-offset $1-start-pos) (position-offset $1-end-pos))]

     [(ID)
      (if (token-id? $1)
          (rhs-token $1 (position-offset $1-start-pos) (position-offset $1-end-pos))
          (rhs-id $1 (position-offset $1-start-pos) (position-offset $1-end-pos)))]
     
     [(rhs PIPE rhs)
      (rhs-choice $1 $3 (position-offset $1-start-pos) (position-offset $3-end-pos))]])

   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))))


(struct rule (lhs rhs)
        #:transparent)

(struct lhs-id (val start end)
        #:transparent)


;; A rhs can be one of the following:
(struct rhs-id (val start end)
        #:transparent)

(struct rhs-token (val start end)
        #:transparent)

(struct rhs-lit (val start end)
        #:transparent)

(struct rhs-choice (x y start end)
        #:transparent)



;; Selectors to get the start and end of a rhs.
(define (rhs-start rhs)
  (cond
   [(rhs-id? rhs) (rhs-id-start rhs)]
   [(rhs-token? rhs) (rhs-token-start rhs)]
   [(rhs-lit? rhs) (rhs-lit-start rhs)]
   [(rhs-choice? rhs) (rhs-choice-start rhs)]))

(define (rhs-end rhs)
  (cond
   [(rhs-id? rhs) (rhs-id-end rhs)]
   [(rhs-token? rhs) (rhs-token-end rhs)]
   [(rhs-lit? rhs) (rhs-lit-end rhs)]
   [(rhs-choice? rhs) (rhs-choice-end rhs)]))





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
             (format "Error while parsing grammar near: ~e [line=~a, column~a, position=~a}"
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
