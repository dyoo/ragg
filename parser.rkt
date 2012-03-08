#lang racket/base
(require parser-tools/yacc
         parser-tools/lex
         racket/list)

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
         [struct-out rhs-choice]
         [struct-out rhs-repeat]
         [struct-out rhs-seq])

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
   (precs [left PIPE])

   (grammar
    [rules
     [(rules*) $1]]

    [rules*
     [(rule rules*)
      (cons $1 $2)]
     [()
      '()]]

    [rule
     [(ID COLON rhs+)
      (rule (lhs-id (position-offset $1-start-pos) (position-offset $1-end-pos) $1)
            (if (> (length $3) 1)
                (rhs-seq (rhs-start (first $3))
                         (rhs-end (last $3))
                         $3)
                (first $3)))]]

    [rhs+
     [(rhs rhs+) (cons $1 $2)]
     [(rhs) (list $1)]]
    
    
    [rhs     
     [(LIT)
      (rhs-lit (position-offset $1-start-pos) (position-offset $1-end-pos) $1)]

     [(ID)
      (if (token-id? $1)
          (rhs-token (position-offset $1-start-pos) (position-offset $1-end-pos) $1)
          (rhs-id (position-offset $1-start-pos) (position-offset $1-end-pos) $1))]
     
     [(rhs PIPE rhs)
      (rhs-choice (position-offset $1-start-pos) (position-offset $3-end-pos) $1 $3)]])

   
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

(struct rhs-choice rhs (x y)
        #:transparent)

(struct rhs-repeat rhs (val)
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
