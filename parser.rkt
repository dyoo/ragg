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
         grammar-parser)

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
   (precs)

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
      (rule $1 $3)]]

    [rhs
     [(LIT)
      (lit $1)]
     
     [(rhs PIPE rhs)
      (choice $1 $3)

      ]])
   
   
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos)))))


(struct rule (lhs rhs)
        #:transparent)

;; A rhs can be one of the following:
(struct lit (val)
        #:transparent)

(struct choice (x y)
        #:transparent)




;; When bad things happen, we need to emit errors:
(struct exn:fail:parse-grammar exn:fail (srclocs)
        #:transparent
        #:property prop:exn:srclocs (lambda (instance)
                                      (exn:fail:parse-grammar-srclocs instance)))


;; During parsing, we can define the source of the input.
(define current-source (make-parameter #f))

(define current-parser-error-handler
  (make-parameter
   (lambda (tok-ok? tok-name tok-value start-pos end-pos)
     (raise (exn:fail:parse-grammar (format "Error while parsing grammar")
                                    (list (srcloc (current-source)
                                                  (position-line start-pos)
                                                  (position-col start-pos)
                                                  (position-offset start-pos)
                                                  (if (and (number? (position-offset end-pos))
                                                           (number? (position-offset start-pos)))
                                                      (- (position-offset end-pos)
                                                         (position-offset start-pos))
                                                      #f))))))))
