#lang racket/base

(require racket/match 
         racket/list
         racket/generator
         (prefix-in lex: parser-tools/lex) 
         ragg/support
         ragg/private/internal-support)


(provide THE-ERROR-HANDLER 
         make-permissive-tokenizer
         atomic-datum->syntax
         positions->srcloc
         rule-components->syntax)



;; The level of indirection here is necessary since the yacc grammar wants a
;; function value for the error handler up front.  We want to delay that decision
;; till parse time.
(define (THE-ERROR-HANDLER tok-ok? tok-name tok-value start-pos end-pos)
  (match (positions->srcloc start-pos end-pos)
    [(list src line col offset span)
     ((current-parser-error-handler) tok-name 
                                     tok-value
                                     offset
                                     line
                                     col 
                                     span)]))




(define no-position (lex:position #f #f #f))
(define (no-position? p)
  (not 
   (or (lex:position-line p)
       (lex:position-col p)
       (lex:position-offset p))))


;; make-permissive-tokenizer: (U (sequenceof (U token token-struct eof void)) (-> (U token token-struct eof void))) hash -> (-> position-token)
;; Creates a tokenizer from the given value.
;; FIXME: clean up code.
(define (make-permissive-tokenizer tokenizer token-type-hash)
  (define tokenizer-thunk (cond
                           [(sequence? tokenizer)
                            (sequence->generator tokenizer)]
                           [(procedure? tokenizer)
                            tokenizer]))

  ;; lookup: symbol any pos pos -> position-token
  (define (lookup type val start-pos end-pos)
    (lex:position-token
     ((hash-ref token-type-hash type
               (lambda ()
                 ((current-tokenizer-error-handler) (format "~a" type) val
                  (lex:position-offset start-pos)
                  (lex:position-line start-pos)
                  (lex:position-col start-pos)
                  (and (number? (lex:position-offset start-pos))
                       (number? (lex:position-offset end-pos))
                       (- (lex:position-offset end-pos) 
                          (lex:position-offset start-pos))))))
      val)
     start-pos end-pos))

  (define (permissive-tokenizer)
    (define next-token (tokenizer-thunk))
    (let loop ([next-token next-token])
    (match next-token
      [(or (? eof-object?) (? void?))
       (lookup 'EOF eof no-position no-position)]

      [(? symbol?)
       (lookup next-token next-token no-position no-position)]

      [(? string?)
       (lookup (string->symbol next-token) next-token no-position no-position)]

      [(? char?)
       (lookup (string->symbol (string next-token)) next-token no-position no-position)]
      
      [(token-struct type val offset line column span skip?)
       (cond [skip?
              ;; skip whitespace, and just tokenize again.
              (permissive-tokenizer)]
             
             [(hash-has-key? token-type-hash type)
              (define start-pos (lex:position offset line column))
              ;; try to synthesize a consistent end position.
              (define end-pos (lex:position (if (and (number? offset) (number? span))
                                                    (+ offset span)
                                                    offset)
                                                line
                                                (if (and (number? column) (number? span))
                                                    (+ column span)
                                                    column)))
              (lookup type val start-pos end-pos)]
             [else
              ;; We ran into a token of unrecognized type.  Let's raise an appropriate error.
              ((current-tokenizer-error-handler) type val 
               offset line column span)])]
      
      [(lex:position-token t s e)
       (define a-position-token (loop t))
       (lex:position-token (lex:position-token-token a-position-token)
                           (if (no-position? (lex:position-token-start-pos a-position-token))
                               s
                               (lex:position-token-start-pos a-position-token))
                           (if (no-position? (lex:position-token-end-pos a-position-token))
                               e
                               (lex:position-token-end-pos a-position-token)))]
      
      [else
       ;; Otherwise, we have no idea how to treat this as a token.
       ((current-tokenizer-error-handler) 'unknown-type (format "~a" next-token)
        #f #f #f #f)])))
  permissive-tokenizer)

                      

;; positions->srcloc: position position -> (list source line column offset span)
;; Given two positions, returns a srcloc-like structure, where srcloc is the value
;; consumed as the third argument to datum->syntax.
(define (positions->srcloc start-pos end-pos)
  (list (current-source)
        (lex:position-line start-pos)
        (lex:position-col start-pos)
        (lex:position-offset start-pos)
        (if (and (number? (lex:position-offset end-pos))
                 (number? (lex:position-offset start-pos)))
            (- (lex:position-offset end-pos)
               (lex:position-offset start-pos))
            #f)))


;; We create a syntax using read-syntax; by definition, it should have the
;; original? property set to #t, which we then copy over to syntaxes constructed
;; with atomic-datum->syntax and rule-components->syntax.
(define stx-with-original?-property
  (read-syntax #f (open-input-string "original")))


;; atomic-datum->syntax: datum position position
;; Helper that does the ugly work in wrapping a datum into a syntax
;; with source location.
(define (atomic-datum->syntax d start-pos end-pos)
  (datum->syntax #f d (positions->srcloc start-pos end-pos) stx-with-original?-property))



;; rule-components->syntax: (U symbol false) (listof stx) ... #:srcloc (U #f (list src line column offset span)) -> stx
;; Creates an stx out of the rule name and its components.
;; The location information of the rule spans that of its components.
(define (rule-components->syntax rule-name/false #:srcloc [srcloc #f] . components)
  (define flattened-components (apply append components))
  (datum->syntax #f 
                 (apply append
                        (list 
                         (datum->syntax #f rule-name/false srcloc stx-with-original?-property))
                        components)
                 srcloc
                 stx-with-original?-property))
