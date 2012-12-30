#lang racket/base

(require racket/match 
         (prefix-in lex: parser-tools/lex) 
         "../token.rkt")


(provide (all-defined-out))



;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))



;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parsing exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parsing-srclocs instance)))


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

(define current-parser-error-handler
  (make-parameter
   (lambda (tok-name tok-value offset line col span)
     (raise (exn:fail:parsing
                (format "Encountered parsing error near token ~e (~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                        tok-name tok-value
                        (current-source)
                        line col offset)
                (current-continuation-marks)
                (list (srcloc (current-source) line col offset span)))))))


(define current-tokenizer-error-handler
  (make-parameter
   (lambda (tok-type tok-value offset line column span)
     (raise (exn:fail:parsing
             (format "Encountered unexpected token ~e (~e) while parsing ~e [line=~a, column=~a, offset=~a]"
                     tok-type
                     tok-value
                     (current-source)
                     line column offset)
             (current-continuation-marks)
             (list (srcloc (current-source) line column offset span)))))))




;; make-permissive-tokenizer: (-> (U token Token eof)) hash -> (-> position-token)
(define (make-permissive-tokenizer tokenizer token-type-hash)
  (define (permissive-tokenizer)
    (define next-token (tokenizer))
    (match next-token
      [(? eof-object?)
       (lex:position-token ((hash-ref token-type-hash 'EOF) eof)
                           (lex:position #f #f #f)
                           (lex:position #f #f #f))]
      
      [(token-struct type val offset line column span whitespace?)
       (cond [whitespace?
              ;; skip whitespace, and just tokenize again.
              (permissive-tokenizer)]
             
             [(hash-has-key? token-type-hash type)
              (lex:position-token ((hash-ref token-type-hash type) val)
                                  (lex:position offset line column)
                                  ;; try to synthesize a consistent end position.
                                  (lex:position (if (and (number? offset) (number? span))
                                                    (+ offset span)
                                                    offset)
                                                line
                                                (if (and (number? column) (number? span))
                                                    (+ column span)
                                                    column)))]
             [else
              ;; We ran into a token of unrecognized type.  Let's raise an appropriate error.
              ((current-tokenizer-error-handler) type val 
               offset line column span)])]
      
      [(lex:position-token (token-struct type val offset line column span whitespace?)
                           (lex:position start-offset start-line start-col)
                           (lex:position end-offset end-line end-col))
       (cond [whitespace?
              ;; Skip whitespace
              (permissive-tokenizer)]
             [(hash-has-key? token-type-hash type)
              (lex:position-token ((hash-ref token-type-hash type) val)
                                  (lex:position (or offset start-offset)
                                                (or line start-line)
                                                (or column start-col))
                                  (if (and (number? offset) (number? span))
                                      (lex:position (+ offset span) line (+ column span))
                                      (lex:position end-offset end-line end-col)))]
             [else
              ;; We ran into a token of unrecognized type.  Let's raise an appropriate error.
              ((current-tokenizer-error-handler)
               type val
               start-offset start-line start-col 
               (if (and (number? start-offset)
                        (number? end-offset))
                   (- end-offset start-offset)
                   #f))])]
      
      [else
       (coerse-to-position-token next-token)]))
  permissive-tokenizer)

                      
;; If someone feeds us a token that has no positional information,
;; just force it into the right shape.
(define (coerse-to-position-token t)
  (cond
    [(lex:position-token? t)
     t]
    [else
     (lex:position-token t
                         (lex:position #f #f #f)
                         (lex:position #f #f #f))]))


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

;; d->s: datum position position
;; Helper that does the ugly work in wrapping a datum into a syntax
;; with source location.
(define (d->s d start-pos end-pos)
  (datum->syntax #f d (positions->srcloc start-pos end-pos)))
