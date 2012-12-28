#lang racket/base

(require (prefix-in lex: parser-tools/lex))



(provide (all-defined-out))



;; During parsing, we should define the source of the input.
(define current-source (make-parameter #f))

;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parsing exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parsing-srclocs instance)))

(define (THE-ERROR-HANDLER tok-ok? tok-name tok-value start-pos end-pos)
  ((current-parser-error-handler) tok-ok? tok-name tok-value start-pos end-pos))

(define current-parser-error-handler
  (make-parameter
   (lambda (tok-ok? tok-name tok-value start-pos end-pos)
     (define-values (src line col offset span)
       (positions->srcloc start-pos end-pos))
     (raise (exn:fail:parsing
             (format "Encountered error while parsing, near: ~e [line=~a, column=~a, offset=~a]"
                     tok-value
                     line col offset)
             (current-continuation-marks)
             (list (srcloc src line col offset span)))))))


(struct tok-struct (val type offset line column span whitespace?))

(define (tok type val 
             #:offset [offset #f]
             #:line [line #f] 
             #:column [column #f]
             #:span [span #f]
             #:whitespace? [whitespace? #f])
  (tok-struct val type offset line column span whitespace?))

                      
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

;; positions->srcloc: position position -> (values source line column offset span)
(define (positions->srcloc start-pos end-pos)
  (values (current-source)
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
