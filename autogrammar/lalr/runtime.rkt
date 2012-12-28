#lang racket/base

(require parser-tools/lex)



(provide (all-defined-out))



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

;; positions->srcloc: position position -> (list source line column offset span)
(define (positions->srcloc start-pos end-pos)
  (list (current-source)
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (if (and (number? (position-offset end-pos))
                 (number? (position-offset start-pos)))
            (- (position-offset end-pos)
               (position-offset start-pos))
            #f)))

;; d->s: datum position position
;; Helper that does the ugly work in wrapping a datum into a syntax
;; with source location.
(define (d->s d start-pos end-pos)
  (datum->syntax #f d (positions->srcloc start-pos end-pos)))
