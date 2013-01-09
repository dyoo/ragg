#lang racket/base
(require syntax/parse)

(provide interpret)

(define (interpret drawing-stx)
  (syntax-parse drawing-stx
    [({~literal drawing} row-stxs ...)

     (for ([row-stx (syntax->list #'(row-stxs ...))])
       (interpret-row row-stx))]))

(define (interpret-row row-stx)
  (syntax-parse row-stx
    [({~literal rows}
      ({~literal repeat} repeat-number:number)
      ({~literal chunk} chunk-size:number chunk-string:str) ... ";")

     (for ([i (in-range (syntax-e #'repeat-number))])
       (for ([n-stx (syntax->list #'(chunk-size ...))]
             [s-stx (syntax->list #'(chunk-string ...))])
         (for ([k (syntax-e n-stx)])
           (display (syntax-e s-stx))))
       (newline))]))

