#lang racket/base
(require (for-syntax racket/base syntax/parse))

(provide drawing rows chunk 
         #%module-begin
         #%datum ;; We reuse Racket's treatment of raw datums, specifically for strings and numbers
         )

(define-syntax (drawing drawing-stx)
  (syntax-parse drawing-stx
    [({~literal drawing} row-stxs ...)

     (syntax/loc drawing-stx
       (begin row-stxs ...))]))


(define-syntax (rows row-stx)
  (syntax-parse row-stx
    [({~literal rows}
      ({~literal repeat} repeat-number)
      chunks ... ";")

     (syntax/loc row-stx
       (begin
         (for ([i (in-range repeat-number)])
           chunks ...
           (newline))))]))


(define-syntax (chunk chunk-stx)
  (syntax-parse chunk-stx
    [({~literal chunk} chunk-size chunk-string)

     (syntax/loc chunk-stx
       (for ([k chunk-size])
         (display chunk-string)))]))
