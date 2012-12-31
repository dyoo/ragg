#lang racket/base

(require rackunit)

;; Make sure we produce proper error messages on weird grammars


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (c prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (compile (read-syntax #f (open-input-string prog)))))
    

(define weird-grammar-no-rules
  "#lang ragg")
(check-exn #px"The grammar does not appear to have any rules"
           (lambda () (c weird-grammar-no-rules)))
           
