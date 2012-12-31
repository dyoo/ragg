#lang racket/base

(require rackunit
         ragg/support)

;; Make sure we produce proper error messages on weird grammars

(define weird-grammar-no-rules
  "#lang ragg")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (evaluate prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (eval (read-syntax #f (open-input-string prog)))))
    

(check-exn exn:fail:parsing? (lambda () (evaluate weird-grammar-no-rules)))
