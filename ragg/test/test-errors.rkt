#lang racket/base

(require rackunit
         ragg/support)

;; Make sure we produce proper error messages on weird grammars


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (evaluate prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (compile (read-syntax #f (open-input-string prog)))))
    


(define weird-grammar-no-rules
  "#lang ragg")
(check-exn exn:fail:parsing-no-rules? (lambda () (evaluate weird-grammar-no-rules)))
