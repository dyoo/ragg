#lang racket/base

(require rackunit
         (for-syntax racket/base))

;; Make sure we produce proper error messages on weird grammars


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (c prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (define ip (open-input-string prog))
    (port-count-lines! ip)
    (compile (read-syntax #f ip))))
    

(define-syntax (check-compile-error stx)
  (syntax-case stx ()
    [(_ msg prog)
     (syntax/loc stx
       (begin (check-exn (regexp (regexp-quote msg))
                         (lambda ()
                           (c prog)))
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (c prog)))))]))


(check-compile-error "The grammar does not appear to have any rules"
                     "#lang ragg")

(check-compile-error "Error while parsing grammar near: \"foo\" [line=2, column=0, position=12]"
                     "#lang ragg\nfoo")
