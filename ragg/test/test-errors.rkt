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
    [(_ prog expected-msg)
     (syntax/loc stx
       (begin (check-exn (regexp (regexp-quote expected-msg))
                         (lambda ()
                           (c prog)))
              (check-exn exn:fail:syntax?
                         (lambda ()
                           (c prog)))))]))


(check-compile-error "#lang ragg"
                     "The grammar does not appear to have any rules")

(check-compile-error "#lang ragg\nfoo"
                     "Error while parsing grammar near: foo [line=2, column=0, position=12]")

(check-compile-error "#lang ragg\nnumber : 42"
                     "Error while parsing grammar near: 42 [line=2, column=9, position=21]")

(check-compile-error "#lang ragg\nnumber : 1"
                     "Error while parsing grammar near: 1 [line=2, column=9, position=21]")

;; Hmmm.  Should the following be an error?  We're being a bit more
;; harsh here than we probably should.
(check-compile-error "#lang ragg\nnumber : 1flarbl"
                     "Error while parsing grammar near: 1flarbl [line=2, column=9, position=21]")





;; I need to ask on nontermination of:
;;
;; #lang ragg
;; x : x y
;; y : "y"
;;
;; Looks like a bug in cfg-parser.




;; I need to ask about the behavior of:
;;
;; #lang ragg
;; x : x
;;
;; which is giving me the weird error message:
;; 
;; parser-productions: A production for a non-terminal must be (non-term right-hand-side ...) with at least 1 right hand side in: (atok)


;; We need to handle this ourselves before passing to cfg-parser.
;; What we need is an algorithm to check that a BNF grammar has at least
;; one finite derivation.  It's a graph algorithm, almost like
;; topsort...
