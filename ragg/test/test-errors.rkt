#lang racket/base

(require rackunit
         (for-syntax racket/base))

;; The tests in this module make sure we produce proper error messages
;; on weird grammars.


(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (c prog)
  (parameterize ([current-namespace ns]
                 [read-accept-reader #t])
    (define ip (open-input-string prog))
    (port-count-lines! ip)
    (compile (read-syntax #f ip))))
    

;; Helper to let me quickly write compile-error checks.
(define-syntax (check-compile-error stx)
  (syntax-case stx ()
    [(_ prog expected-msg)
     (quasisyntax/loc stx
       (begin #,(syntax/loc stx
                  (check-exn (regexp (regexp-quote expected-msg))
                             (lambda ()
                               (c prog))))
              #,(syntax/loc stx
                  (check-exn exn:fail:syntax?
                             (lambda ()
                               (c prog))))))]))




                     
(check-compile-error "#lang ragg"
                     "The grammar does not appear to have any rules")

(check-compile-error "#lang ragg\nfoo"
                     "Error while parsing grammar near: foo [line=2, column=0, position=12]")

(check-compile-error "#lang ragg\nnumber : 42"
                     "Error while parsing grammar near: 42 [line=2, column=9, position=21]")

(check-compile-error "#lang ragg\nnumber : 1"
                     "Error while parsing grammar near: 1 [line=2, column=9, position=21]")



(check-compile-error "#lang ragg\n x: NUMBER\nx:STRING"
                     "Rule x has a duplicate definition")

;; Check to see that missing definitions for rules also raise good syntax
;; errors:

(check-compile-error "#lang ragg\nx:y"
                     "Rule y has no definition")

(check-compile-error "#lang ragg\nnumber : 1flarbl"
                     "Rule 1flarbl has no definition")




(check-compile-error "#lang ragg\nprogram: EOF"
                     "Token EOF is reserved and can not be used in a grammar")



;; Nontermination checks:
(check-compile-error "#lang ragg\nx : x"
                     "Rule x has no finite derivation")



(check-compile-error #<<EOF
#lang ragg
x : x y
y : "y"
EOF
                     "Rule x has no finite derivation")




; This should be illegal too:
(check-compile-error #<<EOF
#lang ragg
a : "a" b
b : a | b 
EOF
                     "Rule a has no finite derivation")




(check-compile-error #<<EOF
#lang ragg
a : [b]
b : [c]
c : c
EOF
                     "Rule c has no finite derivation")


(check-compile-error #<<EOF
#lang ragg
a : [b]
b : c
c : c
EOF
                     "Rule b has no finite derivation")


(check-compile-error #<<EOF
#lang ragg
a : [a]
b : [b]
c : c
EOF
                     "Rule c has no finite derivation")




(check-compile-error #<<EOF
#lang racket/base
(require ragg/examples/simple-line-drawing)
(define bad-parser (make-rule-parser crunchy))
EOF
                     "Rule crunchy is not defined in the grammar"
                     )
