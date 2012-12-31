#lang s-exp syntax/module-reader
ragg/lalr/ragg
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require "../../parser.rkt"
         "../../lexer.rkt"
         "../../stx.rkt")

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src in)
  (define-values (first-line first-column first-position) (port-next-location in))
  (define tokenizer (tokenize in))
  (define rules (grammar-parser tokenizer))
  (define-values (last-line last-column last-position) (port-next-location in))
  (list (rules->stx src rules 
                    #:original-stx (datum->syntax #f 'original-stx
                                                  (list src 
                                                        first-line 
                                                        first-column 
                                                        first-position
                                                        (if (and (number? last-position)
                                                                 (number? first-position))
                                                            (- last-position first-position)
                                                            #f))))))


;; Extension: we'd like to cooperate with DrRacket and tell
;; it to use the default, textual lexer and color scheme when
;; editing bf programs.
;;
;; See: http://docs.racket-lang.org/guide/language-get-info.html
;; for more details, as well as the documentation in
;; syntax/module-reader.
(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer
                      'default-lexer)]
    [else
     (default-filter key default)]))

