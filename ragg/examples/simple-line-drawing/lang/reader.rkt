#lang s-exp syntax/module-reader
ragg/examples/simple-line-drawing/language
#:read my-read
#:read-syntax my-read-syntax
#:info my-get-info
#:whole-body-readers? #t

(require ragg/examples/simple-line-drawing/lexer
         ragg/examples/simple-line-drawing)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))

(define (my-get-info key default default-filter)
  (case key
    [(color-lexer)
     (dynamic-require 'syntax-color/default-lexer 'default-lexer)]
    [else
     (default-filter key default)]))
