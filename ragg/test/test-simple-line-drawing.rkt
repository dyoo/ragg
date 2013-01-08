#lang racket/base

(require ragg/examples/simple-line-drawing
         ragg/support
         racket/list
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         rackunit)

(define (make-tokenizer ip)
  (port-count-lines! ip)
  (define lex (lexer-src-pos 
               [(:+ numeric)
                (token 'INTEGER (string->number lexeme))]
               [upper-case
                (token 'STRING lexeme)]
               ["b"
                (token 'STRING " ")]
               ["\n"
                (token 'NEWLINE)]
               [" "
                (token 'WHITESPACE #:skip? #t)]
               [(eof)
                (void)]))
  (lambda ()
    (lex ip)))



(define the-parsed-object-stx
  (parse (make-tokenizer (open-input-string #<<EOF
3 9 X
6 3 b 3 X 3 b
3 9 X
EOF
))))

(check-equal? (syntax->datum the-parsed-object-stx)
              '(drawing (rows (repeat 3) (chunk 9 "X") (end #f))
                        (rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") (end #f))
                        (rows (repeat 3) (chunk 9 "X") (end))))

(define the-parsed-object (syntax->list the-parsed-object-stx))

(check-equal? (syntax-line the-parsed-object-stx) 1)
(check-equal? (syntax-column the-parsed-object-stx) 0)
(check-equal? (syntax-position the-parsed-object-stx) 1)
(check-equal? (syntax-span the-parsed-object-stx) 25)

(check-equal? (length the-parsed-object) 4)

(check-equal? (syntax->datum (second the-parsed-object))
              '(rows (repeat 3) (chunk 9 "X") (end #f)))
(check-equal? (syntax-line (list-ref the-parsed-object 1)) 1)

(check-equal? (syntax->datum (third the-parsed-object))
              '(rows (repeat 6) (chunk 3 " ") (chunk 3 "X") (chunk 3 " ") (end #f)))
(check-equal? (syntax-line (list-ref the-parsed-object 2)) 2)

(check-equal? (syntax->datum (fourth the-parsed-object))
              '(rows (repeat 3) (chunk 9 "X") (end)))
(check-equal? (syntax-line (list-ref the-parsed-object 3)) 3)

;; FIXME: add tests to make sure location is as we expect.
;;
;; FIXME: handle the EOF issue better.  Something in cfg-parser
;; appears to deviate from parser-tools/yacc with regards to the stop
;; token.
