#lang racket
(require ragg/examples/python-grammar
         ragg/support
         (planet dyoo/python-tokenizer)
         racket/generator
         parser-tools/lex
         racket/match
         rackunit)



(define (kludge-nl-dedent-endmarker toks)
  ;; Kludge!  If the last three tokens in the stream are:
  ;;     NL, DEDENT, ENDMARKER,
  ;; then switch them around to:
  ;;     DEDENT, NEWLINE, ENDMARKER 
  ;; The Python tokenizer is doing something funny here, and I think
  ;; it's a bug in tokenize.py (and, transitively, the python-tokenizer
  ;; PLaneT package).
  (cond [(< (length toks) 3)
         toks]
        [else
         (define last-three-toks (take-right toks 3))
         (match last-three-toks
           [(list (list 'NL nl-text start-loc end-loc rest-str)
                  (and t2 (list 'DEDENT _ ...))
                  (and t3 (list 'ENDMARKER _ ...)))
            (append (drop-right toks 3)
                    (list t2 
                          (list 'NEWLINE nl-text start-loc end-loc rest-str)
                          t3))]
           [else
            toks])]))

(define (adapt-python-tokenizer ip #:end-marker-to-eof? [end-marker-to-eof? #f])
  (define generated-tokens (kludge-nl-dedent-endmarker
                            (sequence->list (generate-tokens ip))))
  (define tokens (sequence->generator generated-tokens))
  (lambda ()
    (let loop ()
      (define next-token (tokens))
      (match next-token
        [(list type text (list start-line start-col) (list end-line end-col) rest-string)
         ;; FIXME: improve the Python tokenizer to hold offsets too.
         (define start-pos (position #f start-line start-col))
         (define end-pos (position #f end-line end-col))
         (cond
          [(eq? type 'NL)
           ;; Skip over NL tokens: they are meant to represent the continuation
           ;; of a logical line.
           (loop)]
          [else
           (position-token (case type
                             [(NAME) 
                              (cond [(set-member? all-token-types (string->symbol text))
                                     (token (string->symbol text) text)]
                                    [else
                                     (token 'NAME text)])]
                             [(OP)
                              (token (string->symbol text) text)]
                             [(NUMBER) 
                              (token 'NUMBER text)]
                             [(STRING) 
                              (token 'STRING text)]
                             [(COMMENT) 
                              (token 'WHITESPACE #:skip? #t)]
                             [(NEWLINE)
                              (token 'NEWLINE text)]
                             [(DEDENT) 
                              (token 'DEDENT text)]
                             [(INDENT)
                              (token 'INDENT text)]
                             [(ERRORTOKEN)
                              (error 'uh-oh)]
                             [(ENDMARKER) 
                              (if end-marker-to-eof?
                                  (token eof)
                                  (token 'ENDMARKER text))])
                           start-pos
                           end-pos)])]
        [(? void)
         (token eof)]))))
  

(define sample-tokens (adapt-python-tokenizer
                          (open-input-string #<<EOF
import blah
def hello(x):
    print "hello", repr(x)
    blah.baz()

EOF
                                             )))


(void #;pretty-write 
 (syntax->datum (parse "hello.py" sample-tokens)))




(define parse-expr (make-rule-parser expr))


(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "42")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "42"))))))))))



(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "(lambda x,y: y,x)")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "(" (testlist_comp (test (lambdef "lambda" (varargslist (fpdef "x") "," (fpdef "y")) ":" (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "y")))))))))))))))) "," (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "x"))))))))))))))) ")"))))))))))


(check-equal?
 (syntax->datum (parse-expr 
                 (adapt-python-tokenizer (open-input-string "sqrt(x^2+y^2)")
                                         #:end-marker-to-eof? #t)))
 '(expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "sqrt") (trailer "(" (arglist (argument (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "x"))))))) "^" (and_expr (shift_expr (arith_expr (term (factor (power (atom "2")))) "+" (term (factor (power (atom "y"))))))) "^" (and_expr (shift_expr (arith_expr (term (factor (power (atom "2")))))))))))))))) ")"))))))))))



(define parse-single-input (make-rule-parser single_input))
(check-equal?
 (syntax->datum 
  (parse-single-input
   (adapt-python-tokenizer (open-input-string "def f(x):\n    return x*x\n\n")
                           #:end-marker-to-eof? #t)))
 '(single_input 
   (compound_stmt
    (funcdef "def" "f" (parameters "(" (varargslist (fpdef "x")) ")") ":" (suite "\n" "    " (stmt (simple_stmt (small_stmt (flow_stmt (return_stmt "return" (testlist (test (or_test (and_test (not_test (comparison (expr (xor_expr (and_expr (shift_expr (arith_expr (term (factor (power (atom "x"))) "*" (factor (power (atom "x")))))))))))))))))) "\n")) ""))) "\n"))
