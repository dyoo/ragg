#lang racket
(require "python-grammar.rkt"
         (planet dyoo/python-tokenizer)
         racket/generator
         parser-tools/lex
         racket/match
         rackunit)


(define (tokenize/1 ip)
  (default-lex/1 ip))


(check-equal? (sort (for/list ([k (in-hash-keys all-tokens-hash)]) k)
                    (lambda (x y)
                      (string<? (format "~s" x) (format "~s" y))))
              (sort `(EOF NAME NEWLINE INDENT ENDMARKER STRING NUMBER DEDENT
                          ,@(map string->symbol
                                 '("%" "&" ")" "(" "+" "*" "-" "," "/" "." ";" ":" "=" "<" ">"
                                       "@" "[" "^" "]" "`" "{" "~" "|" "}" "**" "//" "<<" "|=" "/="
                                       "-=" "+=" "*=" ">=" "==" "<=" "^=" "&=" "%=" "!=" ">>" "<>"
                                       "if" "in" "or" "is" "as" "else" "print" "not" "del" "elif" "lambda"
                                       "and" "//=" "<<=" "pass" "**=" ">>=" "exec" "raise" "class" "return"
                                       "while" "yield" "for" "global" "with" "continue" "def" "try" "from"
                                       "assert" "break" "import" "except" "finally")))
                    (lambda (x y)
                      (string<? (format "~s" x) (format "~s" y)))))


(check-equal? (position-token-token (tokenize/1 (open-input-string "exec")))
              (token-exec "exec"))
(check-equal? (position-token-token (tokenize/1 (open-input-string ">=")))
              (|token->=| ">="))
(check-equal? (position-token-token (tokenize/1 (open-input-string "%")))
              (|token-%| "%"))
(check-equal? (position-token-token (tokenize/1 (open-input-string "")))
              (token-EOF eof))



(define (adapt-python-tokenizer ip)
  (define tokens (sequence->generator (generate-tokens ip)))
  (lambda ()
    (let loop ()
      (define next-token (tokens))
      (match next-token
        [(list type text (list start-line start-col) (list end-line end-col) rest-string)
         ;; FIXME: improve the Python tokenizer to hold offsets too.
         (define start-pos (position #f start-line start-col))
         (define end-pos (position #f end-line end-col))
         (position-token (case type
                           [(NAME) 
                            (cond [(hash-has-key? all-tokens-hash (string->symbol text))
                                   ((hash-ref all-tokens-hash (string->symbol text)) text)]
                                  [else
                                   (token-NAME text)])]
                           [(OP)
                            ((hash-ref all-tokens-hash (string->symbol text)) text)]
                           [(NUMBER) 
                            (token-NUMBER text)]
                           [(STRING) 
                            (token-STRING text)]
                           [(COMMENT) 
                            (loop)]
                           [(NL NEWLINE)
                            (token-NEWLINE text)]
                           [(DEDENT) 
                            (token-DEDENT text)]
                           [(INDENT)
                            (token-INDENT text)]
                           [(ERRORTOKEN)
                            (error 'uh-oh)]
                           [(ENDMARKER) 
                            (token-ENDMARKER text)])
                         start-pos
                         end-pos)]
        [(? void)
         (token-EOF eof)]))))
  

(define sample-tokens (adapt-python-tokenizer
                          (open-input-string #<<EOF
import blah
def hello(x):
    print "hello", repr(x)
    blah.baz()

EOF
                                             )))


(pretty-write (syntax->datum (parse "hello.py" sample-tokens)))
