#lang racket/base
(require "python-grammar.rkt"
         parser-tools/lex
         rackunit)


(define (tokenize/1 ip)
  (default-lex/1 ip))

(check-equal? (sort all-token-names
                    (lambda (x y)
                      (string<? (format "~s" x) (format "~s" y))))
              (sort '(EOF NAME NEWLINE INDENT ENDMARKER STRING NUMBER DEDENT
                          "%" "&" ")" "(" "+" "*" "-" "," "/" "." ";" ":" "=" "<" ">"
                          "@" "[" "^" "]" "`" "{" "~" "|" "}" "**" "//" "<<" "|=" "/="
                          "-=" "+=" "*=" ">=" "==" "<=" "^=" "&=" "%=" "!=" ">>" "<>"
                          "if" "in" "or" "is" "as" "else" "print" "not" "del" "elif" "lambda"
                          "and" "//=" "<<=" "pass" "**=" ">>=" "exec" "raise" "class" "return"
                          "while" "yield" "for" "global" "with" "continue" "def" "try" "from"
                          "assert" "break" "import" "except" "finally")
                    (lambda (x y)
                      (string<? (format "~s" x) (format "~s" y)))))


(check-equal? (tokenize/1 (open-input-string "exec"))
              (token-exec "exec"))
(check-equal? (tokenize/1 (open-input-string ">="))
              (|token->=| ">="))
(check-equal? (tokenize/1 (open-input-string "%"))
              (|token-%| "%"))
(check-equal? (tokenize/1 (open-input-string ""))
              (token-EOF eof))




(parse "hello.py" tokenize/1)