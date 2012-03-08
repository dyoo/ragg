#lang racket/base
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "parser.rkt")

(provide lex/1 tokenize)

;; A newline can be any one of the following.
(define-lex-abbrev NL (:or "\r\n" "\r" "\n"))

;; Slightly modified from the read.rkt example in parser-tools, treating
;; +, :, and * as reserved, non-identifier characters.
(define-lex-abbrevs
   [letter (:or (:/ "a" "z") (:/ #\A #\Z))]
   [digit (:/ #\0 #\9)]
   [initial (:or letter (char-set "!$%&/<=>?^_~@"))]
   [subsequent (:or initial digit (char-set "-.@"))])

(define-lex-abbrev id
  (:or (:: initial (:* subsequent))))

(define lex/1
  (lexer-src-pos
   [(:: "'"
        (:* (:or "\\'" (:~ "'" "\\")))
        "'")
    (token-LIT lexeme)]
   [(:: "\""
        (:* (:or "\\\"" (:~ "\"" "\\")))
        "\"")
    (token-LIT lexeme)]
   ["("
    (token-LPAREN lexeme)]
   ["["
    (token-LBRACKET lexeme)]
   [")"
    (token-RPAREN lexeme)]
   ["]"
    (token-RBRACKET lexeme)]
   ["|"
    (token-PIPE lexeme)]
   [(:or "+" "*")
    (token-REPEAT lexeme)]
   [whitespace
    ;; Skip whitespace
    (return-without-pos (lex/1 input-port))]
   [(:: "#"
        (complement (:: (:* any-char) NL (:* any-char)))
        NL)
    ;; Skip comments up to end of line.
    (return-without-pos (lex/1 input-port))]
   [(eof)
    (token-EOF lexeme)]
   [(:: id (:* whitespace) ":")
    (token-RULE_HEAD lexeme)]
   [id
    (token-ID lexeme)]))


;; tokenize: input-port -> (-> token)
(define (tokenize ip)
  (lambda ()
    (lex/1 ip)))
