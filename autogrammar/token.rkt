#lang racket/base

(provide [struct-out token-struct]
         token)

(struct token-struct (type val offset line column span whitespace?) 
        #:transparent)


;; Token constructor.
;; This is intended to be a general token structure constructor that's nice
;; to work with.
;; It should cooperate with the tokenizers constructed with make-permissive-tokenizer.
(define token
  (let ([undefined-val (cons 'undefined 'undefined)])
    (lambda (type 
             [val undefined-val]
             #:offset [offset #f]
             #:line [line #f] 
             #:column [column #f]
             #:span [span #f]
             #:whitespace? [whitespace? #f])
      (token-struct type
                    (if (eq? val undefined-val) type val)
                    offset line column span whitespace?))))
