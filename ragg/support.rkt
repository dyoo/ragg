#lang racket/base

(provide [struct-out token-struct]
         token
         [struct-out exn:fail:parsing])

(struct token-struct (type val offset line column span skip?) 
        #:transparent)


;; Token constructor.
;; This is intended to be a general token structure constructor that's nice
;; to work with.
;; It should cooperate with the tokenizers constructed with make-permissive-tokenizer.
(define token
  (lambda (type                 ;; (U symbol string)
           [val #f]  ;; any
           #:offset [offset #f] ;; (U #f number)
           #:line [line #f]     ;; (U #f number)
           #:column [column #f] ;; (U #f number)
           #:span [span #f]     ;; boolean
           #:skip? [skip? #f])
    (token-struct (if (string? type) (string->symbol type) type)
                  val
                  offset line column span skip?)))


;; When bad things happen, we need to emit errors with source location.
(struct exn:fail:parsing exn:fail (srclocs)
  #:transparent
  #:property prop:exn:srclocs (lambda (instance)
                                (exn:fail:parsing-srclocs instance)))





