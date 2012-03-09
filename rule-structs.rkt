#lang racket/base
(provide (all-defined-out))

(struct rule (start end lhs pattern)
        #:transparent)

(struct lhs-id (start end val)
        #:transparent)


;; A pattern can be one of the following:
(struct pattern (start end)
        #:transparent)

(struct pattern-id pattern (val)
        #:transparent)


;; Token structure to be defined by the user
(struct pattern-token pattern (val)
        #:transparent)

;; Token structure defined as the literal string to be matched.
(struct pattern-lit pattern (val)
        #:transparent)

(struct pattern-choice pattern (vals)
        #:transparent)

(struct pattern-repeat pattern (min ;; either 0 or 1
                        val)
        #:transparent)

(struct pattern-maybe pattern (val)
        #:transparent)

(struct pattern-seq pattern (vals)
        #:transparent)

