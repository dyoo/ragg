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

(struct pattern-token pattern (val)
        #:transparent)

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

