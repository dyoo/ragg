#lang racket/base

;; For now, default to the LALR generator.  We do want to switch over
;; to a more robust parser generator eventually.
(require "../lalr/lang/reader.rkt")
(provide (all-from-out "../lalr/lang/reader.rkt"))
