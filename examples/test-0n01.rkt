#lang racket/base
(require "0n1n.rkt"
         rackunit)


(define (lex ip)
  (port-count-lines! ip)
  (lambda ()
    (default-lex/1 ip)))

(parse #f (lex (open-input-string "0011")))
(parse #f (lex (open-input-string "01")))
(parse #f (lex (open-input-string "")))
(parse #f (lex (open-input-string "000111")))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001111")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "0001110")))))

(check-exn exn:fail:parsing?
           (lambda () (parse #f (lex (open-input-string "10001110")))))