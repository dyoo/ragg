#lang racket/base

(provide make-and make-or (struct-out node) visit! add-child!)

(require racket/match)

;; I can't get no... satisfaction.
;;
;; A small module to make sure a small constraint system can be satisfied.


(struct node (type val yes? parents children) #:mutable)
;; or nodes are satisfied if any of the children is satisfied.
;; and nodes are satisfied if all of the children are satisfied.


;; visit!: node -> void
;; Visit a node, and marking it if it's all satisfied.
(define (visit! a-node)
  (unless (node-yes? a-node)
    (match a-node
      [(node 'or val yes? parents children)
       (when (for/or ([c (in-list children)]) (node-yes? c))
         (set-node-yes?! a-node #t)
         (for ([p (in-list parents)])
           (visit! p)))]
      [(node 'and val yes? parents children)
       (when (for/and ([c (in-list children)]) (node-yes? c))
         (set-node-yes?! a-node #t)
         (for ([p (in-list parents)])
           (visit! p)))])))


;; make-or: X -> node
;; Create an or node
(define (make-or val)
  (node 'or val #f '() '()))


;; make-and: X -> node
;; Create an and node
(define (make-and val)
  (node 'and val #f '() '()))

;; add-child!: node node -> void
;; Attach a child c to the parent node p.
(define (add-child! p c)
  (set-node-children! p (cons c (node-children p)))
  (set-node-parents! c (cons p (node-parents c))))



(module* test racket
  (require (submod "..")
           racket/block
           rackunit)
  
  (block
   ;; Self-looping "a" and-node should not say yes after visiting.
   (define a (make-and 'a))
   (add-child! a a)
   (visit! a)
   (check-false (node-yes? a)))
  
  (block
   ;; Self-looping "a" or-node should not say yes after visiting.
   (define a (make-or 'a))
   (add-child! a a)
   (visit! a)
   (check-false (node-yes? a)))
  
  (block
   ;; Empty "a" or-node should not say yes after visiting.
   (define a (make-or 'a))
   (visit! a)
   (check-false (node-yes? a)))
  
  (block
   ;; Empty "a" and-node SHOULD say yes after visiting.
   (define a (make-and 'a))
   (visit! a)
   (check-true (node-yes? a)))
  
  )