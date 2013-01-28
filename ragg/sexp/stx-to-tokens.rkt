#lang racket/base

;; Given a syntax object, return a list of tokens.
;;

(provide tokenize-stxs
         tokenize)

(require ragg/rules/parser
         ragg/rules/rule-structs
         (prefix-in original: ragg/rules/lexer)
         
         parser-tools/lex
         racket/port
         racket/list)


(define (numeric-stx? stx)
  (and (syntax? stx)
       (number? (syntax-e stx))))

(define (string-stx? stx)
  (and (syntax? stx)
       (string? (syntax-e stx))))
                       

;; tokenize-stxs: (listof stx) -> (listof position-token)
(define (tokenize-stxs stxs)
  (reverse (for/fold ([acc/rev '()])
                     ([stx stxs])
             (tokenize stx acc/rev))))


;; Returns a _reversed_ list of the tokens we can extract from stx.
(define (tokenize stx acc/rev)
  (define src (syntax-source stx))
  (define line (syntax-line stx))
  (define col (syntax-column stx))
  (define position (syntax-position stx))
  (define span (syntax-span stx))
  
  (define start-pos (make-position position line col))
  (define end-pos (make-position (if (and (number? position)
                                          (number? span))
                                     (+ position span)
                                     #f)
                                 line col))
  
  (syntax-case stx ()
    [(elts ...)
     (let ([shape (syntax-property stx 'paren-shape)])
       
       (define-values (make-lparen lshape
                       make-rparen rshape)
         (cond [(equal? shape #f)
                (values token-LPAREN "(" token-RPAREN ")")]
               [(equal? shape #\[)
                (values token-LBRACKET "[" token-RBRACKET "]")]
               [else
                ((current-parser-error-handler) 
                 #f
                 (format "~a" (syntax->datum stx))
                 (format "~a" (syntax->datum stx))
                 (pos position line col)
                 (pos (add position 1) line (add col 1)))]))               
       (define-values (lparen rparen)
         (values (make-position-token (make-lparen lshape) 
                                      start-pos
                                      (make-position (add 1 position)
                                                     line
                                                     (add 1 col)))
                 (make-position-token (make-rparen rshape) 
                                      (make-position (add -1 (add position span))
                                                     line
                                                     col)
                                      end-pos)))

       (cons rparen
             (for/fold ([acc/rev (cons lparen acc/rev)])
                       ([elt (in-list (syntax->list #'(elts ...)))])
               (tokenize elt acc/rev))))]
    [_
     (identifier? stx)
     (tokenize-identifier stx acc/rev)]

    [_
     (string-stx? stx)
     (cons (position-token (token-LIT (syntax-e stx))
                     start-pos
                     end-pos)
           acc/rev)]
    [else
     ((current-parser-error-handler) 
      #f
      (format "~a" (syntax->datum stx))
      (format "~a" (syntax->datum stx))
      (pos position line col)
      (pos (add position span) line col))]))


;; add: (or number? any) (or number? any) -> (U number? #f)
;; Permissively add two things together.  If they're both numbers, we
;; get a number.  Otherwise, we get false.
(define (add x y)
  (if (and (number? x)
           (number? y))
      (+ x y)
      #f))

(define (tokenize-identifier stx acc/rev)
  ;; Check special case: if the identifier is the empty string, treat it as alternation.
  (cond
    ;; That is, if they entered in: ||
    [(and (string=? (symbol->string (syntax-e stx)) "")
          (equal? (syntax-span stx) 2))
     (cons (make-position-token 
            (token-PIPE "|") 
            (make-position (syntax-position stx) 
                           (syntax-line stx) 
                           (syntax-column stx))
            (make-position (add 1 (syntax-position stx)) 
                           (syntax-line stx)
                           (add 1 (syntax-column stx))))
           acc/rev)]
    [else
     ;; Otherwise, just lex it.
     (define ip (relocate-input-port
                 (open-input-string (symbol->string (syntax-e stx)))
                 (syntax-line stx)
                 (syntax-column stx)
                 (syntax-position stx)))
     (parameterize ([file-path (syntax-source stx)])
       (let loop ([acc/rev acc/rev])
         
         (cond [(and (equal? (peek-char ip) #\:)
                     (not (empty? acc/rev))
                     (equal? 'ID (token-name (position-token-token (first acc/rev)))))
                ;; Very special case: treat it as a rule head, and rewrite the very previous
                ;; token.
                (read-char ip)
                (define-values (l c p) (port-next-location ip))
                (define rewritten-token
                  (position-token (token-RULE_HEAD
                                   (string-append (token-value (position-token-token (first acc/rev)))
                                                  ":"))
                                  (position-token-start-pos (first acc/rev))
                                  (make-position p l c)))
                (loop (cons rewritten-token (rest acc/rev)))]
               [else
                (define next-pos-token (original:lex/1 ip))
                (define next-token (position-token-token next-pos-token))
                (cond
                  [(eq? 'EOF (token-name next-token))
                   acc/rev]
                  [else
                   (loop (cons next-pos-token acc/rev))])])))]))