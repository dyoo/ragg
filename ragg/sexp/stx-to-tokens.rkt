#lang racket/base

;; Given a syntax object, return a list of tokens.
;;

(provide (rename-out [public:tokenize tokenize]))

(require ragg/rules/parser
         ragg/rules/rule-structs
         (prefix-in original: ragg/rules/lexer)
         
         parser-tools/lex
         racket/port)


(define (numeric-stx? stx)
  (and (syntax? stx)
       (number? (syntax-e stx))))

(define (string-stx? stx)
  (and (syntax? stx)
       (string? (syntax-e stx))))


(define (public:tokenize stx)
  (reverse (tokenize stx '())))
                       

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
       (define-values (make-lparen make-rparen)
         (cond [(equal? shape #f)
                (values token-LPAREN token-RPAREN)]
               [(equal? shape #\[)
                (values token-LBRACKET token-RBRACKET)]))
               
                                 
                                 start-pos 
                                 (make-position (if (number? position)
                                                    (add1 position)
                                                    position)
                                                line 
                                                (if (number? col)
                                                    (add1 col)
                                                    col))))

       (for/fold ([acc/rev acc/rev])
         ([elt (in-list (syntax->list #'(elts ...)))])
         (tokenize elt acc/rev)))]
    
    [_
     (identifier? stx)
     (tokenize-identifier stx acc/rev)]

    [str
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
    [(and (string=? (symbol->string (syntax-e stx))
                    "")
          (equal? (syntax-span stx)
                  2))
     (cons (make-position-token 
            (token-PIPE "|") 
            (make-position (syntax-position stx) (syntax-line stx) (syntax-column stx))
            (make-position (add 1 (syntax-position stx)) (syntax-line (add 1 (syntax-column stx)))))
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
         (define next-pos-token (original:lex/1 ip))
         (define next-token (position-token-token next-pos-token))
         (cond
           [(eq? 'EOF (token-name next-token))
            acc/rev]
           [else
            (loop (cons next-pos-token acc/rev))])))]))
