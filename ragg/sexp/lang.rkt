#lang racket/base

(require (for-syntax racket/base
                     racket/generator
                     syntax/strip-context
                     syntax/parse
                     (prefix-in ragg: ragg/rules/parser)
                     (prefix-in ragg: ragg/rules/rule-structs)
                     (prefix-in ragg: ragg/rules/stx)
                     (prefix-in ragg: ragg/codegen/codegen)
                     "stx-to-tokens.rkt"))

(provide (rename-out (my-module-begin #%module-begin)))


(define-syntax (my-module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     (begin 
       (define tokens (tokenize-stxs (syntax->list #'(body ...))))
       (define next-token (sequence->generator tokens))
       (define rules 
         (parameterize ([ragg:current-parser-error-handler
                         (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                           (raise-syntax-error 
                            #f
                            (format "Error while parsing grammar near: ~a [line=~a, column=~a, position=~a]"
                                    tok-value
                                    (ragg:pos-line start-pos)
                                    (ragg:pos-col start-pos)
                                    (ragg:pos-offset start-pos))
                            (datum->syntax #f
                                           (string->symbol (format "~a" tok-value))
                                           (list (syntax-source stx)
                                                 (ragg:pos-line start-pos)
                                                 (ragg:pos-col start-pos)
                                                 (ragg:pos-offset start-pos)
                                                 (if (and (number? (ragg:pos-offset end-pos))
                                                          (number? (ragg:pos-offset start-pos)))
                                                     (- (ragg:pos-offset end-pos)
                                                        (ragg:pos-offset start-pos))
                                                     #f)))))])
           (ragg:grammar-parser next-token)))
       (define rules-stx
         (ragg:rules->stx (syntax-source stx) rules #:original-stx stx))

       ;(displayln rules-stx)
       
       (quasisyntax/loc stx
         (#%plain-module-begin
          #,(ragg:rules-codegen #:parser-provider-module 'ragg/cfg-parser/cfg-parser ;; 'parser-tools/yacc 
                                #:parser-provider-form   'cfg-parser                 ;; 'parser
                                rules-stx))))]))