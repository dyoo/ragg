#lang racket/base

;; A language level for automatically generating parser grammars for
;; parser-tools/yacc.
;;
;; Danny Yoo (dyoo@hashcollision.org)
;;
;; Intent: make it trivial to generate languages for Racket.  At the
;; moment, I find it painful to use parser-tools.  This library is
;; meant to make it less agonizing.
;;
;; The intended use of this language is as follows:
;;
;;;;; s-exp-grammar.rkt ;;;;;;;;;
;; #lang ragg
;; s-exp : "(" s-exp* ")" | ATOM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; What this generates is a module that binds constructors associated
;; to the token types in upper-case, plus the following:
;;
;;     * grammar: a grammar that consumes a source and a
;;       position-aware lexer, and produces a syntax object.
;;
;;     * default-lex/1: a partially-defined lexer that knows how to read
;;       the literal strings.  You'll use this to create the full lexer.
;; 
;; You'll still need to do a little work, by providing a lexer that
;; defines what the uppercased tokens mean.
;;
;; (require "sexp-grammar.rkt"
;;          parser-tools/lex
;;          parser-tools/lex-sre)
;;
;; (define tokenize/1
;;   (lexer-src-pos
;;     [(:+ alphabetic)
;;      (token-ATOM lexeme)]
;;     [whitespace
;;      (return-without-pos (tokenize/1 input-port))]
;;     [else
;;      (return-without-pos (default-lex/1 input-port))]))
;;

;; However, that should be all you need.  The output of an
;; generated grammar is an honest-to-goodness syntax
;; object with source locations, fully-labeled by the rules.
;;
;; (grammar "some-source" tokenize/1)
;;
;;
;; The first rule is treated as the start rule; any successful parse
;; must finish with end-of-file.



;; Terminology:
;;
;;   * An identifier follows the Racket rules for identifiers, except
;;      that it can't contain * or +.
;;
;;   *  A rule identifier is an identifier that is not in upper case.
;;
;;   *  A token is an identifier that is all in upper case.


;; A rule is:
;;
;;   *  a rule identifier, followed by a colon ":", followed by a pattern.


;;
;; A pattern may either be 
;;
;;   * an implicit sequence of patterns,
;;
;;   * a literal string,
;;
;;   * a rule identifier,
;;
;;   * a quanitifed pattern, either with "*" or "+",
;;
;;   * an optional pattern, a pattern surrounded by "[" and "]", or
;;
;;   * an explicit sequence, a pattern surrounded by "(" and ")".


;;
;; TODO: handle precedence
;;

(require (for-syntax racket/base
                     "codegen.rkt"))

(provide rules (rename-out [#%plain-module-begin #%module-begin]))
(define-syntax (rules stx)
  (rules-codegen #:parser-provider-module 'ragg/cfg-parser/cfg-parser #;'parser-tools/yacc 
                 stx))
