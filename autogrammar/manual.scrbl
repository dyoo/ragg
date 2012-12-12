#lang scribble/manual
@(require scribble/eval
          (for-label racket))


@title{Autogrammar: generate parsers automatically}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@section{Informal Quickstart}

@(define informal-eval (make-base-eval))
@(informal-eval '(require (planet dyoo/autogrammar/examples/nested-word-list)))
@(informal-eval '(require racket/list))
Let's consider: if we're given a string like the following:
@racketblock["(hello (world (this is a (test))))"]


@margin-note{(... and pretend that we don't already know about
the built-in @racket[read] function.)}
how do we go about turn this string into a structured value?  That is, how
would we @emph{parse} it?


We first consider the shape of the things we'd like to parse.  The thing above
looks like a deeply nested list of words.  How might we describe this formally?
A convenient notation to describe the shape of these things is
@link["http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form"]{Backus-Naur
Form} (BNF).


Let's try to notate the structure of nested word lists in BNF then:
@nested[#:style 'code-inset]{
@verbatim{
nested-word-list: LEFT-PAREN nested-word-list* RIGHT-PAREN
                | WORD
}}

In this notation, we treat @racket[LEFT-PAREN], @racket[RIGHT-PAREN], and
@racket[WORD] as placeholders for atomic, non-structured things, or
@emph{tokens}.  We also use @racket[*] to represent zero or more of the
previous thing.


At this point, we've got a grammar in hand, but we don't yet have a
@emph{program} to take that grammar and use it to parse strings.


Let's fix this problem:

@filebox["nested-word-list.rkt"]{
@verbatim{
#lang planet dyoo/autogrammar
nested-word-list: LEFT-PAREN nested-word-list* RIGHT-PAREN
                | WORD
}}

Ok... now it's a program.  But what does it do?

@interaction[#:eval informal-eval
@eval:alts[(require "nested-word-list.rkt") (void)]
parse
]
It gives us a @racket[parse] function.  What else is in there?
@interaction[#:eval informal-eval
@eval:alts[(module->exports "nested-word-list.rkt")
           (module->exports '(planet dyoo/autogrammar/examples/nested-word-list))]]

Hmmm... It appears to have quite a few things for us, such as
@racket[parse], @racket[token-LEFT-PAREN], @racket[token-RIGHT-PAREN],
@racket[token-WORD], and @racket[token-EOF].

Let's try using the @racket[parse] function in @filepath{nested-word-list.rkt}.

@interaction[#:eval informal-eval
@code:comment{A parser needs a source of tokens.  Here's one:}
(define (make-tokenizer s)
  (define tokens (regexp-match* #px"\\(|\\)|\\w+" s))
  (lambda ()
    (cond [(empty? tokens)
           (token-EOF #f)]
          [else
           (define next-token
             (cond
               [(string=? (first tokens) "(")
                (token-LEFT-PAREN (first tokens))]
               [(string=? (first tokens) ")")
                (token-RIGHT-PAREN (first tokens))]
               [else
                (token-WORD (first tokens))]))
           (set! tokens (rest tokens))
           next-token])))
@code:comment{For example:}
(define token-source (make-tokenizer "(hello (world (this is a (test))))"))
@code:comment{Let's try parser out on token-source:}
(parse token-source)
]

Wait... that looks like a syntax object!

@interaction[#:eval informal-eval
(define v (parse (make-tokenizer "(hello (world (this is a (test))))")))
(syntax->datum v)
]

Welcome to @tt{autogrammar}.


@close-eval[informal-eval]




@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Introduction}



@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



@section{API}
