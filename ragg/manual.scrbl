#lang scribble/manual
@(require scribble/eval
          racket/date
          file/md5
          (for-label racket
                     ragg/support
                     ragg/examples/nested-word-list
                     (only-in parser-tools/lex lexer-src-pos)
                     (only-in syntax/parse syntax-parse ~literal)))


@(define (lookup-date filename [default ""])
   (cond
     [(file-exists? filename)
      (define modify-seconds (file-or-directory-modify-seconds filename))
      (define a-date (seconds->date modify-seconds))
      (date->string a-date)]
     [else
      default]))

@(define (compute-md5sum filename [default ""])
   (cond [(file-exists? filename)
          (bytes->string/utf-8 (call-with-input-file filename md5 #:mode 'binary))]
         [else
          default]))



@title{ragg: a Racket AST Generator Generator}
@author+email["Danny Yoo" "dyoo@hashcollision.org"]


@section{Informal quickstart}

@(define my-eval (make-base-eval))
@(my-eval '(require ragg/examples/nested-word-list 
                          racket/list
                          racket/match))

Salutations!  Let's consider the following scenario: say that we're given the
following string:
@racketblock["(radiant (humble))"]


@margin-note{(... and pretend that we don't already know about the built-in
@racket[read] function.)}  How do we go about turning this kind of string into a
structured value?  That is, how would we @emph{parse} it?

We need to first consider the shape of the things we'd like to parse.  The
string above looks like a deeply nested list of words.  How might we describe
this formally?  A convenient notation to describe the shape of these things is
@link["http://en.wikipedia.org/wiki/Backus%E2%80%93Naur_Form"]{Backus-Naur
Form} (BNF).  So let's try to notate the structure of nested word lists in BNF.


@nested[#:style 'code-inset]{
@verbatim{
nested-word-list: WORD
                | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

What we intend by this notation is this: @racket[nested-word-list] is either an
atomic @racket[WORD], or a parenthesized list of any number of
@racket[nested-word-list]s.  We use the character @litchar{*} to represent zero
or more repetitions of the previous thing, and we treat the uppercased
@racket[LEFT-PAREN], @racket[RIGHT-PAREN], and @racket[WORD] as placeholders
for atomic @emph{tokens}.

@margin-note{See @secref{install-ragg} for instructions on installing
@tt{ragg.}}
Here are a few examples of tokens:
@interaction[#:eval my-eval
(require ragg/support)
(token 'LEFT-PAREN)
(token 'WORD "crunchy" #:span 7)
(token 'RIGHT-PAREN)]


Have we made progress?  At this point, we only have a BNF description in hand,
but we're still missing a @emph{parser}, something to take that description and
use it to make structures out of a sequence of tokens.


It's clear that we don't yet have a program because there's no @litchar{#lang}
line.  We should add one.  Put @litchar{#lang ragg} at the top of the BNF
description, and save it as a file called @filepath{nested-word-list.rkt}.

@filebox["nested-word-list.rkt"]{
@verbatim{
#lang ragg
nested-word-list: WORD
                | LEFT-PAREN nested-word-list* RIGHT-PAREN
}}

Now it is a proper program.  But what does it do?

@interaction[#:eval my-eval
@eval:alts[(require "nested-word-list.rkt") (void)]
parse
]

It gives us a @racket[parse] function.  Let's investigate what @racket[parse]
does for us.  What happens if we pass it a sequence of tokens?

@interaction[#:eval my-eval
             (define a-parsed-value
               (parse (list (token 'LEFT-PAREN "(")
                            (token 'WORD "some")
                            (token 'LEFT-PAREN "[") 
                            (token 'WORD "pig")
                            (token 'RIGHT-PAREN "]") 
                            (token 'RIGHT-PAREN ")"))))
             a-parsed-value]

Wait... that looks suspiciously like a syntax object!
@interaction[#:eval my-eval
(syntax->datum a-parsed-value)
]


That's @racket[(some [pig])], essentially.

What happens if we pass it a more substantial source of tokens?
@interaction[#:eval my-eval
@code:comment{tokenize: string -> (sequenceof token-struct?)}
@code:comment{Generate tokens from a string:}
(define (tokenize s)
  (for/list ([str (regexp-match* #px"\\(|\\)|\\w+" s)])
    (match str
      ["("
       (token 'LEFT-PAREN str)]
      [")"
       (token 'RIGHT-PAREN str)]
      [else
       (token 'WORD str)])))

@code:comment{For example:}
(define token-source (tokenize "(welcome (to (((ragg)) ())))"))
(define v (parse token-source))
(syntax->datum v)
]

Welcome to @tt{ragg}.







@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{Introduction}

@tt{ragg} is a parsing framework for Racket with the design goal to be easy
to use.  It includes the following features:
@itemize[

@item{It provides a @litchar{#lang} for writing extended BNF grammars.
A module written in @litchar{#lang ragg} automatically generates a
parser.  The output of this parser tries to follow
@link["http://en.wikipedia.org/wiki/How_to_Design_Programs"]{HTDP}
doctrine; the structure of the grammar informs the structure of the
Racket syntax objects it generates.}

@item{The language uses a few conventions to simplify the expression of
grammars.  The first rule in the grammar is automatically assumed to be the
starting production.  Identifiers in uppercase are assumed to represent
terminal tokens, and are otherwise the names of nonterminals.}

@item{Tokenizers can be developed completely independently of parsers.
@tt{ragg} takes a liberal view on tokens: they can be strings,
symbols, or instances constructed with @racket[token].  Furthermore,
tokens can optionally provide location: if tokens provide location, the
generated syntax objects will as well.}

@item{The underlying parser should be able to handle ambiguous grammars.}

@item{It should integrate with the rest of the Racket
@link["http://docs.racket-lang.org/guide/languages.html"]{language toolchain}.}

]

@subsection[#:tag "install-ragg"]{Installation}

@itemize[

@item{@margin-note{At the time of this writing, Racket 5.3.2 is in
@link["http://pre.racket-lang.org/"]{pre-release}.} If you are using a version
of Racket > 5.3.1, then follow the instructions on the
@link["https://plt-etc.byu.edu:9004/info/ragg"]{PLaneT2 page}.}



@item{For those who are using Racket <= 5.3.1, you can download the following PLT package:

@nested[#:style 'inset]{@link["ragg.plt"]{ragg.plt} [md5sum: @compute-md5sum["ragg.plt" "ab79038b40e510a5cf13363825c4aef4"]]
     
        Last updated: @lookup-date["ragg.plt" "Wednesday, January 16th, 2013"]
        }

Once downloaded, either use DrRacket's package installation features
(@link["http://docs.racket-lang.org/drracket/Menus.html#(idx._(gentag._57._(lib._scribblings/drracket/drracket..scrbl)))"]{Install
PLT File...} under DrRacket's File menu), or use the command line:
@nested[#:style 'inset]{@tt{raco setup -A ragg.plt}}}

]



@subsection{Example: a small DSL for ASCII diagrams}

@margin-note{This is a
@link["http://stackoverflow.com/questions/12345647/rewrite-this-script-by-designing-an-interpreter-in-racket"]{restatement
of a question on Stack Overflow}.}  To motivate @tt{ragg}'s design, let's look
at the following toy problem: we'd like to define a language for
drawing simple ASCII diagrams.  We'd like to be able write something like this:

@nested[#:style 'inset]{
@verbatim|{
3 9 X;
6 3 b 3 X 3 b;
3 9 X;
}|}

whose interpretation should generate the following picture:

@nested[#:style 'inset]{
@verbatim|{
XXXXXXXXX
XXXXXXXXX
XXXXXXXXX
   XXX   
   XXX   
   XXX   
   XXX   
   XXX   
   XXX   
XXXXXXXXX
XXXXXXXXX
XXXXXXXXX
}|}



@subsection{Syntax and semantics}
We're being very fast-and-loose with what we mean by the program above, so
let's try to nail down some meanings.  Each line of the program has a semicolon
at the end, and describes the output of several @emph{rows} of the line
drawing.  Let's look at two of the lines in the example:

@itemize[
@item{@litchar{3 9 X;}: ``Repeat the following 3 times: print @racket["X"] nine times, followed by
a newline.''}

@item{@litchar{6 3 b 3 X 3 b;}: ``Repeat the following 6 times: print @racket[" "] three times, 
followed by @racket["X"] three times, followed by @racket[" "] three times, followed by a newline.''}
]

Then each line consists of a @emph{repeat} number, followed by pairs of
(number, character) @emph{chunks}.  We will
assume here that the intent of the lowercased character @litchar{b} is to
represent the printing of a 1-character whitespace @racket[" "], and for other
uppercase letters to represent the printing of themselves.

Once we have a better idea of the pieces of each line, we have a better chance
to capture that meaning in a formal notation.  Once we have each instruction in
a structured format, we should be able to interpret it with a straighforward
case analysis.

Here is a first pass at expressing the structure of these line-drawing
programs.


@subsection{Parsing the concrete syntax}
@filebox["simple-line-drawing.rkt"]{
@verbatim|{
#lang ragg
drawing: rows*
rows: repeat chunk+ ";"
repeat: INTEGER
chunk: INTEGER STRING
}|
}

@margin-note{@secref{ragg-syntax} describes @tt{ragg}'s syntax in more detail.}
We write a @tt{ragg} program as an extended BNF grammar, where patterns can be:
@itemize[
@item{the names of other rules (e.g. @racket[chunk])}
@item{literal and symbolic token names (e.g. @racket[";"], @racket[INTEGER])}
@item{quantified patterns (e.g. @litchar{+} to represent one-or-more repetitions)}
]
The result of a @tt{ragg} program is a module with a @racket[parse] function
that can parse tokens and produce a syntax object as a result.

Let's exercise this function:
@interaction[#:eval my-eval
(require ragg/support)
@eval:alts[(require "simple-line-drawing.rkt") 
           (require ragg/examples/simple-line-drawing)]
(define stx
  (parse (list (token 'INTEGER 6) 
               (token 'INTEGER 2)
               (token 'STRING " ")
               (token 'INTEGER 3)
               (token 'STRING "X")
               ";")))
(syntax->datum stx)
]

Tokens can either be: plain strings, symbols, or instances produced by the
@racket[token] function.  (Plus a few more special cases, one in which we'll describe in a
moment.)

Preferably, we want to attach each token with auxiliary source location
information.  The more source location we can provide, the better, as the
syntax objects produced by @racket[parse] will incorporate them.

Let's write a helper function, a @emph{lexer}, to help us construct tokens more
easily.  The Racket standard library comes with a module called
@racketmodname[parser-tools/lex] which can help us write a position-sensitive
tokenizer:

@interaction[#:eval my-eval
(require parser-tools/lex)
(define (tokenize ip)
  (port-count-lines! ip)
  (define my-lexer
    (lexer-src-pos 
      [(repetition 1 +inf.0 numeric)
       (token 'INTEGER (string->number lexeme))]
      [upper-case
       (token 'STRING lexeme)]
      ["b"
       (token 'STRING " ")]
      [";"
       (token ";" lexeme)]
      [whitespace
       (token 'WHITESPACE lexeme #:skip? #t)]
      [(eof)
       (void)]))
  (define (next-token) (my-lexer ip))
  next-token)

(define a-sample-input-port (open-input-string "6 2 b 3 X;"))
(define token-thunk (tokenize a-sample-input-port))
@code:comment{Now we can pass token-thunk to the parser:}
(define another-stx (parse token-thunk))
(syntax->datum another-stx)
@code:comment{The syntax object has location information:}
(syntax-line another-stx)
(syntax-column another-stx)
(syntax-span another-stx)
]


There are a few things to note from this lexer example: 
@itemize[

@item{The @racket[parse] function can consume either sequences of tokens, or a
function that produces tokens.  Both of these are considered sources of
tokens.}

@item{As a special case for acceptable tokens, a token can also be an instance
of the @racket[position-token] structure of @racketmodname[parser-tools/lex],
in which case the token will try to derive its position from that of the
position-token.}

@item{The @racket[parse] function will stop reading from a token source if any
token is @racket[void].}

@item{The @racket[parse] function will skip over any token with the
@racket[#:skip?]  attribute. Elements such as whitespace and comments will
often have @racket[#:skip?] set to @racket[#t].}

]


@subsection{From parsing to interpretation}

We now have a parser for programs written in this simple-line-drawing language.
Our parser will give us back syntax objects:
@interaction[#:eval my-eval
(define parsed-program
  (parse (tokenize (open-input-string "3 9 X; 6 3 b 3 X 3 b; 3 9 X;"))))
(syntax->datum parsed-program)
]

Moreover, we know that these syntax objects have a regular, predictable
structure.  Their structure follows the grammar, so we know we'll be looking at
values of the form:

@racketblock[
    (drawing (rows (repeat <number>)
                   (chunk <number> <string>) ... ";")
             ...)
]

where @racket[drawing], @racket[rows], @racket[repeat], and @racket[chunk]
should be treated literally, and everything else will be numbers or strings.


Still, these syntax object values are just inert structures.  How do we
interpret them, and make them @emph{print}?  We did claim at the beginning of
this section that these syntax objects should be fairly easy to case-analyze
and interpret, so let's do it.

@margin-note{This is a very quick-and-dirty treatment of @racket[syntax-parse].
See the @racketmodname[syntax/parse] documentation for a gentler guide to its
features.}  Racket provides a special form called @racket[syntax-parse] in the
@racketmodname[syntax/parse] library.  @racket[syntax-parse] lets us do a
structural case-analysis on syntax objects: we provide it a set of patterns to
parse and actions to perform when those patterns match.


As a simple example, we can write a function that looks at a syntax object and
says @racket[#t] if it's the literal @racket[yes], and @racket[#f] otherwise:

@interaction[#:eval my-eval
(require syntax/parse)
@code:comment{yes-syntax-object?: syntax-object -> boolean}
@code:comment{Returns true if the syntax-object is yes.}
(define (yes-syntax-object? stx)
  (syntax-parse stx
    [(~literal yes)
     #t]
    [else
     #f]))
(yes-syntax-object? #'yes)
(yes-syntax-object? #'nooooooooooo)
]

Here, we use @racket[~literal] to let @racket[syntax-parse] know that
@racket[yes] should show up literally in the syntax object.  The patterns can
also have some structure to them, such as:
@racketblock[({~literal drawing} rows-stxs ...)]
which matches on syntax objects that begin, literally, with @racket[drawing],
followed by any number of rows (which are syntax objects too).


Now that we know a little bit more about @racket[syntax-parse], 
we can use it to do a case analysis on the syntax
objects that our @racket[parse] function gives us.
We start by defining a function on syntax objects of the form @racket[(drawing
rows-stx ...)].
@interaction[#:eval my-eval
(define (interpret-drawing drawing-stx)
  (syntax-parse drawing-stx
    [({~literal drawing} rows-stxs ...)

     (for ([rows-stx (syntax->list #'(rows-stxs ...))])
       (interpret-rows rows-stx))]))]

When we encounter a syntax object with @racket[(drawing rows-stx
...)], then @racket[interpret-rows] each @racket[rows-stx].

@;The pattern we
@;express in @racket[syntax-parse] above marks what things should be treated
@;literally, and the @racket[...] is a a part of the pattern matching language
@;known by @racket[syntax-parse] that lets us match multiple instances of the
@;last pattern.


Let's define @racket[interpret-rows] now:
@interaction[#:eval my-eval
(define (interpret-rows rows-stx)
  (syntax-parse rows-stx
    [({~literal rows}
      ({~literal repeat} repeat-number)
      chunks ... ";")

     (for ([i (syntax-e #'repeat-number)])
       (for ([chunk-stx (syntax->list #'(chunks ...))])
         (interpret-chunk chunk-stx))
       (newline))]))]

For a @racket[rows], we extract out the @racket[repeat-number] out of the
syntax object and use it as the range of the @racket[for] loop.  The inner loop
walks across each @racket[chunk-stx] and calls @racket[interpret-chunk] on it.


Finally, we need to write a definition for @racket[interpret-chunk].  We want
it to extract out the @racket[chunk-size] and @racket[chunk-string] portions,
and print to standard output:

@interaction[#:eval my-eval
(define (interpret-chunk chunk-stx)
  (syntax-parse chunk-stx
    [({~literal chunk} chunk-size chunk-string)

     (for ([k (syntax-e #'chunk-size)])
       (display (syntax-e #'chunk-string)))]))
]


@margin-note{Here are the definitions in a single file:
@link["examples/simple-line-drawing/interpret.rkt"]{interpret.rkt}.}
With these definitions in hand, now we can pass it syntax objects 
that we construct directly by hand:

@interaction[#:eval my-eval
(interpret-chunk #'(chunk 3 "X"))
(interpret-drawing #'(drawing (rows (repeat 5) (chunk 3 "X") ";")))
]

or we can pass it the result generated by our parser:
@interaction[#:eval my-eval
(define parsed-program
  (parse (tokenize (open-input-string "3 9 X; 6 3 b 3 X 3 b; 3 9 X;"))))
(interpret-drawing parsed-program)]

And now we've got an interpreter!


@subsection{From interpretation to compilation}

@margin-note{For a gentler tutorial on writing @litchar{#lang} extensions, see:
@link["http://hashcollision.org/brainfudge"]{F*dging up a Racket}.}  (Just as a
warning: the following material is slightly more advanced, but shows how
writing a compiler for the line-drawing language reuses the ideas for the
interpreter.)

Wouldn't it be nice to be able to write something like:

@nested[#:style 'inset]{
@verbatim|{
3 9 X;
6 3 b 3 X 3 b;
3 9 X;
}|}

and have Racket automatically compile this down to something like this?
@racketblock[
(for ([i 3])
  (for ([k 9]) (displayln "X"))
  (newline))

(for ([i 6])
  (for ([k 3]) (displayln " "))
  (for ([k 3]) (displayln "X"))
  (for ([k 3]) (displayln " "))
  (newline))

(for ([i 3])
  (for ([k 9]) (displayln "X"))
  (newline))
]

Well, of course it won't work: we don't have a @litchar{#lang} line.

Let's add one.

@filebox["letter-i.rkt"]{
@verbatim|{
#lang ragg/examples/simple-line-drawing
3 9 X;
6 3 b 3 X 3 b;
3 9 X;
}|
}

Now @filepath{letter-i.rkt} is a program.


How does this work?  From the previous sections, we've seen how to take the
contents of a file and interpret it.  What we want to do now is teach Racket
how to compile programs labeled with this @litchar{#lang} line.  We'll do two
things:

@itemize[
@item{Tell Racket to use the @tt{ragg}-generated parser and lexer we defined
earlier whenever it sees a program written with
@litchar{#lang ragg/examples/simple-line-drawing}.}

@item{Define transformation rules for @racket[drawing], @racket[rows], and
      @racket[chunk] to rewrite these into standard Racket forms.}
]

The second part, the writing of the transformation rules, will look very
similar to the definitions we wrote for the interpreter, but the transformation
will happen at compile-time.  (We @emph{could} just resort to simply calling
into the interpreter we just wrote up, but this section is meant to show that
compilation is also viable.)


We do the first part by defining a @emph{module reader}: a
@link["http://docs.racket-lang.org/guide/syntax_module-reader.html"]{module
reader} tells Racket how to parse and compile a file.  Whenever Racket sees a
@litchar{#lang <name>}, it looks for a corresponding module reader in
@filepath{<name>/lang/reader}.

Here's the definition for
@filepath{ragg/examples/simple-line-drawing/lang/reader.rkt}:

@filebox["ragg/examples/simple-line-drawing/lang/reader.rkt"]{
@codeblock|{
#lang s-exp syntax/module-reader
ragg/examples/simple-line-drawing/semantics
#:read my-read
#:read-syntax my-read-syntax
#:whole-body-readers? #t

(require ragg/examples/simple-line-drawing/lexer
         ragg/examples/simple-line-drawing/grammar)

(define (my-read in)
  (syntax->datum (my-read-syntax #f in)))

(define (my-read-syntax src ip)
  (list (parse src (tokenize ip))))
}|
}

We use a helper module @racketmodname[syntax/module-reader], which provides
utilities for creating a module reader.  It uses the lexer and
@tt{ragg}-generated parser we defined earlier (saved into
@link["http://hashcollision.org/ragg/examples/simple-line-drawing/lexer.rkt"]{lexer.rkt}
and
@link["http://hashcollision.org/ragg/examples/simple-line-drawing/grammar.rkt"]{grammar.rkt}
modules), and also tells Racket that it should compile the forms in the syntax
object using a module called @filepath{semantics.rkt}.

@margin-note{For a systematic treatment on capturing the semantics of
a language, see @link["http://cs.brown.edu/~sk/Publications/Books/ProgLangs/"]{Programming Languages: Application and
Interpretation}.}

Let's look into @filepath{semantics.rkt} and see what's involved in
compilation:
@filebox["ragg/examples/simple-line-drawing/semantics.rkt"]{
@codeblock|{
#lang racket/base
(require (for-syntax racket/base syntax/parse))

(provide #%module-begin
         ;; We reuse Racket's treatment of raw datums, specifically
         ;; for strings and numbers:
         #%datum
         
         ;; And otherwise, we provide definitions of these three forms.
         ;; During compiliation, Racket uses these definitions to 
         ;; rewrite into for loops, displays, and newlines.
         drawing rows chunk)

;; Define a few compile-time functions to do the syntax rewriting:
(begin-for-syntax
  (define (compile-drawing drawing-stx)
    (syntax-parse drawing-stx
      [({~literal drawing} rows-stxs ...)

     (syntax/loc drawing-stx
       (begin rows-stxs ...))]))

  (define (compile-rows rows-stx)
    (syntax-parse rows-stx
      [({~literal rows}
        ({~literal repeat} repeat-number)
        chunks ... 
        ";")

       (syntax/loc rows-stx
         (for ([i repeat-number])
           chunks ...
           (newline)))]))

  (define (compile-chunk chunk-stx)
    (syntax-parse chunk-stx
      [({~literal chunk} chunk-size chunk-string)

       (syntax/loc chunk-stx
         (for ([k chunk-size])
           (display chunk-string)))])))


;; Wire up the use of "drawing", "rows", and "chunk" to these
;; transformers:
(define-syntax drawing compile-drawing)
(define-syntax rows compile-rows)
(define-syntax chunk compile-chunk)
}|
}

The semantics hold definitions for @racket[compile-drawing],
@racket[compile-rows], and @racket[compile-chunk], similar to what we had for
interpretation with @racket[interpret-drawing], @racket[interpret-rows], and
@racket[interpret-chunk].  However, compilation is not the same as
interpretation: each definition does not immediately execute the act of
drawing, but rather returns a syntax object whose evaluation will do the actual
work.

There are a few things to note:

@itemize[

@item{@tt{ragg}'s native data structure is the syntax object because the
majority of Racket's language-processing infrastructure knows how to read and
write this structured value.}


@item{
@margin-note{By the way, we can just as easily rewrite the semantics so that
@racket[compile-rows] does explicitly call @racket[compile-chunk].  Often,
though, it's easier to write the transformation functions in this piecemeal way
and depend on the Racket macro expansion system to do the rewriting as it
encounters each of the forms.}
Unlike in interpretation, @racket[compile-rows] doesn't
compile each chunk by directly calling @racket[compile-chunk].  Rather, it
depends on the Racket macro expander to call each @racket[compile-XXX] function
as it encounters a @racket[drawing], @racket[rows], or @racket[chunk] in the
parsed value.  The three statements at the bottom of @filepath{semantics.rkt} inform
the macro expansion system to do this:

@racketblock[
(define-syntax drawing compile-drawing)
(define-syntax rows compile-rows)
(define-syntax chunk compile-chunk)
]}
]


Altogether, @tt{ragg}'s intent is to be a parser generator generator for Racket
that's easy and fun to use.  It's meant to fit naturally with the other tools
in the Racket language toolchain.  Hopefully, it will reduce the friction in
making new languages with alternative concrete syntaxes.

The rest of this document describes the @tt{ragg} language and the parsers it
generates.


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

@section{The language}

@subsection[#:tag "ragg-syntax"]{Syntax and terminology}
A program in the @tt{ragg} language consists of the language line
@litchar{#lang ragg}, followed by a collection of @tech{rule}s and
@tech{line comment}s.

A @deftech{rule} is a sequence consisting of: a @tech{rule identifier}, a colon
@litchar{":"}, and a @tech{pattern}.

A @deftech{rule identifier} is an @tech{identifier} that is not in upper case.

A @deftech{token identifier} is an @tech{identifier} that is in upper case.

An @deftech{identifier} is a character sequence of letters, numbers, and
characters in @racket["-.!$%&/<=>?^_~@"].  It must not contain
@litchar{*} or @litchar{+}, as those characters are used to denote
quantification.


A @deftech{pattern} is one of the following:
@itemize[
@item{an implicit sequence of @tech{pattern}s separated by whitespace}
@item{a terminal: either a literal string or a @tech{token identifier}}
@item{a @tech{rule identifier}}
@item{a @deftech{choice pattern}: a sequence of @tech{pattern}s delimited with @litchar{|} characters.}
@item{a @deftech{quantifed pattern}: a @tech{pattern} followed by either @litchar{*} (``zero or more'') or @litchar{+} (``one or more'')}
@item{an @deftech{optional pattern}: a @tech{pattern} surrounded by @litchar{[} and @litchar{]}}
@item{an explicit sequence: a @tech{pattern} surrounded by @litchar{(} and @litchar{)}}]

A @deftech{line comment} begins with either @litchar{#} or @litchar{;} and
continues till the end of the line.


For example, in the following program:
@nested[#:style 'inset
@verbatim|{
#lang ragg
;; A parser for a silly language
sentence: verb optional-adjective object
verb: greeting
optional-adjective: ["happy" | "frumpy"]
greeting: "hello" | "hola" | "aloha"
object: "world" | WORLD
}|]

the elements @tt{sentence}, @tt{verb}, @tt{greeting}, and @tt{object} are rule
identifiers.  The first rule, @litchar{sentence: verb optional-adjective
object}, is a rule whose right side is an implicit pattern sequence of three
sub-patterns.  The uppercased @tt{WORLD} is a token identifier.  The fourth rule in the program associates @tt{greeting} with a @tech{choice pattern}.



More examples:
@itemize[

@item{A
@link["http://hashcollision.org/ragg/examples/01-equal.rkt"]{BNF} for binary
strings that contain an equal number of zeros and ones.
@verbatim|{
#lang ragg
equal: [zero one | one zero]   ;; equal number of "0"s and "1"s.
zero: "0" equal | equal "0"    ;; has an extra "0" in it.
one: "1" equal | equal "1"     ;; has an extra "1" in it.
}|
}

@item{A @link["http://hashcollision.org/ragg/examples/baby-json.rkt"]{BNF} for
@link["http://www.json.org/"]{JSON}-like structures.
@verbatim|{
#lang ragg
json: number | string
    | array  | object
number: NUMBER
string: STRING
array: "[" [json ("," json)*] "]"
object: "{" [kvpair ("," kvpair)*] "}"
kvpair: ID ":" json
}|
}
]

The @link["https://github.com/dyoo/ragg"]{ragg github source repository}
includes
@link["https://github.com/dyoo/ragg/tree/master/ragg/examples"]{several more
examples}.



@subsection{Syntax errors}

Besides the basic syntax errors that can occur with a malformed grammar, there
are a few other classes of situations that @litchar{#lang ragg} will consider
as syntax errors.

@tt{ragg} will raise a syntax error if the grammar:
@itemize[
@item{doesn't have any rules.}

@item{has a rule with the same left hand side as any other rule.}

@item{refers to rules that have not been defined.  e.g. the
following program:
@nested[#:style 'code-inset
@verbatim|{
#lang ragg
foo: [bar]
}|
]
should raise an error because @tt{bar} has not been defined, even though
@tt{foo} refers to it in an @tech{optional pattern}.}


@item{uses the token name @racket[EOF]; the end-of-file token type is reserved
for internal use by @tt{ragg}.}


@item{contains a rule that has no finite derivation.  e.g. the following
program:
@nested[#:style 'code-inset
@verbatim|{
#lang ragg
infinite-a: "a" infinite-a
}|
]
should raise an error because no finite sequence of tokens will satisfy
@tt{infinite-a}.}

]

Otherwise, @tt{ragg} should be fairly tolerant and permit even ambiguous
grammars.

@subsection{Semantics}
@declare-exporting[ragg/examples/nested-word-list]

A program written in @litchar{#lang ragg} produces a module that provides a few
bindings.  The most important of these is @racket[parse]:

@defproc[(parse [source any/c #f] 
                [token-source (or/c (sequenceof token)
                                    (-> token))])
         syntax?]{

Parses the sequence of @tech{tokens} according to the rules in the grammar, using the
first rule as the start production.  The parse must completely consume
@racket[token-source].

The @deftech{token source} can either be a sequence, or a 0-arity function that
produces @tech{tokens}.

A @deftech{token} in @tt{ragg} can be any of the following values:
@itemize[
@item{a string}
@item{a symbol}
@item{an instance produced by @racket[token]}
@item{an instance produced by the token constructors of @racketmodname[parser-tools/lex]}
@item{an instance of @racketmodname[parser-tools/lex]'s @racket[position-token] whose 
      @racket[position-token-token] is a @tech{token}.}
]

A token whose type is either @racket[void] or @racket['EOF] terminates the
source.


If @racket[parse] succeeds, it will return a structured syntax object.  The
structure of the syntax object follows the overall structure of the rules in
the BNF.  For each rule @racket[r] and its associated pattern @racket[p],
@racket[parse] generates a syntax object @racket[#'(r p-value)] where
@racket[p-value]'s structure follows a case analysis on @racket[p]:

@itemize[
@item{For implicit and explicit sequences of @tech{pattern}s @racket[p1],
      @racket[p2], ..., the corresponding values, spliced into the
      structure.}
@item{For terminals, the value associated to the token.}
@item{For @tech{rule identifier}s: the associated parse value for the rule.}
@item{For @tech{choice pattern}s: the associated parse value for one of the matching subpatterns.}
@item{For @tech{quantifed pattern}s and @tech{optional pattern}s: the corresponding values, spliced into the structure.}
]

Consequently, it's only the presence of @tech{rule identifier}s in a rule's
pattern that informs the parser to introduces nested structure into the syntax
object.


If the grammar has ambiguity, @tt{ragg} will choose and return a parse, though
it does not guarantee which one it chooses.


If the parse cannot be performed successfully, or if a token in the
@racket[token-source] uses a type that isn't mentioned in the grammar, then
@racket[parse] raises an instance of @racket[exn:fail:parsing].}



It's often convenient to extract a parser for other non-terminal rules in the
grammar, and not just for the first rule.  A @tt{ragg}-generated module also
provides a form called @racket[make-rule-parser] to extract a parser for the
other non-terminals:

@defform[#:id make-rule-parser
         (make-rule-parser name)]{
Constructs a parser for the @racket[name] of one of the non-terminals
in the grammar.

For example, given the @tt{ragg} program
@filepath{simple-arithmetic-grammar.rkt}:
@filebox["simple-arithmetic-grammar.rkt"]{
@verbatim|{
#lang ragg
expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT
}|
}
the following interaction shows how to extract a parser for @racket[term]s.
@interaction[#:eval my-eval
@eval:alts[(require "simple-arithmetic-grammar.rkt") 
                    (require ragg/examples/simple-arithmetic-grammar)]
(define term-parse (make-rule-parser term))
(define tokens (list (token 'INT 3) 
                     "*" 
                     (token 'INT 4)))
(syntax->datum (parse tokens))
(syntax->datum (term-parse tokens))

(define another-token-sequence
  (list (token 'INT 1) "+" (token 'INT 2)
        "*" (token 'INT 3)))
(syntax->datum (parse another-token-sequence))
@code:comment{Note that term-parse will break on another-token-sequence}
@code:comment{as it does not know what to do with the "+"}
(term-parse another-token-sequence)
]

}


Finally, the module provides a set of all the used token types in the grammar
in @racket[all-token-types]:
@defthing[all-token-types (setof symbol?)]{
A set of all the token types used in a grammar.

For example:
@interaction[#:eval my-eval
@eval:alts[(require "simple-arithmetic-grammar.rkt") 
                    (require ragg/examples/simple-arithmetic-grammar)]
all-token-types
]

}





@section{Support API}

@defmodule[ragg/support]

The @racketmodname[ragg/support] module provides functions to interact with
@tt{ragg} programs.  The most useful is the @racket[token] function, which
produces tokens to be parsed.

@defproc[(token [type (or/c string? symbol?)]
                [val any/c #f]
                [#:line line (or/c positive-integer? #f) #f]
                [#:column column (or/c natural-number? #f) #f]
                [#:offset offset (or/c positive-integer? #f) #f]
                [#:span span (or/c natural-number? #f) #f]
                [#:skip? skip? boolean? #f]
                )
         token-struct?]{
Creates instances of @racket[token-struct]s.

The syntax objects produced by a parse will inject the value @racket[val] in
place of the token name in the grammar.

If @racket[#:skip?] is true, then the parser will skip over it during a
parse.}


@defstruct[token-struct ([type symbol?]
                         [val any/c]
                         [offset (or/c positive-integer? #f)]
                         [line (or/c natural-number? #f)]
                         [column (or/c positive-integer? #f)]
                         [span (or/c natural-number? #f)]
                         [skip? boolean?])
                        #:transparent]{
The token structure type.

Rather than directly using the @racket[token-struct] constructor, please use
the helper function @racket[token] to construct instances.
}




@defstruct[(exn:fail:parsing exn:fail) 
           ([message string?]
            [continuation-marks continuation-mark-set?]
            [srclocs (listof srcloc?)])]{
The exception raised when parsing fails.

@racket[exn:fail:parsing] implements Racket's @racket[prop:exn:srcloc]
property, so if this exception reaches DrRacket's default error handler,
DrRacket should highlight the offending locations in the source.}






@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Caveats and things to do}

Here are a few caveats and future aims for @tt{ragg}.

@itemize[

@item{@tt{ragg} doesn't currently have a good story about operator precedence.
Future versions of @tt{ragg} will support the specification of operator
precedence to deal with grammar ambiguity, probably by extending the BNF
grammar rules in @litchar{#lang ragg} with keyword arguments.}


@item{I currently depend on the lexer framework provided by
@racketmodname[parser-tools/lex], which has a steeper learning curve than I'd
like.  A future version of @tt{ragg} will probably try to provide a nicer set
of tools for defining lexers.}


@item{The underlying parsing engine (an Earley-style parser) has not been fully
optimized, so it may exhibit degenerate parse times.  A future version of
@tt{ragg} will guarantee @math{O(n^3)} time bounds so that at the very least,
parses will be polynomial-time.}


@item{@tt{ragg} doesn't yet have a good story on dealing with parser error
recovery.  If a parse fails, it tries to provide the source location, but does
little else.}

@item{@tt{ragg} is slightly misnamed: what it really builds is a concrete
syntax tree rather than an abstract syntax tree.  A future version of @tt{ragg}
will probably support annotations on patterns so that they can be omitted or
transformed in the parser output.}

]


@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


@section{Miscellaneous and thanks}

Thanks to Matthew Flatt for pointing me to @racket[cfg-parser] from the
@racket[cfg-parser] library.  Joe Politz gave me good advice and
feedback.  Also, he suggested the name ``ragg''.  Other alternatives I'd been
considering were ``autogrammar'' or ``chompy''.  Thankfully, he is a better
Namer than me.  Daniel Patterson provided feedback that led to
@racket[make-rule-parser].  Robby Findler and Guillaume Marceau provided
steadfast suggestions to look into other parsing frameworks like
@link["http://en.wikipedia.org/wiki/Syntax_Definition_Formalism"]{SDF} and
@link["http://sablecc.org/"]{SableCC}.  Special thanks to Shriram
Krishnamurthi, who convinced me that other people might find this package
useful.


@close-eval[my-eval]
