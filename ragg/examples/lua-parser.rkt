#lang ragg

;; Lua parser, adapted from:
;; http://www.lua.org/manual/5.1/manual.html#8
;;


chunk : (stat ["; "])* [laststat ["; "]]

block : chunk

stat :  varlist "=" explist | 
          functioncall | 
          DO block END | 
          WHILE exp DO block END | 
          REPEAT block UNTIL exp | 
          IF exp THEN block (ELSEIF exp THEN block)* [ELSE block] END | 
          FOR NAME "=" exp "," exp ["," exp] DO block END | 
          FOR namelist IN explist DO block END | 
          FUNCTION funcname funcbody | 
          LOCAL FUNCTION NAME funcbody | 
          LOCAL namelist ["=" explist] 

laststat : RETURN [explist] | BREAK

funcname : NAME ("." NAME)* [":" NAME]

varlist : var ("," var)*

var :  NAME | prefixexp "[" exp "]" | prefixexp "." NAME 

namelist : NAME ("," NAME)*

explist : (exp ",")* exp
                
;; FIXME: handle exp operator precedences.
;; See: http://www.lua.org/manual/5.1/manual.html#2.5.6
;;
;;
;; Operator precedence in Lua follows the table below, from lower to higher priority:
;;
;;     or
;;     and
;;     <     >     <=    >=    ~=    ==
;;     ..
;;     +     -
;;     *     /     %
;;     not   #     - (unary)
;;     ^              
;;
;; As usual, you can use parentheses to change the precedences of an expression.
;; The concatenation ('..') and exponentiation ('^') operators are right associative.
;; All other binary operators are left associative.
              
exp :  NIL | FALSE | TRUE | NUMBER | STRING | "..." | function | 
         prefixexp | tableconstructor | exp binop exp | unop exp 

prefixexp : var | functioncall | "(" exp ")"

functioncall :  prefixexp args | prefixexp ":" NAME args 

args :  "(" [explist] ")" | tableconstructor | STRING 

function : FUNCTION funcbody

funcbody : "(" [parlist] ")" block END

parlist : namelist ["," "..."] | "..."

tableconstructor : "{" [fieldlist] "}"

fieldlist : field (fieldsep field)* [fieldsep]

field : "[" exp "]" "=" exp | NAME "=" exp | exp

fieldsep : "," | ";"

binop : "+" | "-" | "*" | "/" | "^" | "%" | ".." | 
          "<" | "<=" | ">" | ">=" | "==" | "~=" | 
          AND | OR

unop : "-" | NOT | "#"