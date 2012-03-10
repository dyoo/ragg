#lang planet dyoo/autogrammar/lalr

expr : term ('+' term)*
term : factor (('*') factor)*
factor : INT
