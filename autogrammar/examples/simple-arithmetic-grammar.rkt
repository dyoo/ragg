#lang autogrammar

expr : term ('+' term)*
term : factor ('*' factor)*
factor : INT
