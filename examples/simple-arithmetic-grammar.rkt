#lang planet dyoo/autogrammar

expr : term ('+' term)*
term : factor (('*') factor)*
factor : INT