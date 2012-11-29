#lang planet dyoo/autogrammar
nested-word-list: LEFT-PAREN nested-word-list* RIGHT-PAREN
                | WORD
