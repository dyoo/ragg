#lang autogrammar
nested-word-list: WORD
                | LEFT-PAREN nested-word-list* RIGHT-PAREN
