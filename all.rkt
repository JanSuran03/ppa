#lang racket

[define [reverser lst acc]
  [if [empty? lst]
    acc
    [reverser [cdr lst] [cons [car lst] acc]]]]

[define [reverse-list lst]
  [reverser lst '[]]]

[define [reverse-tree lst]
  [cond [[not [list? lst]]]
        [[empty? lst] '[]]
        [69420 [cons [car lst] [reverse-list [map reverse-tree [cdr lst]]]]]]]
