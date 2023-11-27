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

[define [decorate f g]
  [lambda [x] [g [f x]]]]

[define [some f lst]
  [if [empty? lst]
    #F
    [let [[res [f [car lst]]]]
      [if res res [some f [cdr lst]]]]]]

[define [every? f lst]
  [if [empty? lst]
    #T
    [if [f [car lst]] [every? f [cdr lst]] #F]]]

[define [cnf-value form values]
  [every? [lambda [disj]
            [some [lambda [lit]
                    [some [lambda [pair]
                            [if [= [car pair] lit]
                              [cadr pair]
                              #F]]
                          values]]
                  disj]]
          form]]

[define [comb-impl lst n acc ret]
  [if [empty? lst]
    [if [= n acc]
      [+ ret 1]
      ret]
    [let [[ret [comb-impl [cdr lst] n [+ acc [car lst]] ret]]]
      [comb-impl [cdr lst] n acc ret]]]]
  

[define [comb lst n]
  [comb-impl lst n 0 0]]