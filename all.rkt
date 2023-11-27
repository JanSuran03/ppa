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

{define {inc n} {+ n 1}}

[define [comb-impl lst n acc ret]
  [if [empty? lst]
    [if [= n acc]
      [+ ret 1]
      ret]
    [let [[ret [comb-impl [cdr lst] n [+ acc [car lst]] ret]]]
      [comb-impl [cdr lst] n acc ret]]]]
  

[define [comb lst n]
  [comb-impl lst n 0 0]]

{define {reduce f init lst}
  {if {empty? lst}
    init
    {reduce f {f init {car lst}} {cdr lst}}}}

{define {my-own-max-because-the-evaluator-is-so-shitt-that-i-cannot-even-use-the-built-in-max-lol x y} {if {> x y} x y}}

{define {add-time rem time}
  {if {empty? rem}
    `{{,time 1}}
    {let {{next {car rem}}}
      {if {equal? {car next} time}
        {cons `{,time ,{inc {cadr next}}} {cdr rem}}
        {cons next {add-time {cdr rem} time}}}}}}

{define {coffee-shop times}
  {let {{times {reduce add-time '{} times}}}
    {reduce {lambda {cur time}
              {my-own-max-because-the-evaluator-is-so-shitt-that-i-cannot-even-use-the-built-in-max-lol cur {cadr time}}}
            0
            times}}}
    