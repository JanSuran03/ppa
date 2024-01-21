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

[define [not-any? f lst]
  [not [some f lst]]]

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

{define {reduce f init lst}
  {if {empty? lst}
    init
    {reduce f {f init {car lst}} {cdr lst}}}}

{define {my-max x y} {if {> x y} x y}}

{define {add-time rem time}
  {if {empty? rem}
    `{{,time 1}}
    {let {{next {car rem}}}
      {if {equal? {car next} time}
        {cons `{,time ,{+ {cadr next} 1}} {cdr rem}}
        {cons next {add-time {cdr rem} time}}}}}}

{define {coffee-shop times}
  {let {{times {reduce add-time '{} times}}}
    {reduce {lambda {cur time}
              {my-max cur {cadr time}}}
            0
            times}}}

{define {dfs visited paths target}
  {let {{top {car visited}}}
    {or {some {lambda {edge}
                {and {= {car edge} top}
                     {= {cadr edge} target}}}
              paths}
        {some {lambda {edge}
                {and {= {car edge} top}
                     {not-any? {lambda {vis-vert}
                                 {= {cadr edge} vis-vert}}
                               visited}
                     {dfs {cons {cadr edge} visited} paths target}}}
              paths}}}}

{define {cities-path? paths from to}
  {or {= from to}
      {dfs {cons from null} paths to}}}

{define {nth lst index}
  {if {= index 0}
    {car lst}
    {nth {cdr lst} {- index 1}}}}

{define {take2 n lst}
  {if {= n 0}
    '{}
    {cons {car lst} {take2 {- n 1} {cdr lst}}}}}

{define {drop2 n lst}
  {if {= n 0}
    lst
    {drop2 {- n 1} {cdr lst}}}}

{define {concat lst1 lst2}
  {if {empty? lst1}
    lst2
    {cons {car lst1} {concat {cdr lst1} lst2}}}}

{define {swap-n n lst}
  {concat {drop2 n lst} {take2 n lst}}}

{define {lstlen lst}
  {if {empty? lst}
    0
    {+ 1 {lstlen {cdr lst}}}}}

{define {shift-row matrix n by}
  {let {{len {lstlen {nth matrix n}}}}
    {concat {concat {take2 n matrix}
                    `{,{swap-n {modulo {- len by} len} {nth matrix n}}}}
            {drop2 {+ n 1} matrix}}}}

{define {shift-col-impl matrest by n nths len}
  {if {empty? matrest}
      '{}
      {cons {concat {concat {take2 n {car matrest}} `{,{nth nths {modulo by len}}}}
                    {drop2 {+ n 1} {car matrest}}}
            {shift-col-impl {cdr matrest} {+ by 1} n nths len}}}}

{define {shift-col matrix n by}
  {let {{nths {map {lambda {row} {nth row n}} matrix}}
        {len {lstlen matrix}}}
    {shift-col-impl matrix {- len by} n nths len}}}

{define {rotate-mat matrix commands}
  {reduce {lambda {matrix command}
            {if {= {car command} 0}
              {shift-row matrix {cadr command} {caddr command}}
              {shift-col matrix {cadr command} {caddr command}}}}
          matrix
          commands}}