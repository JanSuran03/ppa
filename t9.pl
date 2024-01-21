match(2, a).
match(2, b).
match(2, c).
match(3, d).
match(3, e).
match(3, f).
match(4, g).
match(4, h).
match(4, i).
match(5, j).
match(5, k).
match(5, l).
match(6, m).
match(6, n).
match(6, o).
match(7, p).
match(7, q).
match(7, r).
match(7, s).
match(8, t).
match(8, u).
match(8, v).
match(9, w).
match(9, x).
match(9, y).
match(9, z).

t9Impl(0, []) :- !.
t9Impl(N, [H | T]) :-
    NewN is N div 10,
    Digit is N mod 10,
    match(Digit, H),
    t9Impl(NewN, T).

t9(N, Arr) :- t9Impl(N, Arr2), reverse(Arr, Arr2).
