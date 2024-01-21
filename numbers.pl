subsets([], Res, Res).
subsets([H | T], Acc, Res) :-
    subsets(T, [H | Acc], Res);
    subsets(T, Acc, Res).
subsets(Lst, Res) :- subsets(Lst, [], Res), Res \= [].

removeFirst([X | T], X, T) :- !.
removeFirst([Y | T], X, [Y | Rst]) :- removeFirst(T, X, Rst).

numImpl([Res], Res) :- !.
numImpl(Lst, Res) :-
    member(X, Lst),
    removeFirst(Lst, X, Rem1),
    member(Y, Rem1),
    removeFirst(Rem1, Y, Rem2),
    (
        (Z is X + Y, numImpl([Z | Rem2], Res));
        (Z is X - Y, numImpl([Z | Rem2], Res));
        (Z is X * Y, numImpl([Z | Rem2], Res));
        (Y \= 0, Z is X / Y, 0 is mod(X, Y), numImpl([Z | Rem2], Res))
    ), !.

numbers(Lst, Res) :-
    subsets(Lst, Subset),
    numImpl(Subset, Res).

onlyFirst2(Goal) :- Goal, !.
