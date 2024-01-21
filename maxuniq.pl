% maxuniq(+Lst, -R)

%partitions([X], [X]).
%partitions[X|Xs], 
%maxuniq(Lst, R).

partitions([], [], Parts, Parts). % both empty

partitions([], [H | T], Parts, [[H | T] | Parts]). % empty lst, add acc

partitions([H | T], [], Parts, R) :- partitions(T, [H], Parts, R). % empty acc, add element to acc

partitions([H | T], [C | Curr], Parts, R) :- % non-empty lst, non-empty acc
    partitions(T, [H | [C | Curr]], Parts, R);
    partitions(T, [H], [[C | Curr] | Parts], R).

partitions(Lst, R) :- partitions(Lst, [], [], R).

anyduplicates([H | T], Vals) :- member(H, Vals); anyduplicates(T, [H | Vals]).
anyduplicates(Lst) :- anyduplicates(Lst, []).
uniquepartitions(Lst, Res) :- findall(X, (partitions(Lst, X), not(anyduplicates(X))), Res).

arraylenmax([], Curr, Curr).
arraylenmax([H | T], Curr, Res) :-
    length(H, L1),
    NewCurr is max(Curr, L1),
    arraylenmax(T, NewCurr, Res).

arraylenmax(Lst, Res) :- arraylenmax(Lst, 0, Res).

maxuniq(Lst, Res) :-
    uniquepartitions(Lst, Partitions),
    arraylenmax(Partitions, Res).
