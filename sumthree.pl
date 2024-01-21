sumOne([H | _], Target, Acc) :- Sum is H + Acc, Sum = Target, !.
sumOne([_ | T], Target, Acc) :- sumOne(T, Target, Acc).

sumTwo([H | T], Target, Acc) :- Sum is H + Acc, sumOne(T, Target, Sum), !.
sumTwo([_ | T], Target, Acc) :- sumTwo(T, Target, Acc).

sumThreeAcc([H | T], Target, Acc) :- Sum is H + Acc, sumTwo(T, Target, Sum), !.
sumThreeAcc([_ | T], Target, Acc) :- sumThreeAcc(T, Target, Acc).

sumThree(Lst, Target) :- sumThreeAcc(Lst, Target, 0).

