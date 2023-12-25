repeat3(Lst, X) :-
    append(X1, X23, Lst),
    append(_, X, X1),
    append(X2, X3End, X23),
    append(_, X, X2),
    append(X3, _, X3End),
    append(_, X, X3),
    X \= [].
