leftTree(_, empty) :- !.
leftTree(X, btree(V, L, R)) :- V < X, leftTree(X, L), leftTree(X, R), leftTree(V, L), rightTree(V, R).
rightTree(_, empty) :- !.
rightTree(X, btree(V, L, R)) :- V > X, rightTree(X, L), rightTree(X, R), leftTree(V, L), rightTree(V, R).
isBst(empty) :- !.
isBst(btree(V, L, R)) :- leftTree(V, L), rightTree(V, R).

