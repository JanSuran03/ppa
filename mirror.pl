mirror(empty, empty) :- !.
mirror(btree(V, L1, R1), btree(V, L2, R2)) :- mirror(L1, R2), mirror(L2, R1).
