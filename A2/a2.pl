% Types
type(intT).
type(boolT).
type(varT).


% base cases
hastype(_, intT(N), intT) :- N = 1 ; integer(N), !.
hastype(_, boolT(T), boolT) :- (T = true ; T = false), !.
hastype(_, varT(X), boolT) :- hastype(_, boolT(X), boolT).
hastype(_, varT(X), intT) :- hastype(_, intT(X), intT).
hastype(G, varT(X), T) :- atom(X), member((X,T), G).

% checking type of expressions with binary and unary operations

hastype(G, add(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, subtract(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, mul(E1, E2), intT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, and(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, or(E1, E2), boolT) :- hastype(G, E1, boolT), hastype(G, E2, boolT).
hastype(G, not(E1), boolT) :- hastype(G, E1, boolT).
hastype(G, eq(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, gt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).
hastype(G, lt(E1, E2), boolT) :- hastype(G, E1, intT), hastype(G, E2, intT).

% testcases and their results
/*
| ?- hastype([], add(true, 5), intT).

no
| ?- hastype(G, add(p, 5), intT).    

no
| ?- hastype(G, add(varT(p), intT(5)), intT).

G = [(p,intT)|_]

yes
| ?- hastype([], add(varT(p), intT(5)), intT).

no
| ?- hastype([], add(boolT(true), intT(5)), intT).

no
| ?- hastype(G, add(mul(varT(p), intT(7)), mul(varT(q), intT(4))), intT).

G = [(p,intT),(q,intT)|_]

yes
| ?- hastype([(x,intT), (y, boolT)] , or( gt( intT(1) , varT(x) ) , varT(y) ), Z).

Z = boolT ? 

yes
| ?- hastype(G, not(varT(x)), Z).

G = [(x,boolT)|_]
Z = boolT ? 

yes
| ?- hastype([], add(intT(5), Y), intT).

Y = intT(1) ? 

yes
| ?- hastype(G, and(boolT(true), Y), boolT).

Y = boolT(true) ? 

yes
| ?- hastype([(xp,intT)],not(boolT(4)),Type).

no
| ?- hastype([(xp,intT)],not(boolT(true)),Type).

Type = boolT ? 

(1 ms) yes
*/