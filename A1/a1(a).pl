mem(X, [X|_]).
mem(X, [_|T]):-mem(X, T).

%part_a
/* Transitive Closure */
memI((X,Y), transclos(R)) :- mem((X, Y), transclos(R), []).
mem((X,Y), transclos(R), _) :- mem((X,Y), R).
mem((X,Z), transclos(R), Seen) :- mem((X,Y), R), \+mem((X,Y), Seen), mem((Y,Z), transclos(R), [(X,Y)|Seen]).

/* Reflexive-transitive closure */
memRefTransClos((X,Y), reftransclos(R, S)) :- updateToRefClos(S, R, V), memI((X,Y), transclos(V)).
/*
?- memRefTransClos((5,5), reftransclos([(1,2),(2,3),(3,4)],[1,2,3,4])).

no
?- memRefTransClos((1,4), reftransclos([(1,2),(2,3),(3,4)],[1,2,3,4])).

true ? 

yes
*/
/* reflexive-symmetric-transitive closure */
memEquiClos((X,Y), refsymtransclos(R, S)) :- updateToSymClos(R, [], U), updateToRefClos(S, U, V), memI((X,Y), transclos(V)).
/*
append( [ ], L, L).
append( [X|R], L, [X|Z]) :- append(R, L, Z).
*/
/* update relation to symmetric closure */
updateToSymClos([], U, U).
updateToSymClos([(X,Y)|R1], U, V) :- append([(X,Y), (Y,X)], U, NewU), updateToSymClos(R1, NewU, V).

/* update relation to reflexive closure */
updateToRefClos([], U, U).
updateToRefClos([X|R], U, V) :- updateToRefClos(R, [(X,X)|U], V).

/*
?- memEquiClos((4,1),refsymtransclos([(1,2),(3,2),(3,4)],[1,2,3,4])).
true ? 

yes
?- memEquiClos((2,4),refsymtransclos([(1,2),(3,2),(3,4)],[1,2,3,4])).

true ? 

yes
memEquiClos((d,a),refsymtransclos([(a,b),(b,c),(e,d),(c,d)],[a,b,c,d,e])).

true ?

yes

memEquiClos((a,d),refsymtransclos([(a,b),(b,a),(b,c),(e,d),(c,d)],[a,b,c,d,e])).

true ? 

yes
?- memEquiClos((4,2), refsymtransclos([(1,3),(4,1),(5,2),(3,5)],[1,2,3,4,5])).

true ? 

yes
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% part_b
/*  del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */ 

del(X, [ ] , [ ]) :- !.

del(X, [X|R], Z) :- del(X, R, Z), !.

del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.

/*  remdups(L, L1) remove duplicates from a list L to get L1 */

remdups([ ], [ ]) :- !.

remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).

/* Assuming no duplicates in S1, S2 here is an implementation of union of S1, S2 */

unionI([ ], S2, S2) :- !.

unionI(S1, [ ], S1) :- !.

unionI([X|R], S2, [X|Z]) :- del(X, S2, S3),  unionI(R, S3, Z).

/* append(L1, L2, L3) -- append lis  L1 to list L2 to get list  L3 */

append( [ ], L, L).

append( [X|R], L, [X|Z]) :- append(R, L, Z).

/* mapcons(X,L1, L2) --  cons the element X to each list in L1 to get L2 */

mapcons(X, [ ], [ ]) :- !.

mapcons(X, [Y|R], [ [X|Y] | Z ]) :- mapcons(X, R, Z).

/* powerI( S, P1): Here is an implementation of powerset of S */

powerI([ ], [ [ ] ]) :- !.

powerI([X|R], P) :- powerI(R, P1),  mapcons(X, P1, P2), append(P2, P1, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* 
1. sufficient examples  that unionI and powerI indeed implement union and power.
(i) Tried to cover all possible type of cases
?- unionI([1,2,3], [2,3,4], S).
S = [1,2,3,4]
?- unionI([1,2,3], [4,5,6], S).
S = [1,2,3,4,5,6]
?- unionI([1,2,3], [1,2,3], S).
S = [1,2,3]
?- unionI([1,2,3], [], S).
S = [1,2,3]
?- unionI([], [], S).
S = []

(ii)
?- powerI([1,2,3], S).
S = [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]
?- powerI([], S).
S = [[]]
?- powerI([1], S).
S = [[],[1]]
?- powerI([a,b,c,d,e,f,g,h], S).
S = [[a,b,c,d,e,f,g,h],[a,b,c,d,e,f,g],[a,b,c,d,e,f,h],[a,b,c,d,e,f],[a,b,c,d,e,g,h],[a,b,c,d,e,g],[a,b,c,d,e,h],[a,b,c,d,e],[a,b,c,d,f,g,h],[a,b,c,d,f,g],[a,b,c,d,f,h],[a,b,c,d,f],[a,b,c,d,g,h],[a,b,c,d,g],[a,b,c,d,h],[a,b,c,d],[a,b,c,e,f,g,h],[a,b,c,e,f,g],[a,b,c,e,f,h],[a,b,c,e,f],[a,b,c,e,g,h],[a,b,c,e,g],[a,b,c,e,h],[a,b,c,e],[a,b,c,f,g,h],[a,b,c,f,g],[a,b,c,f,h],[a,b,c,f],[a,b,c,g,h],[a,b,c,g],[a,b,c,h],[a,b,c],[a,b,d,e,f,g,h],[a,b,d,e,f,g],[a,b,d,e,f,h],[a,b,d,e,f],[a,b,d,e,g,h],[a,b,d,e,g],[a,b,d,e,h],[a,b,d,e],[a,b,d,f,g,h],[a,b,d,f,g],[a,b,d,f,h],[a,b,d,f],[a,b,d,g,h],[a,b,d,g],[a,b,d,h],[a,b,d],[a,b,e,f,g,h],[a,b,e,f,g],[a,b,e,f,h],[a,b,e,f],[a,b,e,g,h],[a,b,e,g],[a,b,e,h],[a,b,e],[a,b,f,g,h],[a,b,f,g],[a,b,f,h],[a,b,f],[a,b,g,h],[a,b,g],[a,b,h],[a,b],[a,c,d,e,f,g,h],[a,c,d,e,f,g],[a,c,d,e,f,h],[a,c,d,e,f],[a,c,d,e,g,h],[a,c,d,e,g],[a,c,d,e,h],[a,c,d,e],[a,c,d,f,g,h],[a,c,d,f,g],[a,c,d,f,h],[a,c,d,f],[a,c,d,g,h],[a,c,d,g],[a,c,d,h],[a,c,d],[a,c,e,f,g,h],[a,c,e,f,g],[a,c,e,f,h],[a,c,e,f],[a,c,e,g,h],[a,c,e,g],[a,c,e,h],[a,c,e],[a,c,f,g,h],[a,c,f,g],[a,c,f,h],[a,c,f],[a,c,g,h],[a,c,g],[a,c,h],[a,c],[a,d,e,f,g,h],[a,d,e,f,g],[a,d,e,f,h],[a,d,e,f],[a,d,e,g,h],[a,d,e,g],[a,d,e,h],[a,d,e],[a,d,f,g,h],[a,d,f,g],[a,d,f,h],[a,d,f],[a,d,g,h],[a,d,g],[a,d,h],[a,d],[a,e,f,g,h],[a,e,f,g],[a,e,f,h],[a,e,f],[a,e,g,h],[a,e,g],[a,e,h],[a,e],[a,f,g,h],[a,f,g],[a,f,h],[a,f],[a,g,h],[a,g],[a,h],[a],[b,c,d,e,f,g,h],[b,c,d,e,f,g],[b,c,d,e,f,h],[b,c,d,e,f],[b,c,d,e,g,h],[b,c,d,e,g],[b,c,d,e,h],[b,c,d,e],[b,c,d,f,g,h],[b,c,d,f,g],[b,c,d,f,h],[b,c,d,f],[b,c,d,g,h],[b,c,d,g],[b,c,d,h],[b,c,d],[b,c,e,f,g,h],[b,c,e,f,g],[b,c,e,f,h],[b,c,e,f],[b,c,e,g,h],[b,c,e,g],[b,c,e,h],[b,c,e],[b,c,f,g,h],[b,c,f,g],[b,c,f,h],[b,c,f],[b,c,g,h],[b,c,g],[b,c,h],[b,c],[b,d,e,f,g,h],[b,d,e,f,g],[b,d,e,f,h],[b,d,e,f],[b,d,e,g,h],[b,d,e,g],[b,d,e,h],[b,d,e],[b,d,f,g,h],[b,d,f,g],[b,d,f,h],[b,d,f],[b,d,g,h],[b,d,g],[b,d,h],[b,d],[b,e,f,g,h],[b,e,f,g],[b,e,f,h],[b,e,f],[b,e,g,h],[b,e,g],[b,e,h],[b,e],[b,f,g,h],[b,f,g],[b,f,h],[b,f],[b,g,h],[b,g],[b,h],[b],[c,d,e,f,g,h],[c,d,e,f,g],[c,d,e,f,h],[c,d,e,f],[c,d,e,g,h],[c,d,e,g],[c,d,e,h],[c,d,e],[c,d,f,g,h],[c,d,f,g],[c,d,f,h],[c,d,f],[c,d,g,h],[c,d,g],[c,d,h],[c,d],[c,e,f,g,h],[c,e,f,g],[c,e,f,h],[c,e,f],[c,e,g,h],[c,e,g],[c,e,h],[c,e],[c,f,g,h],[c,f,g],[c,f,h],[c,f],[c,g,h],[c,g],[c,h],[c],[d,e,f,g,h],[d,e,f,g],[d,e,f,h],[d,e,f],[d,e,g,h],[d,e,g],[d,e,h],[d,e],[d,f,g,h],[d,f,g],[d,f,h],[d,f],[d,g,h],[d,g],[d,h],[d],[e,f,g,h],[e,f,g],[e,f,h],[e,f],[e,g,h],[e,g],[e,h],[e],[f,g,h],[f,g],[f,h],[f],[g,h],[g],[h],[]]
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 2. Check that union does not have duplicates. */
/* from first example of UnionI we can see that if sets do not contain any duplicates then the union also will not contain the duplicates.
below is when the set contain duplicates
?- unionI([1,1,3], [2,3,4], S).
S = [1,1,3,2,4]
to resolve this we can remove duplicates after finding the union
*/

unionWithNoDuplicates(S1, S2, S3) :- unionI(S1, S2, S4), remdups(S4,S3). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 3. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  interI(S1, S2, S3) that implements intersection of two finite sets.  */

interI(S1, [], []) :- !.
interI([], S2, []) :- !.
interI([X|R1], S2, [X|R3]) :- mem(X, S2), !, interI(R1, S2, R3).
interI([X|R1], S2, S3) :- interI(R1, S2, S3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 4. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  diffI(S1, S2, S3) that implements set-difference of two finite sets. */

diffI(S1, [], S1) :- !.
diffI([], S2, []) :- !.
diffI([X|R1], S2, S3) :- mem(X, S2), !, diffI(R1, S2, S3).
diffI([X|R1], S2, [X|R3]) :- diffI(R1, S2, R3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 5. Assuming no duplicates in lists representing S1 and S2, write a PROLOG program  cartesianI(S1, S2, S3) that implements cartesian of two finite sets. */
tuple(X, [], []) :- !.
tuple(X, [Y|R], [(X,Y)|T]) :- tuple(X, R, T).
cartesianI(S1, [], []) :- !.
cartesianI([], S2, []) :- !.
cartesianI([X|R1], S2, S3) :-  cartesianI(R1, S2, R2), tuple(X, S2, P1), append(P1, R2, S3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 
6. sufficient test cases examples to demonstrate your implementations are correct.
?- interI([1,2,3], [2,3,4], S).
S = [2,3]
?- interI([1,2,3], [4,5,6], S).
S = []
?- interI([1,2,3], [1,2,3], S).
S = [1,2,3]
?- interI([1,2,3], [], S).
S = []
?- diffI([1,2,3], [2,3,4], S).
S = [1]
?- diffI([1,2,3], [4,5,6], S).
S = [1,2,3]
?- diffI([1,2,3], [1,2,3], S).
S = []
?- cartesianI([1,2], [a,b], S).
S = [[1,a],[1,b],[2,a],[2,b]]
?- cartesianI([1,2,3], [a,b,c], S).
S = [[1,a],[1,b],[1,c],[2,a],[2,b],[2,c],[3,a],[3,b],[3,c]]
?- cartesianI([1,2,3], [], S).
S = []
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* 7. a way to check that the powersets obtained from the implementation of two different valid representations of a set (elements given in different order) are equal. */
/* below I have implemented the function to check whether the power sets are equal or not, sets provided may be different or same with a different order*/
/* However, also we can just check the sets because equal sets will have same power sets */
sortPowerSets([], []) :- !.
sortPowerSets([X|R], [Y|S]) :-  sort(X, Y), sortPowerSets(R, S).
powersetEqual(S1, S2) :- powerI(S1, P1), powerI(S2, P2), sortPowerSets(P1, P11), sortPowerSets(P2, P21), sort(P11, P12), sort(P21, P22), P12 = P22.