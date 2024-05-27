add_cl(X, [H|T], [H|R]):- add_cl(X, T, R).
add_cl(X, [], [X]).

append_dl(LS1, LE1, LS2, LE2, RS, RE):- RS = LS1, LE1=LS2, RE=LE2.

quicksort([H|T], R):-
    partition(H, T, Sm, Lg),
    quicksort(Sm, SmS),
    quicksort(Lg, LgS),
    append(SmS, [H|LgS], R).
quicksort([],[]).

quicksort_dl([H|T], S, E):- % a new argument was added
    partition(H, T, Sm, Lg), % partition predicate remains the same
    quicksort_dl(Sm, S, [H|L]), % implicit concatenation
    quicksort_dl(Lg, L, E).

quicksort_dl([], L, L). % stopping condition has been changed
partition(P, [X|T], [X|Sm], Lg):- X<P, !, partition(P, T, Sm, Lg).
partition(P, [X|T], Sm, [X|Lg]):- partition(P, T, Sm, Lg).
partition(_, [], [], []).

inorder(t(K,L,R),List):-
 inorder(L,ListL), 
 inorder(R,ListR), 
 append1(ListL,[K|ListR],List).
inorder(nil,[]).

% when we reached the end of the tree we unify the beginning and end 
% of the partial result list – representing an empty list as a difference list
inorder_dl(nil,L,L). 
inorder_dl(t(K,L,R),LS,LE):-
%obtain the start and end of the lists for the left and right subtrees 
 inorder_dl(L,LSL,LEL), 
 inorder_dl(R,LSR,LER), 
 % the start of the result list is the start of the left subtree list
 LS=LSL, 
 % key K is inserted between the end of left and the start of right
 LEL=[K|LSR], 
 % the end of the result list is the end of the right subtree list
 LE=LER.

inorder_dl2(nil,L,L).
inorder_dl2(t(K,L,R),LS,LE):-
 inorder_dl2(L, LS, [K|LT]), 
 inorder_dl2(R, LT, LE).
 
 %ex1

convertIL2CL(L, []) :- var(L), !.
convertIL2CL([H|T], [H|Rest]):- convertIL2CL(T, Rest).          


convertCL2IL([], _).
convertCL2IL([H|T], [H|Rest]) :-
    convertCL2IL(T, Rest).

convertCL2DL([], End, End).
convertCL2DL([X|Xs], [X|Start], End) :-
    convertCL2DL(Xs, Start, End).

convertDL2CL(Start, Start, []):- var(Start), !.
convertDL2CL([X|Start], End, [X|Result]) :-
    convertDL2CL(Start, End, Result).

%ex2

convertIL2DL([], End, End):-var(End), !.
convertIL2DL([X|Xs], [X|Start], End) :-
    convertIL2DL(Xs, Start, End).

convertDL2IL(Start, _ , _):- var(Start), !.
convertDL2IL([X|Start], End, [X|Result]) :-
    convertDL2IL(Start, End, Result).    

%ex3

flat_dl([], LE, LE).
flat_dl([H|T], Start, End):- atomic(H), !, flat_dl(T, IS, IE), Start = [H|IS], End = IE.
flat_dl([H|T], Start, End):- flat_dl(H, Start1, End1), flat_dl(T, Start2, End2), Start = Start1, End1 = Start2, End = End2.

%ex5

% Trees:
complete_tree(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))).
incomplete_tree(t(6, t(4,t(2,_,_),t(5,_,_)), t(9,t(7,_,_),_))).

preorder_dl(nil, L, L).
preorder_dl(t(K, L, R), [K|LS], LE) :-
    preorder_dl(L, LS, LT),
    preorder_dl(R, LT, LE).


postorder_dl(nil,L,L).
postorder_dl(t(K,L,R),LS,LE):-
    postorder_dl(L, LS, LT), 
    postorder_dl(R, LT, [K|LE]).

 %ex6

append_dl([], L, L).
append_dl([H|T], L, [H|R]) :- append_dl(T, L, R).

even_dl(nil, E, E).
even_dl(t(K, L, R), S, E) :-
    K mod 2 =:= 0, !, 
    even_dl(L, S, [K|T]),
    even_dl(R, T, E).
even_dl(t(_, L, R), S, E) :-
    even_dl(L, S, E1),
    even_dl(R, E1, E).

%ex7

between_dl(nil, S, E, _, _) :- S = E.
between_dl(t(K, L, R), S, E, K1, K2) :-
    K > K1,
    K < K2,
    between_dl(L, S, [K|T], K1, K2),
    between_dl(R, T, E, K1, K2).
between_dl(t(K, _, R), S, E, K1, K2) :-
    K =< K1,
    between_dl(R, S, E, K1, K2).
between_dl(t(K, L, _), S, E, K1, K2) :-
    K >= K2,
    between_dl(L, S, E, K1, K2).


%ex8    

collect_depth_k(nil, _, E, E).
collect_depth_k(t(K, _, _), 1, [K|E], E).
collect_depth_k(t(_, L, R), K, S, E) :-
    K > 1,
    K1 is K - 1,
    collect_depth_k(L, K1, S, E1),
    collect_depth_k(R, K1, E1, E).