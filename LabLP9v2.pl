
add_cl(X, List, Result) :- 
    add_cl_helper(X, List, Result).

add_cl_helper(X, [], [X]).
add_cl_helper(X, [H|T], [H|Result]) :- 
    add_cl_helper(X, T, Result).


append_dl(LS1, LE1, LS2, LE2, RS, RE) :-
    RS = LS1,
    LE1 = LS2,
    RE = LE2.


quicksort([H|T], Sorted) :-
    partition(H, T, Smaller, Larger),
    quicksort(Smaller, SmallerSorted),
    quicksort(Larger, LargerSorted),
    append(SmallerSorted, [H|LargerSorted], Sorted).
quicksort([], []).


quicksort_dl([H|T], Start, End) :-
    partition(H, T, Smaller, Larger),
    quicksort_dl(Smaller, Start, [H|Middle]),
    quicksort_dl(Larger, Middle, End).
quicksort_dl([], L, L).


partition(_, [], [], []).
partition(Pivot, [X|Xs], [X|Smaller], Larger) :- 
    X < Pivot, !,
    partition(Pivot, Xs, Smaller, Larger).
partition(Pivot, [X|Xs], Smaller, [X|Larger]) :- 
    partition(Pivot, Xs, Smaller, Larger).


inorder(t(K, L, R), List) :-
    inorder(L, LeftList),
    inorder(R, RightList),
    append(LeftList, [K|RightList], List).
inorder(nil, []).


inorder_dl(nil, L, L).
inorder_dl(t(K, L, R), Start, End) :-
    inorder_dl(L, Start, [K|Middle]),
    inorder_dl(R, Middle, End).


inorder_dl2(nil, L, L).
inorder_dl2(t(K, L, R), Start, End) :-
    inorder_dl2(L, Start, [K|Middle]),
    inorder_dl2(R, Middle, End).


convertIL2CL(List, []) :- 
    var(List), !.
convertIL2CL([H|T], [H|Result]) :- 
    convertIL2CL(T, Result).


convertCL2IL([], _).
convertCL2IL([H|T], [H|Result]) :-
    convertCL2IL(T, Result).


convertIL2DL(List, Start, End) :-
    append(List, End, Start).


convertDL2IL(Start, _, _) :- 
    var(Start), !.
convertDL2IL([H|Start], End, [H|Result]) :-
    convertDL2IL(Start, End, Result).


flat_dl([], End, End).
flat_dl([H|T], Start, End) :-
    atomic(H), !,
    flat_dl(T, [H|Start], End).
flat_dl([H|T], Start, End) :-
    flat_dl(H, Start, Middle),
    flat_dl(T, Middle, End).


even_dl(nil, E, E).
even_dl(t(K, L, R), Start, End) :-
    (   K mod 2 =:= 0 ->
        even_dl(L, Start, [K|Middle]),
        even_dl(R, Middle, End)
    ;   even_dl(L, Start, Middle),
        even_dl(R, Middle, End)
    ).


between_dl(nil, S, E, _, _) :- S = E.
between_dl(t(K, L, R), Start, End, K1, K2) :-
    (   K > K1, K < K2 ->
        between_dl(L, Start, [K|Middle], K1, K2),
        between_dl(R, Middle, End, K1, K2)
    ;   K =< K1 ->
        between_dl(R, Start, End, K1, K2)
    ;   between_dl(L, Start, End, K1, K2)
    ).

collect_depth_k(nil, _, E, E).
collect_depth_k(t(K, _, _), 1, [K|E], E).
collect_depth_k(t(_, L, R), K, Start, End) :-
    K > 1,
    K1 is K - 1,
    collect_depth_k(L, K1, Start, Middle),
    collect_depth_k(R, K1, Middle, End).
