% Exercise 3

del_min(L, R) :-
    min_list(L, Min),
    delete_all(L, Min, R).

del_max(L, R) :-
    max_list(L, Max),
    delete_all(L, Max, R).

delete_all([], _, []).
delete_all([X|Xs], X, Ys) :-
    delete_all(Xs, X, Ys).
delete_all([X|Xs], E, [X|Ys]) :-
    dif(X, E),
    delete_all(Xs, E, Ys).

% Exercise 4

reverse_k([], _, []).
reverse_k(List, K, Result):-
	length(ListUntilK, K),
	append(ListUntilK, RestOFList, List),
	reverse(RestOFList, ReversedPart),
	append(ListUntilK, ReversedPart, Result).

% Exercise 5

rle_encode([], []).
rle_encode([X], [[X,1]]).
rle_encode([X,X|T], [[X,N1]|R]) :-
    rle_encode([X|T], [[X,N]|R]),
    N1 is N+1.
rle_encode([X,Y|T], [[X,1]|R]) :-
    X \= Y,
    rle_encode([Y|T], R).


% Exercise 6

rotate_left(List, 0, List).

rotate_left([X|Xs], N, Rotated) :-
    N > 0,
    append(Xs, [X], RotatedRest),
    N1 is N - 1,
    rotate_left(RotatedRest, N1, Rotated).

rotate_right(List, K, Rotated) :-
    length(List, Len),
    K1 is Len - K mod Len, 
    rotate_left(List, K1, Rotated).

