

count_atomic([],0).
count_atomic([H|T], Count):- atomic(H), !, count_atomic(T, NewCount), Count is NewCount + 1.
count_atomic([H|T], Count):- count_atomic(H, NewCount1), count_atomic(T, NewCount2), Count is NewCount1 + NewCount2.


sum_atomic([], 0).
sum_atomic([H|T], S) :- atomic(H), !, sum_atomic(T, S1), S is S1 + H.
sum_atomic([H|T], S) :- sum_atomic(H, S1), sum_atomic(T, S2), S is S1 + S2.



member_deterministic(H, [H|_]):- !.
member_deterministic(X, [H|_]):- member_deterministic(X,H), !. 
member_deterministic(X, [_|T]):- member_deterministic(X,T).


replace(_, _, [], []).
replace(X, Y, [X|T], [Y|TR]) :- 
    atomic(X),
    !,
    replace(X, Y, T, TR).
replace(X, Y, [H|T], [HR|TR]) :- 
    replace(X, Y, H, HR),!, 
    replace(X, Y, T, TR).
replace(X, Y, [H|T], [H|TR]) :- 
    replace(X, Y, T, TR).


lasts([], []):- !.
lasts([X], [X]):- atomic(X), !.
lasts([H], R):- lasts(H, R).
lasts([H|T], R):- atomic(H), !, 
    lasts(T, R).
lasts([H|T], R):-
    lasts(H, R1), 
    lasts(T, R2),!,
    append(R1, R2, R).


len_con_depth([], Len, [Len]).
len_con_depth([H|T], Len, R):- atomic(H), !, 
    Len1 is Len + 1, 
    len_con_depth(T, Len1, R).
len_con_depth([H|T], 0, [RH|RT]):- 
    len_con_depth(H, 0, RH), 
    len_con_depth(T, 0, RT),!.
len_con_depth([H|T], Len, [Len, RH|RT]):- 
    len_con_depth(H, 0, RH),
    len_con_depth(T, 0, RT),!.
