delete1(X, [X|T], T).
delete1(X, [H|T], [H|R]) :- delete1(X, T, R).
delete1(_, [], []).

min2([H|T], M) :- min2(T, M), M<H, !.
min2([H|_], H).


max1([], Mp, M) :- M=Mp.
max1([H|T], Mp, M) :- H>Mp, !, max1(T, H, M).
max1([_|T], Mp, M) :- max1(T, Mp, M).
max1_pretty([H|T], M) :- max1(T, H, M). 



is_ordered([H1, H2|T]):- H1 =< H2, is_ordered([H2|T]).
is_ordered([_]). % if one element, it is already ordered


sel_sort(L, [M|R]):- min2(L, M), delete1(M, L, L1), !, sel_sort(L1, R).
sel_sort([], []).

ins_sort([H|T], R):- ins_sort(T, R1), insert_ord(H, R1, R).
ins_sort([], []).
insert_ord(X, [H|T], [H|R]):-X>H, !, insert_ord(X, T, R).
insert_ord(X, T, [X|T]).


bubble_sort(L,R):- one_pass(L,R1,F), nonvar(F), !, bubble_sort(R1,R).
bubble_sort(L,L). 
one_pass([H1,H2|T], [H2|R], F):- H1>H2, !, F=1, one_pass([H1|T],R,F).
one_pass([H1|T], [H1|R], F):- one_pass(T, R, F).
one_pass([], [] ,_).


quick_sort([H|T], R):- % pivot is chosen as first element
partition(H, T, Sm, Lg), 
quick_sort(Sm, SmS), % sort sublist with elements smaller than pivot
quick_sort(Lg, LgS), % sort sublist with elements larger than pivot
append(SmS, [H|LgS], R).
quick_sort([], []).

partition(P, [X|T], [X|Sm], Lg):- X<P, !, partition(P, T, Sm, Lg).
partition(P, [X|T], Sm, [X|Lg]):- partition(P, T, Sm, Lg).
partition(_, [], [], []).


merge_sort(L, R):-
split(L, L1, L2), % splits L into 2 equal-length sub-lists
merge_sort(L1, R1),
merge_sort(L2, R2), 
merge(R1, R2, R). % merges the ordered sub-lists
% split fails if the list contains one or no elements
merge_sort([H], [H]). 
merge_sort([], []).
split(L, L1, L2):-
length(L, Len), 
Len>1, 
K is Len/2, 
splitK(L, K, L1, L2).
splitK([H|T], K, [H|L1], L2):- K>0,!,K1 is K-1,splitK(T, K1, L1, L2).
splitK(T, _, [], T).



merge([H1|T1], [H2|T2], [H1|R]):-H1<H2, !, merge(T1, [H2|T2], R).
merge([H1|T1], [H2|T2], [H2|R]):-merge([H1|T1], T2, R).
merge([], L, L).
merge(L, [], L).


sel_sort_max(L, [M|R]):- max1_pretty(L, M), delete1(M, L, L1), !, sel_sort_max(L1, R).
sel_sort_max([], []).



ins_sort_fwd(List, R) :-
    ins_sort_fwd(List, [], R).

ins_sort_fwd([H|T], Acc, R) :-
    insert_ord_fwd(H, Acc, NewAcc),
    ins_sort_fwd(T, NewAcc, R).
ins_sort_fwd([], Acc, Acc).

insert_ord_fwd(X, [H|T], [H|R]) :-
    X > H,
    !,
    insert_ord_fwd(X, T, R).
insert_ord_fwd(X, T, [X|T]).


one_pass2([H1,H2|T], [H2|R]):- H1>H2, !, one_pass2([H1|T],R).
one_pass2([H1|T], [H1|R]):- one_pass2(T, R).
one_pass2([], []).

bubble_sort_fixed(L, R, Pass):-NewPass is Pass - 1,  Pass>0, ! , one_pass2(L,R1),  bubble_sort_fixed(R1, R, NewPass).
bubble_sort_fixed(L, L, 0). 

sort_chars(List, Result) :-
    maplist(char_code, List, CharCodes),
    quick_sort(CharCodes, SortedCharCodes),
    maplist(char_code, Result, SortedCharCodes).



%?- sort_lens([[a, b, c], [f], [2, 3, 1, 2], [], [4, 4]], R).
%R = [[], [f], [4, 4], [a, b, c], [2, 3, 1, 2]] ;
%false

sort_lens(List, R) :-
    quick_sort2(List, R).

quick_sort2([H|T], R) :-
    partition2(H, T, Sm, Lg),
    quick_sort2(Sm, SmS),
    quick_sort2(Lg, LgS),
    append(SmS, [H|LgS], R).
quick_sort2([], []).


perm(L, [H|R]):- append(A, [H|T], L), append(A, T, L1), perm(L1, R).
perm([], []).


perm1([], []).
perm1(List, [X|Perm]) :-
    select(X, List, Rest),
    perm1(Rest, Perm).


partition2(_, [], [], []).
partition2(P, [X|T], [X|Sm], Lg) :-
    length(X, LenX),
    length(P, LenP),
    LenX < LenP,
    !,
    partition2(P, T, Sm, Lg).
partition2(P, [X|T], Sm, [X|Lg]) :-
    partition2(P, T, Sm, Lg).