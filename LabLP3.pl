% Exercise 1

append3([], L2, L3, L2L3) :-
    append(L2, L3, L2L3).
append3([H|T], L2, L3, [H|Result]) :-
    append3(T, L2, L3, Result).

% Exercise 2

add_first(X,L,[X|L]).

% Exercise 3 

sum_bwd([],0).
sum_bwd([X|Xs],Sum):-
    sum_bwd(Xs, Rest),
    Sum is X + Rest.

sum_bwd([], 0).
sum_bwd([X|Xs], Sum):-
    sum_bwd(Xs, S),
    Sum is X + S. 

sum_fwd(List, Sum) :-
    sum_fwd(List, 0, Sum).

sum_fwd([], Sum, Sum). 
sum_fwd([H|T], Acc, Sum) :-
    NewAcc is Acc + H,   
    sum_fwd(T, NewAcc, Sum).

% Exercise 4

% empty list with empty list of odd and even numbers
separate_parity([],[],[]).
% take head of the list even , add it to the list of even numbers
separate_parity([X|Xs],[X|Even],Odd):-
    0 is X mod 2,
    separate_parity(Xs,Even,Odd).
% take head of the list odd , add it to the list of odd numbers
separate_parity([X|Xs],Even,[X|Odd]):-
    1 is X mod 2,
    separate_parity(Xs,Even,Odd).

% Exercise 5 

% First occurence

remove_duplicates([], []).
remove_duplicates([X|Xs], [X|Ys]):-
    delete(Xs, X, Rest),
    remove_duplicates(Rest, Ys).

% Last occurence

remove_duplicates_last(List, Result) :-
    reverse(List, Reversed),
    remove_duplicates(Reversed, Temp),
    reverse(Temp, Result).    

% Exercise 6

replace_all(_, _, [], []).s
replace_all(X, Y, [X|Xs], [Y|Ys]) :-
    replace_all(X, Y, Xs, Ys).
replace_all(X, Y, [Z|Xs], [Z|Ys]) :-
    dif(Z, X),
    replace_all(X, Y, Xs, Ys).

% Exercise 7 

drop_k(List, K, Result) :-
    drop_k(List, K, K, Result).

drop_k([], _, _, []).
drop_k([_|Xs], K, 1, Result) :-
    drop_k(Xs, K, K, Result).
drop_k([X|Xs], K, I, [X|Result]) :-
    I > 1,
    NextI is I - 1,
    drop_k(Xs, K, NextI, Result).
drop_k([_|Xs], K, I, Result) :-
    I > 1,
    NextI is I - 1,
    drop_k(Xs, K, NextI, Result).


% Exercise 8 

remove_consecutive_duplicates([], []).
remove_consecutive_duplicates([X], [X]).
remove_consecutive_duplicates([X,X|Xs], Result) :-
    remove_consecutive_duplicates([X|Xs], Result).
remove_consecutive_duplicates([X,Y|Xs], [X|Result]) :-
    dif(X, Y),
    remove_consecutive_duplicates([Y|Xs], Result).
