%ex1

convertIL2CL([], R):- var(R), !.
convertIL2CL([H|T], [H|Rest]):- convertIL2CL(T, Rest).

convertCL2IL([],R):- var(R), !.
convertCL2IL([H|T], [H|Rest]):-
    convertCL2IL(T,Rest).

convertIT2CT(L,nil):- var(L), !.
convertIT2CT(t(Key, Left, Right), t(Key, LeftCT, RightCT)) :-
    convertIT2CT(Left, LeftCT),
    convertIT2CT(Right, RightCT).
convertIT2CT(nil, t(_,nil,nil)).

convertCT2IT(nil, R):- var(R), !.
convertCT2IT(t(Key, Left, Right), t(Key, LeftIT, RightIT)) :-
    convertCT2IT(Left, LeftIT),
    convertCT2IT(Right, RightIT).
convertCT2IT(t(_, nil, nil), nil).

%ex2

append_il(A, L, L) :- var(A), !.
append_il([X|Xs], Ys, [X|Zs]) :-
    append_il(Xs, Ys, Zs).


%ex3 

reverse_il_bwd(L, _):- var(L), !.
reverse_il_bwd([H|T], R):- reverse_il_bwd(T, Rtail), append_il(Rtail, [H|_], R).

reverse_il_fwd(L, R):- reverse_il_fwd(L, _, R).
reverse_il_fwd(L, Acc, Acc):- var(L), !.
reverse_il_fwd([H|T], Acc, R) :- Acc1=[H|Acc], reverse_il_fwd(T, Acc1, R).

%ex4

flat_il(L,_):-var(L), !.
flat_il([H|T], [H|R]):- atomic(H), !, flat_il(T,R).
flat_il([H|T], R):- flat_il(H,R1), flat_il(T,R2), append_il(R1,R2,R).

%ex5 

incomplete_tree(t(7, t(5, t(3, _, _), t(6, _, _)), t(11, _, _))).
complete_tree(t(7, t(5, t(3, nil, nil), t(6, nil, nil)), t(11, nil, nil))).

%ex6

preorder_it(L, _):-var(L), !.
preorder_it(t(K,L,R), List):-
	preorder_it(L,LL), 
	preorder_it(R, LR), 
	append_il([K|LL], LR, List).


%ex7 

height_it(L, 0):-var(L), !.
height_it(t(_, L, R), H):-
    height_it(L, H1),
    height_it(R, H2),
	max(H1, H2, H3), 
	H is H3+1.
max(A, B, A):-A>B, !.
	max(_, B, B).


%ex8

diam_it(L, 0):- var(L), !.
diam_it(t(_, Left, Right), Diameter) :-
    height_it(Left, LeftHeight),
    height_it(Right, RightHeight),
    diam_it(Left, LeftDiameter),
    diam_it(Right, RightDiameter),
    Diameter is max(LeftHeight + RightHeight + 1, max(LeftDiameter, RightDiameter)).


%ex9

subl_il(L, _):-var(L), !.
subl_il(L, R):- 
	append_il(_, SF, R),
	append_il(L, _, SF).