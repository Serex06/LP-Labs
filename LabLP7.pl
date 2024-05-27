%

tree1(t(6, t(4,t(2,nil,nil),t(5,nil,nil)), t(9,t(7,nil,nil),nil))).
tree2(t(8, t(5, nil, t(7, nil, nil)), t(9, nil, t(11, nil, nil)))).

% left sub-tree, key, right sub-tree (order in append)
inorder(t(K,L,R), List):-
inorder(L,LL), 
inorder(R,LR), 
append(LL, [K|LR], List).
inorder(nil, []).
% key, left sub-tree, right sub-tree (order in append)
preorder(t(K,L,R), List):-
preorder(L,LL), 
preorder(R, LR), 
append([K|LL], LR, List).
preorder(nil, []).
% left sub-tree, right sub-tree, key (order in appends)
postorder(t(K,L,R), List):-
postorder(L,LL), 
postorder(R, LR), 
append(LL, LR,R1), 
append(R1, [K], List).
postorder(nil, []).



% wrapper
pretty_print(T):- pretty_print(T, 0).
% predicate that prints the tree


pretty_print(nil, _).
pretty_print(t(K,L,R), D):-
	D1 is D+1,
	pretty_print(L, D1), 
	print_key(K, D), 
	pretty_print(R, D1). 

% print_key/2 prints key K at D tabs from the left margin
% and inserts a new line
print_key(K, D):-D>0, !, D1 is D-1, tab(8), print_key(K, D1).
print_key(K, _):-write(K), nl.


height(nil, 0).
height(t(_, L, R), H):-
height(L, H1), 
height(R, H2),
max(H1, H2, H3), 
H is H3+1.
max(A, B, A):-A>B, !.
max(_, B, B).

ternary_tree(
t(6, 
t(4, 
t(2, nil, nil, nil), 
nil, 
t(7, nil, nil, nil)), 
t(5, nil, nil, nil), 
t(9, 
t(3, nil, nil, nil),
nil,
nil)
)
).

ternary_inorder(t(K,L,M,R), List):-
ternary_inorder(L,LL), 
ternary_inorder(M, LM),
ternary_inorder(R,LR), 
append(LL, [K|LM], Temp),
append(Temp, LR, List).
ternary_inorder(nil, []).


ternary_preorder(t(K,L,M,R), List):-
ternary_preorder(L,LL), 
ternary_preorder(M,ML), 
ternary_preorder(R, LR), 
append([K|LL], ML, Temp),
append(Temp, LR, List).
ternary_preorder(nil, []).


ternary_postorder(t(K,L,M,R), List):-
ternary_postorder(L,LL), 
ternary_postorder(M,ML), 
ternary_postorder(R, LR), 
append(LL, ML,R1), 
append(R1, LR, R2),
append(R2, [K], List).
ternary_postorder(nil, []).


test_traversal(FN,R):-
	ternary_tree(T),
	call(FN, T, R).
	

pretty_print_ternary(T):- pretty_print_ternary(T, 0).

pretty_print_ternary(nil, _).
pretty_print_ternary(t(K,L,M,R), D):-
	D1 is D+1,
	print_key(K, D), 
	pretty_print_ternary(L, D1),
	pretty_print_ternary(M, D1), 
	pretty_print_ternary(R, D1). 

ternary_height(nil, 0).
ternary_height(t(_, Left, Middle, Right), Height) :-
    ternary_height(Left, LeftHeight),
    ternary_height(Middle, MiddleHeight),
    ternary_height(Right, RightHeight),
    MaxHeight12 is max(LeftHeight, MiddleHeight),
    MaxHeight is max(MaxHeight12, RightHeight),
    Height is MaxHeight + 1.

leaf_list(nil,[]).
leaf_list(t(Key, nil, nil),[Key]) :- !.
leaf_list(t(_, Left, Right), List):-
    leaf_list(Left, LeftList),
    leaf_list(Right, RightList),
    append(LeftList, RightList, List).

diam(nil, 0).
diam(t(_, Left, Right), Diameter) :-
	height(Left, LeftHeight),
	height(Right, RightHeight),
	diam(Left, LeftDiameter),
	diam(Right, RightDiameter),
	Diameter is max(LeftHeight + RightHeight + 1, max(LeftDiameter, RightDiameter)).

same_depth(nil, _,  []).
same_depth(t(Key, _, _), 1, [Key]).
same_depth(t(_, Left, Right),D,  List) :-
	D > 1,
	NewD is D-1, 
    same_depth(Left, NewD ,LeftList),
    same_depth(Right, NewD, RightList),
    append(LeftList, RightList, List).
	

symmetric(nil).  
symmetric(t(_, Left, Right)) :-
    compareSubtrees(Left, Right).

compareSubtrees(nil, nil). 
compareSubtrees(t(_, Left1, Right1), t(_, Left2, Right2)) :-
    compareSubtrees(Left1, Right2),
    compareSubtrees(Right1, Left2).
	

internal_list(nil, []).  
internal_list(t(_, nil, nil), []). 
internal_list(t(Key, Left, Right), [Key|List]) :-
    internal_list(Left, LeftList),
    internal_list(Right, RightList),
    append(LeftList, RightList, List).	