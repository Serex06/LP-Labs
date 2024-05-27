edge(1,2).
edge(1,5).
edge(2,3).
edge(2,5).
edge(3,4).
edge(4,5).
edge(4,6).
is_edge(X,Y):- edge(X,Y);edge(Y,X).


edge_ex1(a,b).
edge_ex1(a,c).
edge_ex1(b,d).
edge_ex1(d,e).
edge_ex1(c,f).
edge_ex1(e,g).
edge_ex1(f,h).

is_edge2(X, Y) :- edge_ex1(X, Y); edge_ex1(Y, X).




:- dynamic visited_node/1.
dfs(X,_) :- 
	df_search(X). 
dfs(_,L) :- 
	!, 
	collect_reverse([], L).
df_search(X):-
	asserta(visited_node(X)),
	is_edge2(X,Y),
	not(visited_node(Y)),
	df_search(Y).

collect_reverse(L, P):-
	retract(visited_node(X)), !, 
	collect_reverse([X|L], P).
collect_reverse(L,L).


:- dynamic visited_node/1.
:- dynamic queue/1. 
bfs(X, _):-
	assertz(visited_node(X)),
	assertz(queue(X)),
	bf_search.
bfs(_,R):-
	!,
	collect_reverse([],R).
bf_search:-
	retract(queue(X)),
	expand(X), !,
	bf_search.
expand(X):-
	is_edge(X,Y),
	not(visited_node(Y)),
	asserta(visited_node(Y)), 
	assertz(queue(Y)), 
	fail. 
expand(_).


pos_vec(start,0,2,[a,d]).
pos_vec(a,2,0,[start,b]).
pos_vec(b,5,0,[a,c, end]).
pos_vec(c,10,0,[b, end]).
pos_vec(d,3,4,[start,e]).
pos_vec(e,7,4,[d]).
pos_vec(end,7,2,[b,c]).
is_target(end).



best([], []):-!.
best([[Target|Rest]|_], [Target|Rest]):- is_target(Target),  reverse([Target|Rest], Path),writeln(Path), !.
best([[H|T]|Rest], Best):-
	writeln([[H|T]|Rest]),
	pos_vec(H,_,_, Neighb),
	expand(Neighb, [H|T], Rest, Exp),
	quick_sort(Exp, SortExp, []),
	best(SortExp, Best).


expand([],_,Exp,Exp):- !.
expand([H|T],Path,Rest,Exp):-
	\+(member(H,Path)),
	!,
	expand(T,Path,[[H|Path]|Rest],Exp).
	expand([_|T],Path,Rest,Exp):- expand(T,Path,Rest,Exp).


quick_sort([H|T],S,E):-
	partition(H,T,A,B),
	quick_sort(A,S,[H|Y]),
	quick_sort(B,Y,E).
quick_sort([],S,S).

partition(H,[A|X],[A|Y],Z):- order(A,H), !, partition(H,X,Y,Z).
partition(H,[A|X],Y,[A|Z]):- partition(H,X,Y,Z).
partition(_,[],[],[]).

dist(Node1,Node2,Dist):-
	pos_vec(Node1, X1, Y1, _),
	pos_vec(Node2, X2, Y2, _),
	Dist is (X1-X2)*(X1-X2)+(Y1-Y2)*(Y1-Y2).
	order([Node1|_],[Node2|_]):-
	is_target(Target),
	dist(Node1,Target,Dist1),
	dist(Node2,Target,Dist2),
	Dist1<Dist2.


depth_max(3).

dls(X, _):-
    depth_max(MaxDepth),
    dl_search(X, MaxDepth).
dls(_, L):-
    !,
    collect_reverse([], L).
dl_search(X, Depth):-
    Depth > 0,
    NewDepth is Depth - 1,
    asserta(visited_node(X)),
    is_edge2(X, Y),
    not(visited_node(Y)),
    dl_search(Y, NewDepth).