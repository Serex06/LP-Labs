node(a). 
node(b). %etc
edge(a,b). 
edge(b,a).
edge(b,c). 
edge(c,b). %etc


is_edge(X,Y):- edge(X,Y); edge(Y,X).


neighbor(a, [b, d]).
neighbor(b, [a, c, d]).
neighbor(c, [b, d]).
neighbor(h, []). %etc

	% we declare the predicate as dynamic to be able to use retract
	:-dynamic neighbor/2. 
	% as the neighbour predicate is introduced in the file, 
	% it is considered static only through the introduction of the 
	% dynamic declaration can we use the retract operation on it
	% an example graph – 1st connected component of the example graph
	neighbor(a, [b, d]). 
	neighbor(b, [a, c, d]).
	neighbor(c, [b, d]).
	%etc.
	neighb_to_edge:-
	 % extract the first neighbour predicate
	 retract(neighbor(Node,List)),!, 
	 % and process it
	 process(Node,List),
	 neighb_to_edge.
	neighb_to_edge. % if no more neighbor/2 predicates remain then we stop
	% processing presumes the addition of edge & node predicates 
	% for each neighbour predicate, we first add the edges 
	% until the list is empty and then add the node predicate
	process(Node, [H|T]):- assertz(gen_edge(Node, H)), process(Node, T).
	process(Node, []):- assertz(gen_node(Node)).
	
	
	neighb_to_edge_v2:-
	 neighbor(Node,List), % access the fact
	 process(Node,List),
	 fail.
	neighb_to_edge_v2.
	
	
	% path(Source, Target, Path)
% the partial path starts with the source node – it is a wrapper
path(X, Y, Path):-path(X, Y, [X], Path). 
% when source (1st argument) is equal to target (2nd argument), 
% we finished the path and we unify the partial and final paths.
path(Y, Y, PPath, PPath).
path(X, Y, PPath, FPath):-
 edge(X, Z), % search for an edge
 not(member(Z, PPath)), % that was not traversed
 path(Z, Y, [Z|PPath], FPath). % add to partial result



% restricted_path(Source, Target, RestrictionsList, Path)
restricted_path(X,Y,LR,P):-
 path(X,Y,P), 
 reverse(P,PR), 
 check_restrictions(LR, PR).
% predicate that verifies the restrictions
check_restrictions([],_):- !.
check_restrictions([H|TR], [H|TP]):- !, check_restrictions(TR,TP).
check_restrictions(LR, [_|TP]):-check_restrictions(LR,TP).


:- dynamic sol_part/2.
% optimal_path(Source, Target, Path)
optimal_path(X,Y,Path):-
 asserta(sol_part([], 100)), % 100 = max initial distance
 path(X, Y, [X], Path, 1).
optimal_path(_,_,Path):-
 retract(sol_part(Path,_)).
% path(Source, Target, PartialPath, FinalPath, PathLength)
% when target is equal to source, we save the current solution
path(Y,Y,Path,Path,LPath):-
 % we retract the last solution
 retract(sol_part(_,_)),!, 
 % save current solution
 asserta(sol_part(Path,LPath)), 
 % search for another solution
 fail.
path(X,Y,PPath,FPath,LPath):-
 edge(X,Z),
 not(member(Z,PPath)),
 % compute partial distance
 LPath1 is LPath+1,
 % extract distance from previous solution
 sol_part(_,Lopt),
 % if current distance is smaller than the previous distance, 
 LPath1<Lopt,
 % we keep going
 path(Z,Y,[Z|PPath],FPath,LPath1).
 


edge_ex1(a, b).
edge_ex1(b, c).
edge_ex1(a, c).
edge_ex1(c, d).
edge_ex1(b, d).
edge_ex1(d, e).
edge_ex1(e, a).

% hamilton(NumOfNodes, Source, Path)
hamilton(NN, X, Path):- NN1 is NN-1, hamilton_path(NN1, X, X, [X],Path).

hamilton_path(0, _, Start, Path, [Start|Path]).
hamilton_path(NN1, Current, Start, Path, FullPath) :-
    NN1 > 0,
	edge_ex1(Current, Next),
	\+(member(Next, Path)),
	NN2 is NN1 - 1,
	hamilton_path(NN2, Next, Start, [Next|Path], FullPath).


% euler(NE, S, R). – where S is the source node 
% and NE the number of edges in the graph
edge_ex2(a,b).
edge_ex2(b,e).
edge_ex2(c,a).
edge_ex2(d,c).
edge_ex2(e,d).

euler(NE, S, R):-
	edge_ex2(S, Rand),
	NE1 is NE -1,
	euler_path(NE1, Rand, S, [[S, Rand]], R).
	
euler_path(0, _, _, R, R).
euler_path(NE, C, S, R, FP):-
	NE > 0,
	edge_ex2(C, Next),
	\+(member(R, [C, Next])),
	\+(member(R, [Next, C])),
	NN2 is NE - 1,
	euler_path(NN2, Next, S, [[C, Next]|R], FP).



edge_ex3(a, b).
edge_ex3(a, c).
edge_ex3(c, e).
edge_ex3(e, a).
edge_ex3(b, d).
edge_ex3(d, a).

cycle(X, R) :-
    find_cycle(X, [], R).

find_cycle(X, Visited, Cycle) :-
    edge_ex3(X, Next),        
    check_cycle(Next, [X|Visited], Cycle).

check_cycle(X, Visited, Cycle) :-
    member(X, Visited),!,                          
    reverse([X|Visited], Cycle).

check_cycle(X, Visited, Cycle) :-
    \+ member(X, Visited),      
    edge_ex3(X, Next),          
    check_cycle(Next, [X|Visited], Cycle).


neighb_ex4(a, [b,c]).
neighb_ex4(b, [d]).
neighb_ex4(c, [e]).
neighb_ex4(d, [a]).
neighb_ex4(e, [a]).

path(X, X, [X]).
path(X, Y, [X | Path]) :-
    neighb_ex4(X, Neighbours),
    member(Z, Neighbours),
    path(Z, Y, Path).

cycle_neighb(X, R) :-
    path(X, X, [X | Path]),
    append(Path, [X], R),
    length(R, Len),
    Len > 3. 



edge_to_neighb :-
    retractall(gen_neighb(_, _)),
    findall(Node, (edge_ex5(Node, _); edge_ex5(_, Node)), Nodes),
    list_to_set(Nodes, UniqueNodes),
    maplist(process_node, UniqueNodes).

process_node(Node) :-
    findall(Neighbor, (edge_ex5(Node, Neighbor); edge_ex5(Neighbor, Node)), Neighbors),
    assertz(gen_neighb(Node, Neighbors)).
