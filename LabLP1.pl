animal(dog).
animal(cat).
weight(dog,10).
weight(cat,5).

heavier(X,Y) :-
    weight(X,WX),
    weight(Y,WY),
    WX>WY.


% the on_route/1 fact
on_route(camin).
% the on_route/1 rule – this is a recursive rule
on_route(Place):-
    move(Place, Method, NewPlace),
    on_route(NewPlace).

% the move/3 facts
move(acasa, taxi, gara).
move(gara, tren, cluj).
move(cluj, bus, camin).


woman(ana).
woman(sara).
woman(ema).
woman(dorina).
woman(irina).
woman(maria). 
woman(carmen).

man(andrei).
man(george).
man(alex).
man(mihai).
man(sergiu).
man(marius).

parent(maria, ana). % maria is the parent of ana
parent(george, ana). % george is the parent of ana
parent(maria, andrei).
parent(george, andrei). 

parent(dorina,maria).
parent(marius,maria).

parent(mihai,george).
parent(irina,george).
parent(mihai,carmen).
parent(irina,carmen).

parent(carmen,ema).
parent(carmen,sara).
parent(alex,ema).
parent(alex,sara).

mother(X,Y) :- woman(X), parent(X,Y).

father(X,Y) :- man(X), parent(X,Y).

sibling(X,Y) :- parent(Z,X), parent(Z,Y), X\=Y.

sister(X,Y) :- sibling(X,Y), woman(X).

aunt(X,Y) :- sister(X,Z), parent(Z,Y).

brother(X,Y) :- sibling(X,Y), man(X).

uncle(X,Y) :- brother(X,Z), parent(Z,Y).

grandmother(X,Y) :- woman(X),parent(Z,Y),parent(X,Z).

grandfather(X,Y) :- man(X),parent(Z,Y),parent(X,Z).