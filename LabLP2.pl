% Variant 1
gcd(X,X,X). % The third parameter is the result of GCD
gcd(X,Y,Z) :- X>Y, Diff is X-Y, gcd(Diff,Y,Z).
gcd(X,Y,Z) :- X<Y, Diff is Y-X, gcd(X,Diff,Z).


% Variant 2
gcd2(X,0,X). % The third parameter is the result of GCD
gcd2(X,Y,Z) :- Y>0, Rest is X mod Y, gcd2(Y,Rest,Z).


fact_bwd(0,1).
fact_bwd(N,F) :- N > 0, N1 is N-1, fact_bwd(N1,F1), F is N*F1.
fact_bwd(N,F) :- N1 is N-1, fact_bwd(N1,F1), F is N*F1.


fact_fwd(0,Acc,F) :- F = Acc.
fact_fwd(N,Acc,F) :- N > 0, N1 is N-1, Acc1 is Acc*N, fact_fwd(N1,Acc1,F).
fact_fwd(N,F) :- fact_fwd(N,1,F). % the accumulator is initialized with 1

for(Inter,Inter,0).
for(Inter,Out,In):-
 I>0,
 NewIn is In-1, <do_something_to_Inter_to_get_Intermediate>
 for(Intermediate,Out,NewIn).
 
 

lcm(X,Y,Z) :- gcd2(X,Y,GCD),
			P is X * Y, 
			Z is P div GCD.
			
			

		
power_bwd(X, 1, X).
power_bwd(X,Y,Z) :-
	Y>0,
	Ynew is Y-1,
	power_bwd(X, Ynew, Znew),
	Z is Znew * X.
	


power_fwd(X, 0, Z, Z).
power_fwd(X, Y, Z, Acc) :- Y>0, Ynew is Y-1, Accnew is Acc * X, power_fwd( X, Ynew, Z, Accnew). 


fib(0, 0).
fib(1, 1).
fib(X, Y):- X > 1, Xnew is X-1, Xnew2 is X-2, fib(Xnew, Ynew), fib(Xnew2, Ynew2), Y is Ynew + Ynew2.

fib2(1, 0, 1).
fib2(1, 1, 1).
fib2(X, A, Y):- X > 1, Xnew is X-1, fib2(Xnew, Last2, Last1), Y is Last2 + Last1, A is Last1.
fib2(X,Y):-fib2(X,_,Y).


triangle(A, B, C) :-
    A + B > C, 
    A + C > B, 
    B + C > A. 
	
solve(A, B, C, X):-
	Delta is B*B - 4*A*C,
	Down is 2*A,
	Up1 is -B - sqrt(Delta),