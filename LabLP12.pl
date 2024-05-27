% gcd(X, Y, G) :- G is the greatest common divisor of X and Y.
gcd(X, 0, X) :- X > 0, !.
gcd(X, Y, G) :- Y > 0,
                Z is X mod Y,
                gcd(Y, Z, G).
