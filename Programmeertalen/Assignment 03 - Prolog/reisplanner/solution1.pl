:- consult('graph.pl').

connected(X,Y) :- edge(X,Y) ; edge(Y,X).

path(From, To, Path) :-
    travel(From, To, [], Path).

travel(X, Y, Visited, Path) :-
    X==Y,
    reverse(Visited, Path),
    !.
travel(From, To, Visited, Path) :-
    edge(From, X, Cost),
    From\==To,
    \+ member(edge(_, X, _), Visited),
    travel(X,
           To,
           [edge(From, X, Cost)|Visited],
           Path).