:- consult('graph.pl').

path(From, To, Path) :-
    travel(From, To, [], Path).

travel(From, To, Visited, Path) :-
    From == To,
    reverse(Visited, Path),
    !.

travel(From, To, Visited, Path) :-
    edge(From, X, Cost),
    From \== To,
    \+ member(edge(_, X, _), Visited),
    \+ member(edge(From, _, _), Visited),
    travel(X, To, [edge(From, X, Cost) | Visited], Path).