:- consult('graph.pl').
:- consult('solution1.pl').

cost([], 0).
cost([Head | Tail], Cost) :-
    Head = edge(_, _, HeadCost),
    cost(Tail, TailCost),
    Cost is HeadCost + TailCost.

shortestPath(From, To, Path) :-
    findall((Cost, NewPath),
            (path(From, To, NewPath), cost(NewPath, Cost)), Paths),
    sort(Paths, [(_, Result) | _]),
    Path = Result.