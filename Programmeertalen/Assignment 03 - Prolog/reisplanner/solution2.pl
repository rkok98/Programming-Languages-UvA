:- consult('graph.pl').
:- consult('solution1.pl').

cost([], 0).
cost([Head | Tail], Cost) :-
    Head = edge(_, _, HeadCost),
    cost(Tail, TailCost),
    Cost is HeadCost + TailCost.

shortestPath(From, To, Path) :-
    findall((Cost, NPath),
            (path(From, To, NPath), cost(NPath, Cost)), Paths),
    sort(Paths, [(_, Answer) | _]),
    Path = Answer.