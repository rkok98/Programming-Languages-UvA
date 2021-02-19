:- consult('graph.pl').

diffTime(H1:M1, H0:M0, Minutes) :-
    Minutes is (H1 - H0) * 60 + (M1 - M0).

cost([], 0).
cost([H | T], Cost) :-
    H = travel(_, _, Cost1),
    cost(T, Cost2),
    Cost is Cost1 + Cost2.

shortestPath(From, To, Path) :-
    findall((Cost, NewPath),
            (path(From, To, NewPath), cost(NewPath, Cost)), Paths),
    sort(Paths, [(_, Result) | _]),

    
    Path = Result.