/**
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 *
 * This predicates will calculate the cost of a route 
 * and finds the shortest path between two nodes.
 */

:- consult('graph.pl').
:- consult('solution1.pl').

/**
 * Calculates the total cost of a given route.
 */
cost([], 0).
cost([Head | Tail], Cost) :-
    Head = edge(_, _, HeadCost),
    cost(Tail, TailCost),
    Cost is HeadCost + TailCost.

/**
 * Searches for the shortest path between nodes
 */
shortestPath(From, To, Path) :-
    findall((Cost, NPath),
            (path(From, To, NPath), cost(NPath, Cost)), Paths),
    sort(Paths, [(_, Answer) | _]),
    Path = Answer.
