/**
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 * 
 * This file contains predicates who searches for paths between two nodes.
 */

:- consult('graph.pl').

% Executes the travel predicate
path(X, Y, Path) :-
    travel(X, Y, [], Path).

% Find a path between two nodes
travel(From, To, Visited, Path) :-
    edge(From, X, Cost),

    \+ member(edge(_, X, _), Visited),
    \+ member(edge(From, _, _), Visited),
    VisitedNodes = [edge(From, X, Cost) | Visited],

    (X = To, reverse(VisitedNodes, Path); 
     X \= To, travel(X, To, VisitedNodes, Path)).