/**
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 * 
 * This queries will try to answer the questions asked in the prolog assignment.
 */

:- consult('solution1.pl').
:- consult('solution2.pl').

/**
 * Answers question 1.1:
 * What paths are there from 1 to 3, from 3 to 5 and from 5 to 4?
 */
assignment1_1() :-
    writeln("What paths are there from 1 to 3, from 3 to 5 and from 5 to 4?"),
    forall(path(1, 3, Path), writeln("1 to 3" : Path)),
    forall(path(3, 5, Path), writeln("3 to 5" : Path)),
    forall(path(5, 4, Path), writeln("5 to 4" : Path)),
    nl.

/**
 * Answers question 2.1:
 * What is the cost of each path from 5 to 4 to walk it?
 */
assignment2_1() :-
    writeln("What is the cost of each path from 5 to 4 to walk it?"),
    findall(Path, path(5, 4, Path), Paths),
    maplist(cost, Paths, Costs),
    writeln("Costs": Costs),
    nl.

/**
 * Answers question 2.2:
 * What are the shortest paths from 1 to 3, from 3 to 5 and from 5 to 4?
 */
assignment2_2() :-
    writeln("What are the shortest paths from 1 to 3, from 3 to 5 and from 5 to 4?"),
    forall(shortestPath(1, 3, Path), writeln("1 to 3" : Path)),
    forall(shortestPath(3, 5, Path), writeln("3 to 5" : Path)),
    forall(shortestPath(5, 4, Path), writeln("5 to 4" : Path)),
    nl.

:- assignment1_1.
:- assignment2_1.
:- assignment2_2.
