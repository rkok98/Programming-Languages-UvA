/**
 * Author:   René Kok (13671146)
 * Study:    Doorstroomminor Software Engineering UvA
 *
 * This predicate calculates the time difference between two times.
 */

:- consult('graph.pl').

/**
 * Calculates the time difference between two times
 * and returns the difference in minutes.
 */
diffTime(H1:M1, H0:M0, Minutes) :-
    Minutes is (H1 - H0) * 60 + (M1 - M0).
