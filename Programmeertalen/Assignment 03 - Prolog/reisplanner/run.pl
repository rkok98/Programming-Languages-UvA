:- initialization main.

main :-
    load_test_files(make(all)),
    run_tests,
    halt.

main :-
    halt(1).

path(From, To, Path) :-
    travel(From, To, [A], Q),
    reverse(Q, Path)

travel(From, To, [B|P]) :-
    connected