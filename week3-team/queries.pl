:- ensure_loaded(solution1).
:- ensure_loaded(solution2).
:- consult(solution2).

query1() :-
    forall(path(1, 5, Path), writeln("1..5 " = Path)),
    nl,
    forall(path(3, 5, Path), writeln("3..5 " = Path)),
    nl,
    forall(path(5, 4, Path), writeln("5..4 " = Path)),
    nl.

query2() :-
    path(1, 5, Path),
    writeln("Path " = Path),
    cost(Path, Cost),
    writeln("Cost " = Cost).

% Can be commented if they are to be called manually.
:- query1.
:- query2.
