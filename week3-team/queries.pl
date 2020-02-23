:- ensure_loaded(solution1).
:- ensure_loaded(solution2).

query1() :-
    writeln("________ query1 ________"),
    forall(path(1, 5, Path), writeln("1..5 "=Path)),
    nl,
    forall(path(3, 5, Path), writeln("3..5 "=Path)),
    nl,
    forall(path(5, 4, Path), writeln("5..4 "=Path)),
    nl.

query2() :-
    writeln("________ query2 ________"),
    path(1, 5, Path),
    writeln("Path "=Path),
    cost(Path, Cost),
    writeln("Cost "=Cost),
    nl.

query3() :-
    writeln("________ query3 ________"),
    forall(shortestPath(1, 3, Path),
           writeln("Shortest path 1..3 "=Path)),
    forall(shortestPath(3, 5, Path),
           writeln("Shortest path 3..5 "=Path)),
    forall(shortestPath(5, 4, Path),
           writeln("Shortest path 5..4 "=Path)),
    nl.

% Can be commented if they are to be called manually.
:- query1.
:- query2.
:- query3. 
