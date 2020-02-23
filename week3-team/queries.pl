/**
 * Namen: Thomas Vos, Michel Rummens
 * Studentnummers: 12829501, 13108093
 * In dit bestand staan queries die de desbetreffende predicaten aanroepen.
 * Om 'redefined static procedure' te voorkomen is solution 3 uitgecomment.
 * Om deze te testen dient solution 1, 2 en diens queries gecomment te worden.
 */
:- ensure_loaded(solution1).
:- ensure_loaded(solution2).
% :- ensure_loaded(solution3).

% Voert het predicaat path/3 uit en print de output.
query1() :-
    writeln("________ query1 ________"),
    forall(path(1, 5, Path), writeln("1..5 "=Path)),
    nl,
    forall(path(3, 5, Path), writeln("3..5 "=Path)),
    nl,
    forall(path(5, 4, Path), writeln("5..4 "=Path)),
    nl.

% Voert het predicaat path/3 en cost/2 uit en print de output.
query2() :-
    writeln("________ query2 ________"),
    path(1, 5, Path),
    writeln("Path "=Path),
    cost(Path, Cost),
    writeln("Cost "=Cost),
    nl.

% Voert het predicaat shortestPath/3 uit en print de output.
query3() :-
    writeln("________ query3 ________"),
    forall(shortestPath(1, 3, Path),
           writeln("Shortest path 1..3 "=Path)),
    forall(shortestPath(3, 5, Path),
           writeln("Shortest path 3..5 "=Path)),
    forall(shortestPath(5, 4, Path),
           writeln("Shortest path 5..4 "=Path)),
    nl.

% voert het predicaat travel/3 uit en print de output.
% query4() :-
%     writeln("________ query4 ________"),
%     travel("Amsterdam Amstel"at 11:15, "Utrecht Centraal"at 11:38, Minutes),
%     writeln("Minutes travelling "=Minutes),
%     nl.


% Can be commented if they are to be called manually.
:- query1.
:- query2.
:- query3. 
% :- query4.
