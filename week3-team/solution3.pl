/**
 * Namen: Thomas Vos, Michel Rummens
 * Studentnummers: 12829501, 13108093
 * In dit bestand staan predikaten die de kosten van een pad kunnen berekenen,
 * en een predikaat dat het kortste pad tussen twee knopen kan bepalen.
 */
:- ensure_loaded('route.pl').
:- ensure_loaded('solution1.pl').
:- ensure_loaded('solution2.pl').

% Comments
diffTime(H1:M1, H0:M0, Minutes):-
    Minutes is ((H1 * 60)+ M1) - ((H0 * 60)+ M0) .

% Comments
travel(Start at Startime, End at Endtime, Cost):-
    route(Stations),
    nextto(Start at Startime, End at Endtime, Stations),
    diffTime(Startime, Endtime, Minutes),
    Cost is abs(Minutes).
