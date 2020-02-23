/**
 * Namen: Thomas Vos, Michel Rummens
 * Studentnummers: 12829501, 13108093
 * In dit bestand staan predikaten die de kosten van een pad kunnen berekenen,
 * en een predikaat dat het kortste pad tussen twee knopen kan bepalen.
 */
:- ensure_loaded('route.pl').
% :- ensure_loaded('solution1.pl').
% :- ensure_loaded('solution2.pl').
% 
%  We hebben geprobeerd bovenstaande loads te gebruiken, en de desbetreffende predicaten
% die anders waren te overriden, maar hebben daar geen mogelijkheid toe gevonden in prolog.

% "path" fungeert als een soort "stepping stone" hier en roept alleen "travel"
% aan met dezelfde argumenten + een lege lijst die uiteindelijk de lijst met
% al bezochte kanten wordt.
path(From, To, Path) :-
    travel(From, To, [], Path).

% "travel" zoekt een pad van het ene punt naar het andere, door steeds per knoop
% naar aanliggende edges te zoeken. Regels zijn dat men niet terug mag gaan naar
% een al eerder bezochte knoop en dat From niet gelijk mag zijn aan To. Zodra
% een edge is gevonden, gaat "travel" door met de uiterste knoop van die edge
% met behulp van recursie, enzovoort.
travel(X, Y, Visited, Path) :-
    X==Y,
    reverse(Visited, Path),
    !.
travel(From, To, Visited, Path) :-
    travel(From, X, Cost),
    From\==To,
    \+ member(travel(_, X, _), Visited),
    travel(X,
           To,
           [travel(From, X, Cost)|Visited],
           Path).


% "cost" berekent de kosten van een pad, door steeds de kost van een edge op te
% tellen bij het totaal met behulp van recursie.
cost([], 0).
cost([travel(_, _, C)|RestOfPath], Cost) :-
    cost(RestOfPath, NewCost),
    Cost is C+NewCost.

%"shortestPath" (idiomatiserwijze geschreven als "shortest_path") 
% berekent het kortste pad uit een lijst met paden, door met een
% findall alle paden + bijbehorende kosten in de vorm van tuples in een lijst te
% zetten, die vervolgens te sorteren van weinig kosten naar veel kosten en
% van die gesorteerde lijst de eerste (head) te pakken.
shortestPath(F, T, Path) :-
    findall((Cost, NewPath),
            ( path(F, T, NewPath),
              cost(NewPath, Cost)
            ),
            Paths),
    sort(Paths, [(_, Result)|_]),
    Path=Result.

% Comments
diffTime(H1:M1, H0:M0, Minutes):-
    Minutes is ((H1 * 60)+ M1) - ((H0 * 60)+ M0) .

% Comments
travel(Start at Startime, End at Endtime, Cost):-
    route(Stations),
    nextto(Start at Startime, End at Endtime, Stations),
    diffTime(Startime, Endtime, Minutes),
    Cost is abs(Minutes).
