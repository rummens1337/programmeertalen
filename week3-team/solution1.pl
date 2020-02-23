/**
 * Namen: Thomas Vos, Michel Rummens
 * Studentnummers: 12829501, 13108093
 * In dit bestand staan predikaten die een pad tussen twee knopen berekenen.
 */
:- ensure_loaded('graph.pl').

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
    edge(From, X, Cost),
    From\==To,
    \+ member(edge(_, X, _), Visited),
    travel(X,
           To,
           [edge(From, X, Cost)|Visited],
           Path).
