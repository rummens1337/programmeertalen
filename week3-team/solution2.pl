% Namen: Thomas Vos, Michel Rummens
% Studentnummers: 12829501, 13108093
% In dit bestand staan predikaten die de kosten van een pad kunnen berekenen,
% en een predikaat dat het kortste pad tussen twee knopen kan bepalen.

:- consult('graph.pl').
:- consult('solution1.pl').

% "cost" berekent de kosten van een pad, door steeds de kost van een edge op te
% tellen bij het totaal met behulp van recursie.

cost([], 0).
cost([edge(_, _, C)|RestOfPath], Cost) :-
    cost(RestOfPath, NewCost),
    Cost is C+NewCost.

%"shortestPath" (niet helemaal idiomatisch overigens maar zo moest het wel heten
% in de opdracht) berekent het kortste pad uit een lijst met paden, door met een
% findall alle paden + bijbehorende kosten in de vorm van tuples in een lijst te
% zetten, die vervolgens te sorteren van weinig kosten naar veel kosten en
% van die gesorteerde lijst de eerste (head) te pakken.

shortestPath(F, T, Path) :-
    findall((Newpath, C),
            ( path(F, T, Newpath),
              cost(Newpath, C)
            ),
            Paths),
    sort(Paths, [(Shortest, _)|_]),
    Path=Shortest.

% Onderstaande predikaten zouden in eerste instantie de tuples van Path en Cost
% Moeten vormen, maar werkt niet dus hebben we het anders moeten doen. Als
% diegene die dit nakijkt een oplossing weet om deze predikaten werkend
% te krijgen, graag!

% addCostBetween(Paths, PathsPlusCosts),

% addCostBetween(Paths, PathsPlusCosts) :-
%     addCost(Paths, [], PathsPlusCosts).

% addCost([],[]).
% addCost([H | T], List, PathsPlusCosts) :-
%        cost(H,Cost),
%        append([(H,Cost)], List, NewList),
%        addCost(T, NewList, PathsPlusCosts).
