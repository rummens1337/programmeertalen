:- consult('graph.pl').
:- consult('solution1.pl').

cost([],0).
cost([edge(_,_,C) | RestOfPath], Cost) :-
       cost(RestOfPath, NewCost),
       Cost is C+NewCost.

shortestPath(F,T,Path) :-
       findall((Newpath,C), (path(F,T,Newpath),cost(Newpath,C)), Paths),
       sort(Paths, [(Shortest,_)|_]),
       Path = Shortest.

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

