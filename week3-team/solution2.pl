:- consult('graph.pl').
:- consult('solution1.pl').

cost([],0).
cost([edge(_,_,C) | RestOfPath], Cost) :-
       cost(RestOfPath, NewCost),
       Cost is C+NewCost.

shortestPath(F,T,Path) :-
       findall(Newpath, path(F,T,Newpath), List), % Berekent alle paden in 1 keer en zet ze in een list.
       addCostBetween(List),
       sort(List, [(Shortest,_)|_]),
       Path = Shortest.

    %    List = [H | _],
    %    Path = H.

addCostBetween(Paths) :-
    addCost(Paths, []).

addCost([],[]).
addCost([H | T], List) :-
       cost(H,Cost),
       append([(H,Cost)], List, NewList),
       addCost(T, NewList).

