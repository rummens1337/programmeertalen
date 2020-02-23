% :- consult('graph.pl').
% :- consult('solution1.pl').

% cost([],0).
% cost([edge(_,_,C) | RestOfPath], Cost) :-
%        cost(RestOfPath, NewCost),
%        Cost is C+NewCost.

% shortestPath(F,T,Path) :-
%        findall(path(F,T,Path), path(F,T,Path), List), % Berekent alle paden in 1 keer en zet ze in een list.
%        addCost(List,[]),
%        sort(List, Path).
%        %SortedList = [ Head | Tail],
%        %Path is Head.

% addCost([],_).
% addCost([H | T], List) :-
%        cost(H,Cost),
%        append([H], [Cost], Newlist),
%        addCost(T, Newlist).