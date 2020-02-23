% :- consult(graph).

% path(From, To, Path) :-
%     path(From, To, [], Path).

% path(To, To, Visited,Path):-
%     reverse(Visited, Path),!.

% path(From, To, Visited, Path):-
%     edge(From, X, Cost),
%     not(member(edge(_, X, _), Visited)),
%     path(X, To, [edge(From, X, Cost)|Visited], Path).



% cost([],Cost):-
%     Cost = 0.
% cost([Head|Tail], Total):-
%     Head = edge(_,_,Cost),
%     cost(Tail,Rest),
%     Total is Cost + Rest.

% shortestPath(From, To, Path):-
%     findall((Cost,Check), (path(From, To, Check),cost(Check,Cost)), Bag),
%     sort(Bag, [(_,Result)|_]),
%     Path = Result.