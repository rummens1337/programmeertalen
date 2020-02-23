% :- consult(graph).

% path(From, To, Path) :-
%     path(From, To, [], Path).

% path(To, To, Visited,Path):-
%     reverse(Visited, Path),!.

% path(From, To, Visited, Path):-
%     edge(From, X, Cost),
%     not(member(edge(_, X, _), Visited)),
%     path(X, To, [edge(From, X, Cost)|Visited], Path).