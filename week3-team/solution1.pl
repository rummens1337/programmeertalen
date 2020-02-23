:- consult('graph.pl').

%    F T Path
% path(F,T,Path) :-
%        travel(F,T,[],Visited,_),
%        reverse(Visited,Path).

% travel(A,B,P,[edge(A,B,L) |P ],L) :-
%        edge(A,B,L).

% travel(A,B,Visited,Path,L) :-
%        edge(A,C,D),
%        C \== B,
%        not(member(edge(A,C,_),Visited)),
%        travel(C,B,[edge(A,C,D) | Visited],Path,L1),
%        L is D+L1.

path(From, To, Path) :-
    travel(From, To, [], Path).

travel(To, To, Visited, Path) :-
    reverse(Visited, Path),
    !.

travel(From, To, Visited, Path) :-
    edge(From, X, Cost),
    \+ member(edge(_, X, _), Visited),
    travel(X,
           To,
           [edge(From, X, Cost)|Visited],
           Path).