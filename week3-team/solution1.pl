:- consult('graph.pl').

%  van https://www.cpp.edu/~jrfisher/www/prolog_tutorial/2_15A.pl
connected(X,Y,L) :-
    edge(X,Y,L).

%    F T Path
path(F,T,Path) :-
       travel(F,T,[],Visited,_),
       reverse(Visited,Path).

travel(A,B,P,[edge(A,B,L) |P ],L) :-
       connected(A,B,L).
travel(A,B,Visited,Path,L) :-
       connected(A,C,D),
       C \== B,
       \+member(edge(A,C,_),Visited),
       travel(C,B,[edge(A,C,D) | Visited],Path,L1),
       L is D+L1.