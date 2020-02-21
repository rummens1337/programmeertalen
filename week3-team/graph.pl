% Namen: Thomas Vos, Michel Rummens
% Studentnummers: 12829501, 13108093
% Lovely assignment!

% F = From
% T = To
% C = Cost

%    F T C
edge(1,2,5).
edge(2,1,3).
edge(2,3,4).
edge(2,4,3).
edge(2,5,5).
edge(3,1,9).
edge(3,2,2).
edge(5,1,3).
edge(5,4,2).

path(From, To, Path) :-
    path(From, To, Visited, Path).

path(From, To, Visited, Path) :-
    R = [],
    Path = [],
    edge(To,Y,Z),
    append([Y], R, Visited),
    write(Visited).
    
















connected(X,Y,L) :- E = edge(X,Y,L).
connected(X,Y,L) :- edge(Y,X,L).

is_connected(E1, E2, L) :- edge(E1, E2, _).
is_connected(E1, E2, L) :- edge(E1, R, _) , is_connected(R, E2, L).


addition(X,Y,Z) :- Z is X+Y.
add(X,Y):-
   between(X,Y,A),
   addition(X,A,Z),
   writeln(addition(X,A,Z)),
   X1 is X+1,
   add(X1,Y).

%! FACTS
bigger(elephant, horse).
bigger(horse, donkey).
bigger(donkey, dog).
bigger(donkey, monkey).

% bigger(elephant,X). yields -> horse
% is_bigger(elephant,X). yields -> dog, monkey, donkey, horse
%
% (elephant,dog) ==> False
% (elephant, Z=horse) -> (Z=horse,dog) ==> False
% (horse, Z=donkey) -> (Z=donkey,dog) ==> True
%
% RULES
is_bigger(X, Y) :- bigger(X, Y).
is_bigger(X, Y) :- bigger(X, Z), is_bigger(Z, Y).