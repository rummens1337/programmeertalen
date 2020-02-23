:- consult(route).

diffTime(H1:M1, H0:M0, Minutes):-
    Minutes is ((H1 * 60)+ M1) - ((H0 * 60)+ M0) .

travel(Start at Startime, End at Endtime, Cost):-
    route(Stations),
    nextto(Start at Startime, End at Endtime, Stations),
    diffTime(Startime, Endtime, Minutes),
    Cost is abs(Minutes).

path(From, To, Path) :-
    path(From, To, [], Path).

path(To, To, Visited,Path):-
    reverse(Visited, Path),!.

path(From, To, Visited, Path):-
    travel(From, X, Cost),
    not(member(travel(_, X, _), Visited)),
    path(X, To, [travel(From, X, Cost)|Visited], Path).


cost([],Cost):-
    Cost = 0.
cost([Head|Tail], Total):-
    Head = travel(_,_,Cost),
    cost(Tail,Rest),
    Total is Cost + Rest.

shortestPath(From, To, Path):-
    findall((Cost,Check), (path(From, To, Check),cost(Check,Cost)), Bag),
    sort(Bag, [(_,Result)|_]),
    Path = Result.