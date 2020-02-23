edge(1,2,5).
edge(2,1,3).
edge(2,3,4).
edge(2,4,3).
edge(2,5,5).
edge(3,1,9).
edge(3,2,2).
edge(5,1,3).
edge(5,4,2).

connected(F,T) :-
    edge(F,T,_).

path(F,T,Path) :-
    pathVisited(F,T,_,Path).

pathVisited(F,T,Visited,Path) :- % From, To, Visited, path

    travel(F,T,F,Visited),
    reverse(Visited,Path).

travel(_,T,T,_). % Base case: if current is the same as final node.
travel(F,T,C,Visited) :- % From, To, Current, Visited

    connected(C,_),
    append(Visited, [edge(C,X,_)], VisitedPlus), % append to visitedlist.
    \+member(edge(C,X,_), Visited), % checks if an edge was already in the list.
    travel(F,T,X,VisitedPlus). % recursive bois