% Naam: Michel Rummens
% Studentnummer: 13108093
% Final assignment is to solve a sudoku :)

%! ASSIGNMENT 1 -------- Append
% START = append3([a,b], [c,e], Result=[a,b,c,e]) ==> False
% H1=a |T1=[b]], [c,e], [H1=a | T3=[b,c,e]
% -> append3([b], [c,e], [b,c,e]) ==> False
% H1=b | T1=[]], [c,e], [H1=b | T3=[c,e]
% -> append3([], [c,e], [c,e]) ==> True
append([], List, List).
append([H|T], List, [H|Result]) :- append(T, List, Result).



%! ASSIGNEMNT 2 -------- Palindrome
palindroom(List) :- list_reverse(List,List).

% START = list_reverse([a,b,a], [a,b,a]). ==> False
% H=a, T=[b,a]], [a,b,a]
% -> list_reverse([b, a], [??]), append3([??], H=[a], [a,b,a]
list_reverse([], []).
list_reverse([H|T], Reversed) :-
    list_reverse(T, ReversedRest),
    append3(ReversedRest, [H], Reversed).



% sudoku9([ [5,3,4,6,7,8,9,1,_] , [6,7,2,1,9,5,3,4,8] , [1,9,_,3,4,2,5,6,7] , [8,5,9,7,6,_,4,2,3] , [4,2,6,8,5,3,7,9,1] , [7,1,3,9,2,4,8,5,6] , [9,6,1,5,3,7,2,8,4] , [2,8,7,4,1,9,6,3,5] , [_,4,5,2,_,6,1,_,9]]).
%! ASSIGNMENT 3 -------- Sudoku
sudoku9(Rows, Solution) :-
        sudoku(Rows),
        maplist(portray_clause, Rows).

sudoku(Rows) :-
        length(Rows, 9),
        maplist(same_length(Rows), Rows),
        maplist(inrange, Rows),
%        append(Rows, Vs),
%        inrange(Vs),
        maplist(all_distinct, Rows),
        transpose(Rows, Columns),
        maplist(all_distinct, Columns),
        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
                blocks(As, Bs, Cs),
                blocks(Ds, Es, Fs),
                blocks(Gs, Hs, Is).

% blocks([1,2,3],[4,5,6],[7,8,9]). -> yields True
blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :- % First 3 elements of each block and check whether its distinct.
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

% Loop over each element from the sudoku and check whether it's in range of [1..9]
% inrange([1,2,3,5,6]). -> Yields true.
inrange([]).
inrange([H|T]) :- member(H, [1,2,3,4,5,6,7,8,9]), inrange(T).

all_distinct(X) :-
    sort(X, Sorted), % filters duplicate entries, sorts them
    length(X, OriginalLength), % get length of original list
    length(Sorted, SortedLength), % get length of sorted list
    OriginalLength == SortedLength. % if both length's are equal, return true

% transpose([[1,2,3],[4,5,6],[6,7,8]], X).
% Yields -> X = [[1, 4, 6], [2, 5, 7], [3, 6, 8]].
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

problem(1, [[_,_,_,_,_,_,_,_,_],
            [_,_,_,_,_,3,_,8,5],
            [_,_,1,_,2,_,_,_,_],
            [_,_,_,5,_,7,_,_,_],
            [_,_,4,_,_,_,1,_,_],
            [_,9,_,_,_,_,_,_,_],
            [5,_,_,_,_,_,_,7,3],
            [_,_,2,_,1,_,_,_,_],
            [_,_,_,_,4,_,_,_,9]]).


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