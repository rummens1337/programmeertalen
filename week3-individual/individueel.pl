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
    append(ReversedRest, [H], Reversed).

%! ASSIGNMENT 3 -------- Sudoku
sudoku9(Rows, Solution) :-
        sudoku(Rows), !,
        maplist(portray_clause, Rows),!.

sudoku(Rows) :-
        Rows = [[A1,B1,C1,D1,E1,F1,G1,H1,I1],
                [A2,B2,C2,D2,E2,F2,G2,H2,I2],
                [A3,B3,C3,D3,E3,F3,G3,H3,I3],
                [A4,B4,C4,D4,E4,F4,G4,H4,I4],
                [A5,B5,C5,D5,E5,F5,G5,H5,I5],
                [A6,B6,C6,D6,E6,F6,G6,H6,I6],
                [A7,B7,C7,D7,E7,F7,G7,H7,I7],
                [A8,B8,C8,D8,E8,F8,G8,H8,I8],
                [A9,B9,C9,D9,E9,F9,G9,H9,I9]],

        Columns = [[A1,A2,A3,A4,A5,A6,A7,A8,A9],
                [B1,B2,B3,B4,B5,B6,B7,B8,B9],
                [C1,C2,C3,C4,C5,C6,C7,C8,C9],
                [D1,D2,D3,D4,D5,D6,D7,D8,D9],
                [E1,E2,E3,E4,E5,E6,E7,E8,E9],
                [F1,F2,F3,F4,F5,F6,F7,F8,F9],
                [G1,G2,G3,G4,G5,G6,G7,G8,G9],
                [H1,H2,H3,H4,H5,H6,H7,H8,H9],
                [I1,I2,I3,I4,I5,I6,I7,I8,I9]],

        Blocks = [[A1,A2,A3,B1,B2,B3,C1,C2,C3],
                [D1,D2,D3,E1,E2,E3,F1,F2,F3],
                [G1,G2,G3,H1,H2,H3,I1,I2,I3],
                [A4,A5,A6,B4,B5,B6,C4,C5,C6],
                [D4,D5,D6,E4,E5,E6,F4,F5,F6],
                [G4,G5,G6,H4,H5,H6,I4,I5,I6],
                [A7,A8,A9,B7,B8,B9,C7,C8,C9],
                [D7,D8,D9,E7,E8,E9,F7,F8,F9],
                [G7,G8,G9,H7,H8,H9,I7,I8,I9]],

         maplist(permutation([1,2,3,4,5,6,7,8,9]), Rows),
         maplist(permutation([1,2,3,4,5,6,7,8,9]), Columns),
         maplist(permutation([1,2,3,4,5,6,7,8,9]), Blocks).

%        length(Rows, 9), !,
%        maplist(same_length    (Rows2), Rows2), !,
%        maplist(inrange, Rows2),
%        append(Rows, Vs),
%        inrange(Vs).
%        Sudoku = [Rows,Columns,Blocks],
%        maplist(permutation([1,2,3,4,5,6,7,8,9]), Sudoku),
%        transpose(Rows, Columns),
%        maplist(permutation([1,2,3,4,5,6,7,8,9]), Columns).
%        Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
%                 blocks(As, Bs, Cs),
%                        blocks(Ds, Es, Fs),
%                        blocks(Gs, Hs, Is).

%% blocks([1,2,3],[4,5,6],[7,8,9]). -> yields True
%blocks([], [], []).
%blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :- % First 3 elements of each block and check whether its distinct.
%        permutation([1,2,3,4,5,6,7,8,9],[N1,N2,N3,N4,N5,N6,N7,N8,N9]), !,
%        blocks(Ns1, Ns2, Ns3), !.

% Loop over each element from the sudoku and check whether it's in range of [1..9]
% inrange([1,2,3,5,6]). -> Yields true
inrange([]).
inrange([H|T]) :- member(H, [1,2,3,4,5,6,7,8,9]), inrange(T).

is_permutation(Xs, Ys) :-
  msort(Xs, Sorted),
  msort(Ys, Sorted).

all_distinct(X) :-
    sort(X, Sorted), !, % filters duplicate entries, sorts them
    length(X, OriginalLength), % get length of original list
    length(Sorted, SortedLength), % get length of sorted lists
    OriginalLength == SortedLength. % if both length's are equal, return true

% transpose([[1,2,3],[4,5,6],[6,7,8]], X).
% Yields -> X = [[1, 4, 6], [2, 5, 7], [3, 6, 8]].
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts), !.

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),!,
        transpose(Rs, Ms1, Tss), !.

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
    lists_firsts_rests(Rest, Fs, Oss), !.

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