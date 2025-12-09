
%%% Day 8

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(library(apply)).


aoc_08_test_part1(Out):-
	solve_aoc_08_part1("test_data/data_08_2025_test.txt", 10, Out).

aoc_08_part1(Out):-
	solve_aoc_08_part1("data/data_08_2025.txt", 1000, Out).

solve_aoc_08_part1(FileName, NumClosest, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(boxes(Boxes), FileCodes),
	distances(Boxes, BoxDists),
	keysort(BoxDists, SortedBoxDists),
	take(NumClosest, SortedBoxDists, ClosestPairs),
	pairs_values(ClosestPairs, Closest),
	group_connected(Closest, Circuits),
	maplist(length, Circuits, CircuitLengths),
	sort(0, @>=, CircuitLengths, [C1, C2, C3|_SortedCircuitLengths]),
	Out is C1 * C2 * C3.


% weirdly, don't seem to be recognising take/3.
% No matter...

take(N, ListIn, ListOut):-
	length(ListOut, N),
	append(ListOut, _, ListIn).


boxes([]) --> eol.
boxes([Box|Boxes]) --> box(Box), eol, boxes(Boxes).

box([X, Y, Z]) --> integer(X), ",", integer(Y), ",", integer(Z).

distances([_], []).
distances([Next|Rest], Distances):-
	maplist({Next}/[Y, Dist-[Next, Y]] >> distance(Next, Y, Dist), Rest, DistsNext),
	distances(Rest, DistsRest),
	append(DistsNext, DistsRest, Distances).

% don't root: can keep as ints
distance([X1, Y1, Z1], [X2, Y2, Z2], Distance):-
	Distance is (X2-X1)**2 + (Y2-Y1)**2 + (Z2-Z1)**2.


% Take set of pairs. Return set of sets of connected pairs

group_connected(PairsIn, Out):-
	initial_sets(PairsIn, InitialSets),
	combine_sets(PairsIn, InitialSets, Out).


initial_sets([], []).
initial_sets([[X, Y]|Rest], SetOut):-
	initial_sets(Rest, SetRest),
	list_to_set([[X], [Y]|SetRest], SetOut).


combine_sets([], Out, Out).
combine_sets([[X, Y]|Rest], Sets, Out):-
	member(Set, Sets),
	member(X, Set),
	member(Y, Set),
	combine_sets(Rest, Sets, Out).
combine_sets([[X, Y]|Rest], Sets, Out):-
	select(SetX, Sets, Sets1),
	member(X, SetX),
	\+ member(Y, SetX),
	select(SetY, Sets1, SetsRest),
	member(Y, SetY),
	append(SetX, SetY, SetXY),
	combine_sets(Rest, [SetXY|SetsRest], Out).



%%%%%%%%%%%%%%%%
%
% Part 2

% Slow but tractable


aoc_08_test_part2(Out):-
	solve_aoc_08_part2("test_data/data_08_2025_test.txt", 10, Out).

aoc_08_part2(Out):-
	solve_aoc_08_part2("data/data_08_2025.txt", 1000, Out).

solve_aoc_08_part2(FileName, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(boxes(Boxes), FileCodes),
	distances(Boxes, BoxDists),
	keysort(BoxDists, SortedBoxDists),
	pairs_values(SortedBoxDists, Closest),
	final_connection(Closest, Connections),
	last(Connections, [[X1, _Y1, _Z1], [X2, _Y2, _Z2]]),
	Out is X1 * X2.


t(X) :- solve_aoc_08_part2("test_data/data_08_2025_test.txt", X).
t1(X) :- solve_aoc_08_part2("data/data_08_2025.txt", X).


final_connection(PairsIn, Out):-
	initial_sets(PairsIn, InitialSets),
	combine_sets2(PairsIn, InitialSets, Out).

combine_sets2([[X, Y]|Rest], Sets, [[X, Y]|Out]):-
	member(Set, Sets),
	member(X, Set),
	member(Y, Set),
	combine_sets2(Rest, Sets, Out).

combine_sets2([[X, Y]|Rest], Sets, [[X, Y]|Out]):-
	select(SetX, Sets, Sets1),
	member(X, SetX),
	\+ member(Y, SetX),
	select(SetY, Sets1, [Set|SetsRest]),
	member(Y, SetY),
	append(SetX, SetY, SetXY),
	combine_sets2(Rest, [SetXY,Set|SetsRest], Out).

% Final cases where we end up with a single network

combine_sets2([[X, Y]|_Rest], [SetX, SetY], [[X, Y]]):-
	member(X, SetX),
	member(Y, SetY).
combine_sets2([[X, Y]|_Rest], [SetX, SetY], [[X, Y]]):-
	member(X, SetY),
	member(Y, SetX).