
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(library(apply)).


% Today is running very slowly, but I'm not concerned about efficiency,
% just getting the answer. findall/3 is always a good way of slowing 
% your program to a crawl, but I can just leave this running while
% I do some proper work.

aoc_04_test_part1(Out):-
	solve_aoc_04_part1("test_data/data_04_2025_test.txt", Out).

aoc_04_part1(Out):-
	solve_aoc_04_part1("data/data_04_2025.txt", Out).

solve_aoc_04_part1(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\t\n\s", [StrippedString]),
	string_codes(StrippedString, Codes),
	phrase(rows(_, Rolls),
		Codes),
	!,
	count_all_neighbours(Rolls, Rolls, NeighbourCounts),
	include([X] >> (X=_Y-Z, Z<4), NeighbourCounts, MovableRolls),
	length(MovableRolls, Out).


% Counterintuitive counting for the moment: [0,0]
% at the bottom *right*.

rows(0, []) --> [].

rows(Row1, Rolls) -->
	row(Row1, _, RowRolls),
	eol,
	rows(Row, RollsSoFar),
	{succ(Row, Row1),
	 append(RowRolls, RollsSoFar, Rolls)}.

row(_Row, 0, []) --> [].
row(Row, N1, Rolls) --> ".", row(Row, N, Rolls),
						{succ(N, N1)}.
row(Row, N1, [[Row, N1]|Rolls]) --> "@", row(Row, N, Rolls),
									{succ(N, N1)}.



neighbour([X1, Y1], [X2, Y2]):-
	abs(X1-X2) =< 1,
	abs(Y1-Y2) =< 1,
	\+ [X1, Y1]=[X2, Y2].

count_all_neighbours([], _, []).
count_all_neighbours([Next|Rest], Coords, [Next-NumNeighbours|Out]):-
	count_neighbours(Next, Coords, NumNeighbours),
	count_all_neighbours(Rest, Coords, Out).

count_neighbours(Coord, Coords, NumNeighbours):-
	findall(Neighbour,
		(member(Neighbour, Coords),
			neighbour(Coord, Neighbour)),
		Neighbours),
	length(Neighbours, NumNeighbours).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Part 2.

% I don't think this needs anything clever... just a repeated
% run while I go and have a bath. Again, very slow, but I've got
% more important things to do than solve this properly.

aoc_04_test_part2(Out):-
	solve_aoc_04_part2("test_data/data_04_2025_test.txt", Out).

aoc_04_part2(Out):-
	solve_aoc_04_part2("data/data_04_2025.txt", Out).

solve_aoc_04_part2(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\t\n\s", [StrippedString]),
	string_codes(StrippedString, Codes),
	phrase(rows(_, Rolls),
		Codes),
	!,
	part2(Rolls, [], NumRemoved),
	sum_list(NumRemoved, Out).


%	ugly disjunction for a negligible improvement in runtime

part2(Coords, SoFar, Out):-
	count_all_neighbours(Coords, Coords, NeighbourCounts),
	movable_rolls(NeighbourCounts, MovableRolls),
	(
		(MovableRolls = [], SoFar = Out)
	;
		(subtract(Coords, MovableRolls, UnmovedCoords),
		length(MovableRolls, NumMovableRolls),
		part2(UnmovedCoords, [NumMovableRolls|SoFar], Out))
	).

movable_rolls([], []).
movable_rolls([_Next-N|Rest], Out):-
	N >= 4,
	movable_rolls(Rest, Out).
movable_rolls([Next-N|Rest], [Next|Out]):-
	N < 4,
	movable_rolls(Rest, Out).

