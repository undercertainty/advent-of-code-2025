
%%% Interesting problem this one: OK to solve with 
%%% recursion, but need some lookahead in the 
%%% recursive cases. Which needed being on the ball
%%% to make sure that the cases weren't missed.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(library(apply)).


aoc_07_test_part1(Out):-
	solve_aoc_07_part1("test_data/data_07_2025_test.txt", Out).

aoc_07_part1(Out):-
	solve_aoc_07_part1("data/data_07_2025.txt", Out).

solve_aoc_07_part1(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "\n", "\t\n\s", Strings),
	maplist(string_chars, Strings, [MF|Manifold]),
		initial_state(MF, InitialState),
	count_splits(InitialState, Manifold, [], Out).


count_splits(_, [], SplitsList, Out):-
	sum_list(SplitsList, Out).
	
count_splits(State, [Splitters|Manifold], SoFar, Out):-
	next_state(State, Splitters, NextState, Splits),
	count_splits(NextState, Manifold, [Splits|SoFar], Out).



% Completely unnecessary conversion of 'S' to '|' to look
% a bit more like the problem spec.

initial_state(ManifoldTop, InitialState):-
	append(M1, ['S'|M2], ManifoldTop),
	append(M1, ['|'|M2], InitialState).

% A quick check suggests that there are no adjacent splitters.
% So...

next_state([Z], ['.'], [Z], 0).

next_state([Y, Z], ['.', '.'], [Y, Z], 0).
next_state(['|', _Z], ['^', '.'], ['.', '|'], 1).
next_state(['.', Z], ['^', '.'], ['.', Z], 0).

next_state([X, Y, Z|RestState], ['.', '.', '.'|RestMan], [X|Rest], Splits):-
	next_state([Y, Z|RestState], ['.', '.'|RestMan], Rest, Splits).

next_state([_, '|', _|RestState], ['.', '^', '.'|RestMan], ['|', '.'|Rest], Splits1):-
	next_state(['|'|RestState], ['.'|RestMan], Rest, Splits),
	succ(Splits, Splits1).
next_state([X, '.', Z|RestState], ['.', '^', '.'|RestMan], [X, '.'|Rest], Splits):-
	next_state([Z|RestState], ['.'|RestMan], Rest, Splits).

next_state([X, Y, '|'|RestState], ['.', '.', '^'|RestMan], [X|Rest], Splits):-
	next_state([Y, '|'|RestState], ['.', '^'|RestMan], Rest, Splits).

next_state([X, Y, '.'|RestState], ['.', '.', '^'|RestMan], [X|Rest], Splits):-
	next_state([Y, '.'|RestState], ['.', '^'|RestMan], Rest, Splits).


%%%%%%%%%%%%%%%%
%
% Part 2

% Let's construct the actual final state. Then we can work out
% from that how many pathways there are to each point.
%
% It would have been easier to do part one from this, as well...

aoc_07_test_part2(Out):-
	solve_aoc_07_part2("test_data/data_07_2025_test.txt", Out).

aoc_07_part2(Out):-
	solve_aoc_07_part2("data/data_07_2025.txt", Out).

solve_aoc_07_part2(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "\n", "\t\n\s", Strings),
	maplist(string_chars, Strings, [MF|Manifold]),
	initial_state(MF, InitialState),
	full_state(InitialState, Manifold,[], FullState),
	get_paths(FullState, PathCounts),
	sum_list(PathCounts, Out).


full_state(_, [], ROut, Out):-
	reverse(ROut, Out).

full_state(State, [Splitters|Manifold], SoFar, Out):-
	next_state2(State, Splitters, NextState, MapRow),
	full_state(NextState, Manifold, [MapRow|SoFar], Out).

get_paths([M1|MapIn], Out):-
	get_initial_counts(M1, InitialCounts),
	get_paths(InitialCounts, MapIn, Out).

get_paths(Out, [], Out).

get_paths(Nums, [MapLine|Map], Out):-
	sum_paths(Nums, MapLine, NextNums),
	get_paths(NextNums, Map, Out).


get_initial_counts([], []).
get_initial_counts(['.'|Next], [0|Rest]):-
	get_initial_counts(Next, Rest).
get_initial_counts(['|'|Next], [1|Rest]):-
	get_initial_counts(Next, Rest).



sum_paths([], [], []).

sum_paths([N], ['.'], [N]).
sum_paths([N], ['|'], [N]).


sum_paths([N1, N2|Counts], ['.', '.'|Map], [N1|Rest]):-
	sum_paths([N2|Counts], ['.'|Map], Rest).

sum_paths([N1, N2|Counts], ['.', '|'|Map], [N1|Rest]):-
	sum_paths([N2|Counts], ['|'|Map], Rest).


sum_paths([N1, N2|Counts], ['^', M|Map], [0, NN|Rest]):-
	sum_paths([N2|Counts], [M|Map], [R|Rest]),
	NN is R+N1.

sum_paths([N1, N2|Counts], ['|', '|'|Map], [N1|Rest]):-
	sum_paths([N2|Counts], ['|'|Map], Rest).

sum_paths([N1, N2|Counts], ['|', '.'|Map], [N1|Rest]):-
	sum_paths([N2|Counts], ['.'|Map], Rest).

sum_paths([N1, N2|Counts], ['|', '^'|Map], [NN|Rest]):-
	sum_paths([N2|Counts], ['^'|Map], Rest),
	NN is N1 + N2.



% So adjust the next_state predicate to include the pattern
% of the final row.
%
% Realising it would have been quicker just to build this
% structure in the first place...

next_state2([Z], ['.'], [Z], [Z]).

next_state2([Y, Z], ['.', '.'], [Y, Z], [Y, Z]).
next_state2(['|', _Z], ['^', '.'], ['.', '|'], ['^', '|']).
next_state2(['.', Z], ['^', '.'], ['.', Z], ['^', Z]).

next_state2([X, Y, Z|RestState], ['.', '.', '.'|RestMan], [X|Rest], [X|RestMap]):-
	next_state2([Y, Z|RestState], ['.', '.'|RestMan], Rest, RestMap).

next_state2([_X, '|', _Z|RestState], ['.', '^', '.'|RestMan], ['|', '.'|Rest], ['|', '^'|RestMap]):-
	next_state2(['|'|RestState], ['.'|RestMan], Rest, RestMap).


next_state2([X, '.', Z|RestState], ['.', '^', '.'|RestMan], [X, '.'|Rest], [X, '^'|RestMap]):-
	next_state2([Z|RestState], ['.'|RestMan], Rest, RestMap).

next_state2([X, Y, '|'|RestState], ['.', '.', '^'|RestMan], [X|Rest], [X|RestMap]):-
	next_state2([Y, '|'|RestState], ['.', '^'|RestMan], Rest, RestMap).

next_state2([X, Y, '.'|RestState], ['.', '.', '^'|RestMan], [X|Rest], [X|RestMap]):-
	next_state2([Y, '.'|RestState], ['.', '^'|RestMan], Rest, RestMap).

