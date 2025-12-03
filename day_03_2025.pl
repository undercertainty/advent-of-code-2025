
%%% Day 3

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).


aoc_test_part1(Out):-
	solve_aoc_part1("test_data/data_03_2025_test.txt", Out).

aoc_part1(Out):-
	solve_aoc_part1("data/data_03_2025.txt", Out).

solve_aoc_part1(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\s\t\n", [StrippedFileString]),
	split_string(StrippedFileString, "\n", "", Banks),
	maplist(maximum_joltage_1, Banks, MaxJoltages),
	sum_list(MaxJoltages, Out).

maximum_joltage_1(NumStr, MaxJoltage):-
	string_codes(NumStr, Codes),
	findall(Joltage,
		(subseq(Codes, [X, Y], _),
			number_codes(Joltage, [X, Y])),
		Joltages),
	max_member(MaxJoltage, Joltages).
	

% Obviously more efficient ways of doing this, but
% I'm after an easy solution, not a quick one. Yet.

%%%
%%% I assume it's going to take ages if I just plug 12 in...
%%%

aoc_test_part2(Out):-
	solve_aoc_part2("test_data/data_03_2025_test.txt", Out).

aoc_part2(Out):-
	solve_aoc_part2("data/data_03_2025.txt", Out).


% Just plugging 12 in as a value blows the stack, so let's do the
% damn thing properly. I'll set it for arbitrary length, then I
% can retest on the 2-cases.

solve_aoc_part2(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\s\t\n", [StrippedFileString]),
	split_string(StrippedFileString, "\n", "", Banks),
	maplist([B, M] >> maximum_joltage_2(B, 12, M),
		Banks,
		MaxJoltages),
	sum_list(MaxJoltages, Out).


maximum_joltage_2(NumStr, N, MaxJoltage):-
	string_codes(NumStr, NumCodes),
	maximum_joltage_2_(NumCodes, N, MaxJoltageCodes),
	number_codes(MaxJoltage, MaxJoltageCodes).

maximum_joltage_2_(_, 0, []).

maximum_joltage_2_(CodesIn, L1, [Max|CodesOut]):-
	high(CodesIn, L1, Max, Rest),
	succ(L, L1),
	maximum_joltage_2_(Rest, L, CodesOut).

% high(+CodesIn, +Len, -Max, -Rest)
%
% Splits CodesIn at Max, the first occurrence of the highest
% value of CodesIn, ensuring that there are at least Len
% characters left. Rest are the members to the right of Max.

high(CodesIn, Len, Max, Rest):-
	append(Codes, TailCodes, CodesIn),
	length([_|TailCodes], Len),
	append(L1, [Max|R], Codes),
	max_list([0|L1], M),
	Max > M,
	max_list([Max|R], Max),
	append(R, TailCodes, Rest).
