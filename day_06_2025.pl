
%%% Bringing in the clpfd library for its helpful
%%% transpose predicate

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).


aoc_06_test_part1(Out):-
	solve_aoc_06_part1("test_data/data_06_2025_test.txt", Out).

aoc_06_part1(Out):-
	solve_aoc_06_part1("data/data_06_2025.txt", Out).

solve_aoc_06_part1(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\t\n\s", [StrippedString]),
	string_codes(StrippedString, Codes),
	!,
	phrase(homework(Operators, NumberCols),
		Codes),
	evaluate_columns(Operators, NumberCols, Totals),
	sum_list(Totals, Out).
	

evaluate_columns([], [], []).
evaluate_columns([Operator|Os], [Numbers|Lists], [N|Out]):-
	call(Operator, Numbers, N),
	evaluate_columns(Os, Lists, Out).


homework(Operators, NumbersT)-->
	number_rows(Numbers),
	operators(Operators),
	{
		transpose(Numbers, NumbersT)
		}.

number_rows([])-->
	[].
number_rows([NR|NRs])-->
	number_row(NR),
	"\n",
	number_rows(NRs).

number_row([])-->
	whites.
number_row([Num|Nums])-->
	whites,
	digits([NC|NumCodes]),
	number_row(Nums),
	{
		number_codes(Num, [NC|NumCodes])
		}.

operators([])-->
	whites,
	eol.
operators([plus_list|Ops])-->
	whites,
	"+",
	operators(Ops).
operators([times_list|Ops])-->
	whites,
	"*",
	operators(Ops).

% predicates to either sum or multiply members of a list

plus_list(NumsIn, Out):-
	foldl(plus, NumsIn, 0, Out).
times_list(NumsIn, Out):-
	foldl(times, NumsIn, 1, Out).

times(X, Y, Z) :-
	Z is X*Y.
	
%%% Part 2

% Hmmm. this looks like it might be a bit of a slog.
% Probably worth transposing the input first.
%
% Actually, it falls out fairly cleanly with the transpose.


aoc_06_test_part2(Out):-
	solve_aoc_06_part2("test_data/data_06_2025_test.txt", Out).

aoc_06_part2(Out):-
	solve_aoc_06_part2("data/data_06_2025.txt", Out).

solve_aoc_06_part2(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "\n", "", Strings),
	maplist(string_codes, Strings, StringCodes),
	transpose(StringCodes, Codes),
	split_by_clear_rows(Codes, CodeGroups),
	!,
	maplist(evaluate_lists, CodeGroups, EvaluatedLists),
	sum_list(EvaluatedLists, Out).


split_by_clear_rows([], [[]]).

split_by_clear_rows([NextRow|Rest], [[NextRow|Rows]|Out]):-
	member(X, NextRow),
	X \= 32,
	split_by_clear_rows(Rest, [Rows|Out]).

split_by_clear_rows([NextRow|Rest], [[]|Out]):-
	 \+ (member(X, NextRow),
		X \= 32),
	split_by_clear_rows(Rest, Out).


% To evaluate, first get the operator, then convert the
% remaining codelists to numbers. Operator is the last 
% element of the first list
evaluate_lists([List|Lists], Out):-
	char_code('*', TimesCode),
	append(List1, [TimesCode], List),
	flatten([List1, [32]|Lists], NumberRow),
	phrase(number_row(Numbers),
		NumberRow),
	foldl(times, Numbers, 1, Out).

evaluate_lists([List|Lists], Out):-
	char_code('+', PlusCode),
	append(List1, [PlusCode], List),
	flatten([List1, [32]|Lists], NumberRow),
	phrase(number_row(Numbers),
		NumberRow),
	foldl(plus, Numbers, 0, Out).

	
