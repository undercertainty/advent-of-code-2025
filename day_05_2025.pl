
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(library(apply)).


aoc_05_test_part1(Out):-
	solve_aoc_05_part1("test_data/data_05_2025_test.txt", Out).

aoc_05_part1(Out):-
	solve_aoc_05_part1("data/data_05_2025.txt", Out).

solve_aoc_05_part1(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\t\n\s", [StrippedString]),
	string_codes(StrippedString, Codes),
	!,
	phrase(kitchen(Ranges, IDs),
		Codes),
		include({
			Ranges
			}/[ID] >> fresh(ID, Ranges),
		IDs,
		Out).

fresh(IngredientID, Ranges):-
	member([Low, High], Ranges),
	between(Low, High, IngredientID).


kitchen(Ranges, IDs)-->
	ranges(Ranges),
	"\n",
	 ingredients(IDs).

ranges([])-->
	[].
ranges([Range|Ranges])-->
	range(Range),
	eol,
	ranges(Ranges).

range([Start, End])-->
	digits(StartCodes),
	"-",
	digits(EndCodes),
	{
		number_codes(Start, StartCodes),
		number_codes(End, EndCodes)
		}.

ingredients([])-->
	[].
ingredients([ID|IDs])-->
	digits([IDCode|IDCodes]),
	eol,
	 ingredients(IDs),
	{
		number_codes(ID, [IDCode|IDCodes])
		}.


%%%%%%%%%%%%%%%%
%
% Part 2

% Should be pretty straightforward, just merge the ranges.

aoc_05_test_part2(Out):-
	solve_aoc_05_part2("test_data/data_05_2025_test.txt", Out).

aoc_05_part2(Out):-
	solve_aoc_05_part2("data/data_05_2025.txt", Out).

solve_aoc_05_part2(FileName, Out):-
	read_file_to_string(FileName, FileString, []),
	split_string(FileString, "", "\t\n\s", [StrippedString]),
	string_codes(StrippedString, Codes),
	!,
	phrase(kitchen(Ranges, _IDs),
		Codes),
	merge_ranges(Ranges, MergedRanges),
	maplist([Range, RangeSize] >> (Range = [Low, High], RangeSize is High + 1 - Low),
		MergedRanges,
		RangeSizes),
	sumlist(RangeSizes, Out).

% Could have done this a bit more cleverly, but seems to
% be working fine using the language as we teach it.

merge_ranges(RangesIn, RangesOut):-
	select([Low1, High1], RangesIn, R1),
	select([Low2, High2], R1, R2),
	between(Low1, High1, Low2),
	HighOut is max(High1, High2),
	merge_ranges([[Low1, HighOut]|R2], RangesOut).
merge_ranges(Ranges, Ranges):-
	 \+ (member([Low1, High1], Ranges),
		member([Low2, High2], Ranges),
		[Low1, High1] \= [Low2, High2],
		between(Low1, High1, Low2)).