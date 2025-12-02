
%%% Day 2

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).


aoc_test(Out):-
    solve_aoc("data/data_02_2025_test.txt", Out).

aoc(Out):-
    solve_aoc("data/data_02_2025.txt", Out).

solve_aoc(FileName, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(ranges(InvalidIds), Codes),
    sumlist(InvalidIds, Out).

% For part 1, we're not worrying about efficiency.

/*
% Invalidity check for part 1 : Comment out as needed
invalid_id(Id):-
    number_codes(Id, IdCodes),
    append(Half, Half, IdCodes),!.

*/

% Invalidity check for part 2 : Comment out as needed
invalid_id(Id):-
    number_codes(Id, IdCodes),
    % Repeating pattern must be at least 1 long
    append([P|Prefix], Rest, IdCodes),
    repeating_pattern([P|Prefix], Rest),
    !. % (Avoid multiple solutions in eg. 333333 -> 33+33+33 or 333+333)

repeating_pattern(Pattern, Pattern).
repeating_pattern(Pattern, List):-
    append(Pattern, Rest, List),
    repeating_pattern(Pattern, Rest).

%%%


ranges(InvalidIds) --> range(InvalidIdsHead),
                       ",",
                       ranges(InvalidIDsRest),
                    {append(InvalidIdsHead, InvalidIDsRest, InvalidIds)}.


ranges(InvalidIds) --> range(InvalidIds), eos.

range(InvalidIds) --> digits(StartCodes), "-", digits(EndCodes),
                {number_codes(Start, StartCodes),
                 number_codes(End, EndCodes),
                 findall(Id, (between(Start, End, Id),
                              invalid_id(Id)),
                        InvalidIds)
                 }.

% Surprisingly, didn't have a speed problem with part 2.