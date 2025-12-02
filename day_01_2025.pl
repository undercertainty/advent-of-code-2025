
%%% I'm sure that this will all be much easier to solve with python,
%%% but I'm doing some prolog at the moment, and feel like it might
%%% be fun to do a few of this year's tasks in it.

%%% I'm also interested in how far I can use DCGs.

:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).



aoc_01_test_part1(Out):-
    solve_aoc_01_part1("data/data_01_2025_test.txt", Out).

aoc_01_part1(Out):-
    solve_aoc_01_part1("data/data_01_2025.txt", Out).

solve_aoc_01_part1(FileName, Out):-
    read_file_to_codes(FileName, Codes, []),
    phrase(rotator(50, EndPoints, _ZeroPasses), Codes),
    msort(EndPoints, EP),
    clumped(EP, ClumpedEP),
    member(0-Out, ClumpedEP).


% I suspect that passing 0 will be a part of the next task,
% so I'll build that in now.
%
% There's probably a mathematically quicker way of doing this, but
% I can't be bothered to work it out at the moment.
%
% rotate_dial(?Direction, +Number, ?Start, ?Finish, -PassZero)

% Start by running through any >100 turns
rotate_dial(Direction, Num, Start, Finish, PassZero):-
    PassZero1 is Num // 100,
    Num1 is Num rem 100,
    rotate_dial_(Direction, Num1, Start, Finish, PassZero2),
    PassZero is PassZero1 + PassZero2.

% Num is now <100
rotate_dial_(left, Num, Start, Finish, 1):-
    Num>Start,
    Finish is 100 + Start - Num.

rotate_dial_(left, Num, Start, Finish, 0):-
    Num=<Start,
    Finish is Start - Num.

rotate_dial_(right, Num, Start, Finish, 1):-
    Num + Start >= 100,
    Finish is Start + Num - 100.

rotate_dial_(right, Num, Start, Finish, 0):-
    Num + Start <100,
    Finish is Start + Num.


% For part 1, we'll keep a list of where the pointer ends up.

rotator(Start, 
        [Finish|Finishes],
        ZeroPasses) --> "L",
                        digits(NumberCodes),
                        eol,

                            {number_codes(Clicks, NumberCodes),
                            rotate_dial(left, Clicks, Start,Finish, ZP1)},

                        rotator(Finish, Finishes, ZP),
                        
                            {ZeroPasses is ZP + ZP1}.

rotator(Start,
        [Finish|Finishes],
        ZeroPasses) --> "R",
                        digits(NumberCodes),
                        eol,

                            {number_codes(Clicks, NumberCodes),
                             rotate_dial(right, Clicks, Start, Finish, ZP1)},

                        rotator(Finish, Finishes, ZP),

                            {ZeroPasses is ZP + ZP1}.

rotator(_, [], 0) --> eos.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%%% Part 2:

% passing 0 was indeed the next task,
% so just tweak the inputs.

aoc_01_test_part2(Out):-
    solve_aoc_01_part2("data/data_01_2025_test.txt", Out).

aoc_01_part2(Out):-
    solve_aoc_01_part2("data/data_01_2025.txt", Out).

solve_aoc_01_part2(FileName, ZeroPasses):-
    read_file_to_codes(FileName, Codes, []),
    phrase(rotator(50, _EndPoints, ZeroPasses), Codes).

