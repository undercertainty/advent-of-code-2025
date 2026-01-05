
%%% Day 10
%%%


:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(simplex)).
:- use_module(library(gensym)).
:- use_module(library(yall)).
:- use_module(library(apply)).


aoc_10_test_part1(Out):-
	solve_aoc_10_part1("test_data/data_10_2025_test.txt", Out).

aoc_10_part1(Out):-
	solve_aoc_10_part1("data/data_10_2025.txt", Out).

solve_aoc_10_part1(FileName, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(machines(Solutions),
		FileCodes),
	maplist(length, Solutions, SolutionLengths),
	sum_list(SolutionLengths, Out).


% OK, for part 1, we should be able to do a nice easy bfs. Part 2 will
% clearly be harder, and I'll anticipate needing a constraint solver
% for it, but I'll cross that bridge when we come to it.

button_sequence(LightDiagram, [], LightDiagram).
button_sequence(LightDiagramIn, [Button|Buttons], LightDiagramOut):-
	button(LightDiagramIn, Button, LD),
	button_sequence(LD, Buttons, LightDiagramOut).

button(LightDiagram, [], LightDiagram).
button(LightDiagramIn, [Switch|Rest], LightDiagramOut):-
	switch(LightDiagramIn, Switch, LD),
	button(LD, Rest, LightDiagramOut).

switch(LightDiagramIn, SwitchNum, LightDiagramOut):-
	append(L1, ['.'|L2], LightDiagramIn),
	length(L1, SwitchNum),
	append(L1, ['#'|L2], LightDiagramOut).

switch(LightDiagramIn, SwitchNum, LightDiagramOut):-
	append(L1, ['#'|L2], LightDiagramIn),
	length(L1, SwitchNum),
	append(L1, ['.'|L2], LightDiagramOut).

% I'll have a quick run without doing anything clever
% first. Just keep increasing the number of combinations
% until we hit on one that works. Even brutes are more
% sophisticated than this...

% For the solver, use the length of the Target to 
% generate the initial state:
%
% Works as:
%
% brute_solve(['.', '.', '.', '#', '.'], 
%             [[0, 2, 3, 4], [2, 3], [0, 4], [0, 1, 2], [1, 2, 3, 4]],
%             [[0, 4], [0, 1, 2], [1, 2, 3, 4]] ).

brute_solve(Target, Buttons, Sequence):-
	once(brute_solve_(Target, Buttons, Sequence)).

brute_solve_(Target, Buttons, Sequence):-
		initial_state(Target, InitialState),
	subsets(Buttons, Sequences),
	member(Sequence, Sequences),
	button_sequence(InitialState, Sequence, Target).

initial_state([], []).
initial_state([_|R], ['.'|O]):-
		initial_state(R, O).

% Want subset ordered by size

subsets(Items, Out):-
	powerset(Items, Powerset),
	maplist([List, O] >> (length(List, L),
			O = L - List),
		Powerset,
		S),
	keysort(S, Sorted),
	pairs_values(Sorted, Out).


powerset([], [[]]).
powerset([Next|Rest], Out):-
	powerset(Rest, PS1),
	maplist({
			Next
			}/[L1, L2] >> (L2 = [Next|L1]),
		PS1,
		PS2),
	append(PS1, PS2, Out).

% Now we've got the solver, we can calculate the total
% number of presses directly in the file parser:

machines([])-->
	[].
machines([Solution|Solutions])-->
	machine(Solution),
	eol,
	machines(Solutions).

machine(Solution)-->
	lights(Lights),
	blanks,
	wirings(Wirings),
	blanks,
	joltages(_Joltages),
	{
		brute_solve(Lights, Wirings, Solution)
		}.

lights(Lights)-->
	"[",
	lights_(Lights),
	"]".

lights_([])-->
	[].
lights_(['.'|Lights])-->
	".",
	lights_(Lights).
lights_(['#'|Lights])-->
	"#",
	lights_(Lights).

wirings([])-->
	[].
wirings([Wiring|Wirings])-->
	wiring(Wiring),
	blanks,
	wirings(Wirings).

wiring(Wiring)-->
	"(",
	wiring_(Wiring),
	")".

wiring_([Wiring])-->
	 integer(Wiring).
wiring_([Wiring|Wiring_])-->
	 integer(Wiring),
	",",
	wiring_(Wiring_).

joltages(Joltages)-->
	"{",
	joltages_(Joltages),
	"}".

joltages_([Joltage])-->
	 integer(Joltage).
joltages_([Joltage|Joltages])-->
	 integer(Joltage),
	",",
	joltages_(Joltages).


%%%%%%%%%%%%%%%%
%
% Part 2

% I'll use the simplex library for this one. Feels slightly 
% cheating, but I've only got so much time on this earth...


aoc_10_test_part2(Out):-
	solve_aoc_10_part2("test_data/data_10_2025_test.txt", Out).

aoc_10_part2(Out):-
	solve_aoc_10_part2("data/data_10_2025.txt", Out).

solve_aoc_10_part2(FileName, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(machines2(Problems),
		FileCodes),
	maplist(solve_problem_line, Problems, Solutions),
	sum_list(Solutions, Out).

t(X):-
	aoc_10_test_part2(X).
t1(X):-
	aoc_10_part2(X).

% A more straightforward parse this time, just
% extracting the main information.

machines2([])-->
	[].
machines2([Problem|Problems])-->
	machine2(Problem),
	eol,
	machines2(Problems).

machine2([target-Lights, buttons-Wirings, joltages-Joltages])-->
	lights(Lights),
	blanks,
	wirings(Wirings),
	blanks,
	joltages(Joltages).


% OK, we've got the file information in a managable
% format. Now we need to set up the constraints.
% Fortunately, the problem's working a line at a time
% again, so we don't need to do anything that uses
% more than one input line at once.


% solve_problem_line/2 takes the parsed problem spec, returns the
% optimum

solve_problem_line(Problem, Solution):-
	generate_variables(Problem, ProblemWithVars),
	generate_constraints(ProblemWithVars, Constraints, Targets),
	member(simplex-SimplexVars, ProblemWithVars),
	get_constrained_state(SimplexVars, Constraints, Targets, ConstrainedState),
	minimize(SimplexVars, ConstrainedState, SolutionState),
	extract_solution(SolutionState, SimplexVars, Solution).


% generate_variables/2 just creates a fresh atom for 
% each button in the machine, and adds the variables
% to the problem spec
generate_variables(Problem, [simplex-Vars|Problem]):-
	member(buttons-Buttons, Problem),
	maplist([_X, Y]>>gensym(b, Y), Buttons, Vars).


% generate_constraints/3 takes the problem space, and then
% has Constraints = lists of each simplex variable which
% sums to the nth joltage, and Targets = target joltages.

generate_constraints(Problem, Constraints, Targets):-
	member(joltages-Joltages, Problem),
	length(Joltages, JL1),
	succ(JL, JL1),
	numlist(0, JL, Js),
	maplist({Problem}/[N, C, T]>>generate_nth_constraint(N, Problem, C, T), Js, Constraints, Targets).

% generate_nth_constraint
% n is the nth joltage to be set
generate_nth_constraint(N, Problem, Constraints, Target):-
	member(buttons-Buttons, Problem),
	member(joltages-Joltages, Problem),
	member(simplex-SimplexVars, Problem),
	nth0(N, Joltages, Target),
	generate_nth_constraint_(N, SimplexVars, Buttons, Constraints).

generate_nth_constraint_(_, [], [], []).
generate_nth_constraint_(N, [Var|Vars], [Button|Buttons], [Var|Out]):-
	member(N, Button),
	generate_nth_constraint_(N, Vars, Buttons, Out).
generate_nth_constraint_(N, [_Var|Vars], [Button|Buttons], Out):-
	\+ member(N, Button),
	generate_nth_constraint_(N, Vars, Buttons, Out).

% Return a constrained state

get_constrained_state(SimplexVars, Constraints, Targets, State):-
	gen_state(S0),
	add_integrality_constraints(SimplexVars, S0, S1),
	add_linear_constraints(Constraints, Targets, S1, State).

% add the integral constraints...

add_integrality_constraints([], State, State).
add_integrality_constraints([Var|Vars], State, StateOut):-
	constraint(integral(Var), State, State1),
	add_integrality_constraints(Vars, State1, StateOut).

% ... and the linear constraints

add_linear_constraints([], [], State, State).
add_linear_constraints([VarList|Vars], [Target|Targets], State, StateOut):-
	constraint(VarList = Target, State, State1),
	add_linear_constraints(Vars, Targets, State1, StateOut).

% Get the final solution out of the state.

extract_solution(State, Vars, Solution):-
	maplist({State}/[Var, Val]>>variable_value(State, Var, Val), Vars, Values),
	sum_list(Values, Solution).


