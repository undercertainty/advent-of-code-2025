
%%% Day 11
%%%

% :- table path/3.
:- dynamic connected/2, node/1.

:- set_prolog_flag(table_space, 8589934592).

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(simplex)).
:- use_module(library(gensym)).
:- use_module(library(yall)).
:- use_module(library(apply)).

% I don't normally approve of this, but I'll assert the 
% connections in the first instance. Makes the task a
% bit easier to think about.


aoc_11_test_part1(Out):-
	solve_aoc_11_part1("test_data/data_11_2025_test.txt", Out).

aoc_11_part1(Out):-
	solve_aoc_11_part1("data/data_11_2025.txt", Out).

solve_aoc_11_part1(FileName, Out):-
	assert_graph(FileName),
	!,
	findall(Path, path(you, out, Path), Paths),
	length(Paths, Out).

% parse the input

server_rack([])-->
	[].
server_rack(ServerRack)-->
	device(Device),
	eol,
	server_rack(Devices),
	{append(Device, Devices, ServerRack)}.
	
device(Connections)-->
	string(SourceCodes),
	{atom_codes(Source, SourceCodes)},
	":",
	whites,
	destinations(Source, Connections).

destinations(_, [])-->
	[].
destinations(Source, [connected(Source, Destination)|Destinations])-->
	nonblanks(DestinationCodes),
	whites,
	destinations(Source, Destinations),
	{atom_codes(Destination, DestinationCodes)}.

% create predicates

assert_graph(FileName):-
	retractall(connected(_, _)),
	retractall(node(_)),
	read_file_to_codes(FileName, FileCodes, []),
	phrase(server_rack(Graph),
		FileCodes),
	(member(Clause, Graph),
		assert(Clause),
		fail
		;
		true),
	findall(Node,
		(connected(Node, _)
		;
		connected(_, Node)),
		Nodes),
	list_to_set(Nodes, NodeSet),
	(member(Node, NodeSet),
		assert(node(Node)),
		fail
		;
		true),
	!.

% Don't need to search yet... can just find all the paths

path(Destination, Destination, [Destination]).
path(Source, Destination, [Source|Path]):-
	connected(Source, Conn),
	path(Conn, Destination, Path).


t(X):-
	aoc_11_test_part1(X).
t1(X):-
	aoc_11_part1(X).


%%%%%%%%%%%%%%%%
%
% Part 2

% I assume that there are some loops in the actual input..?
% 
% A quick search suggests there might not be.

aoc_11_test_part2(Out):-
	solve_aoc_11_part2("test_data/data_11_2025_test_2.txt", Out).

aoc_11_part2(Out):-
	solve_aoc_11_part2("data/data_11_2025.txt", Out).


tt(X):-
	aoc_11_test_part2(X).
tt1(X):-
	aoc_11_part2(X).

% Not the most elegant, but no particular benefit in
% splitting the predicate up further

solve_aoc_11_part2(FileName, NumPaths):-
	assert_graph(FileName),
	initial_state(InitialState),

	% Paths from svr to fft without dac
	set_source(svr, InitialState, State1),
	remove_parents(dac, State1, State1a),
	complete_graph_paths(State1a, fft, NumSvrFft),

	% Paths from svr to dac without fft
	set_source(svr, InitialState, State2),
	remove_parents(fft, State2, State2a),
	complete_graph_paths(State2a, dac, NumSvrDac),

	% Paths from fft to out without dac
	set_source(fft, InitialState, State3),
	remove_parents(dac, State3, State3a),
	complete_graph_paths(State3a, out, NumFftOut),

	% Paths from dac to out without fft
	set_source(dac, InitialState, State4),
	remove_parents(fft, State4, State4a),
	complete_graph_paths(State4a, out, NumDacOut),

	% Paths from dac to fft
	set_source(dac, InitialState, State5),
	complete_graph_paths(State5, fft, NumDacFft),

	% Paths from fft to dac
	set_source(fft, InitialState, State6),
	complete_graph_paths(State6, dac, NumFftDac),

	% Number of paths is number of
	% 	svr -> dac -> fft -> out
	%	+
	%	svr -> fft -> dac -> out

	NumPaths is ((NumSvrDac * NumDacFft * NumFftOut) +
				(NumSvrFft * NumFftDac * NumDacOut)).
	

%%%
%%% tabling is still running out of memory, so do it
%%% all manually

initial_state(State):-
	findall(Node, initial_node_state(Node), State).

initial_node_state([node-Node, parents-Parents, cost-0]):-
	node(Node),
	findall(Parent, connected(Parent, Node), Parents).

%set_source(Source, StateIn, [[node-Source, parents-Parents, cost-1]|State]):-
%	select([node-Source, parents-Parents, cost-_], StateIn, State).

set_source(Source, StateIn, [[node-Source, parents-[], cost-1]|State]):-
	select([node-Source|_], StateIn, State).

% Use remove_parents/3 to remove the routes into a node
remove_parents(Node, State, [[node-Node, parents-[], cost-0]|StateOut]):-
	select([node-Node|_], State, StateOut).



count_paths(Source, Destination, NumPaths):-
	initial_state(InitialState),
	set_source(Source, InitialState, State),
	complete_graph_paths(State, Destination, NumPaths).

complete_graph_paths(State, Destination, NumPaths):-
	complete_graph_paths(State, Destination, [], NumPaths).

complete_graph_paths(_State, Destination, Visited, Cost):-
	member(Destination - Cost, Visited).
complete_graph_paths(StateIn, Destination, Visited, StateOut):-
	select([node - Node, parents - [], cost - Cost], StateIn, State),
	add_costs(Node, Cost, State, State1),
	complete_graph_paths(State1, Destination, [Node - Cost|Visited], StateOut).


add_costs(_Node, _Cost, [], []).
add_costs(Node, Cost, [[node-N, parents-P, cost-C]|Rest], [[node-N, parents-P1, cost-C1]|Out]):-
	select(Node, P, P1),
	C1 is C + Cost,
	add_costs(Node, Cost, Rest, Out).
add_costs(Node, Cost, [[node-N, parents-P, cost-C]|Rest], [[node-N, parents-P, cost-C]|Out]):-
	\+ member(Node, P),
add_costs(Node, Cost, Rest, Out).

