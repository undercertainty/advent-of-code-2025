
%%% Day 09
%%%
%%% Part 1 looks straightforward. Bodes ill for part 2.
%%%
%%% Yes, part 2 was a right slog. Several attempts at solving before
%%% I eventually settled on a scanline fill

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(yall)).
:- use_module(library(apply)).

:- dynamic tile_col/2.
:- dynamic agenda/2.

aoc_09_test_part1(Out):-
	solve_aoc_09_part1("test_data/data_09_2025_test.txt", Out).

aoc_09_part1(Out):-
	solve_aoc_09_part1("data/data_09_2025.txt", Out).

solve_aoc_09_part1(FileName, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(tiles(Tiles),
		FileCodes),
	!,
	pairs(Tiles, TilePairs),
	map_list_to_pairs(rectangle_size, TilePairs, Sizes),
	keysort(Sizes, Sorted),
	last(Sorted, Size - _Coords),
	Size = Out.
	 

tiles([])-->
	eol.
tiles([[X, Y]|Tiles])-->
	 integer(X),
	",",
	 integer(Y),
	eol,
	tiles(Tiles).

pairs([_], []).
pairs([Next|Rest], Out):-
	pairs(Next, Rest, O1),
	pairs(Rest, O2),
	append(O1, O2, Out).

pairs(_, [], []).
pairs(X, [Next|Rest], [[X, Next]|Out]):-
	pairs(X, Rest, Out).

% Need extra +1 in each dimension because of the
% specified coordinate system
rectangle_size([[X1, Y1], [X2, Y2]], Out):-
	Out is (abs(X2 - X1) + 1)*(abs(Y2 - Y1) + 1).



%%%%%%%%%%%%%%%%
%
% Part 2

% Last ditch attempt using an attempt at a scanline fill
%
% I'm going to take advantage of the fact that there's only one vertical
% line per X coordinate. (That applies to the test data and the actual
% data)


aoc_09_test_part2(Out):-
	solve_aoc_09_part2("test_data/data_09_2025_test.txt", Out).

aoc_09_part2(Out):-
	solve_aoc_09_part2("data/data_09_2025.txt", Out).

solve_aoc_09_part2(FileName, SizeOut, [C1Out, C2Out]):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(tiles(Tiles),
		FileCodes),
	!,
	\+ assert_columns(Tiles),


findall(Size-[Coord1, Coord2], (append(_, [Coord1|R], Tiles), member(Coord2, R), rectangle_size([Coord1, Coord2], Size)), Rectangles), keysort(Rectangles, Sorted), reverse(Sorted, SortedRev),

member(SizeOut-[C1Out, C2Out], SortedRev),contains_rectangle(C1Out, C2Out).

t([size-Size, coords-Coords]) :-
	solve_aoc_09_part2("test_data/data_09_2025_test.txt", Size, Coords).
t1([size-Size, coords-Coords]) :-
	solve_aoc_09_part2("data/data_09_2025.txt", Size, Coords).

assert_columns(Codes):-
	retractall(columns(_, _)),
	init(Codes, Grid),
	fill_grid(Grid, Out),
	retractall(column(_, _)),
	!,
	member(X - Ys, Out),
	member(Y, Ys),
	assert(column(X, Y)),
	fail.


contains_rectangle([X1, Y1], [X2, Y2]):-
	msort([X1, X2], [XMin, XMax]),
	msort([Y1, Y2], [YMin, YMax]),
	\+ (between(XMin, XMax, X),
	 \+ (column(X, YMinCol - YMaxCol),
		YMinCol =< YMin,
		YMax =< YMaxCol)).


limits(Pairs, [[MinX, MinY], [MaxX, MaxY]]):-
	maplist([X, Y, Z]>>(X=[Y, Z]), Pairs, Xs, Ys),
	min_list(Xs, MinX),
	min_list(Ys, MinY),
	max_list(Xs, MaxX),
	max_list(Ys, MaxY).
	
init(Pairs, FinalGrid):-
	limits(Pairs, [[MinX, _MinY], [MaxX, _MaxY]]),
	succ(GridMinX, MinX),
	%succ(GridMinY, MinY),
	succ(MaxX, GridMaxX),
	%succ(MaxY, GridMaxY),
	findall(X-[], between(GridMinX, GridMaxX, X), InitialGrid),
	init(Pairs, InitialGrid, FinalGrid).

init([], GridCoords, FinalGrid):-
	get_vertical_grid_edges(GridCoords, FilledGrid),
	keysort(FilledGrid, FinalGrid).

init([[X, Y]|Rest], Grid, FinalGrid):-
	select(X-Ys, Grid, Grid1),
	init(Rest, [X-[Y|Ys]|Grid1], FinalGrid).

get_vertical_grid_edges([], []).
get_vertical_grid_edges([X-[]|Rest], [X-[]|GridOut]):-
	get_vertical_grid_edges(Rest, GridOut).
get_vertical_grid_edges([X-[Y1, Y2]|Rest], [X-[Y1-Y2]|GridOut]):-
	Y1<Y2,
	get_vertical_grid_edges(Rest, GridOut).
get_vertical_grid_edges([X-[Y1, Y2]|Rest], [X-[Y2-Y1]|GridOut]):-
	Y1>Y2,
	get_vertical_grid_edges(Rest, GridOut).

% Takes an initial grid of vertical edges, as created by init/2

fill_grid(InitialGrid, FilledGrid):-
	fill_grid(InitialGrid, [], [], FilledGridUnsorted),
	keysort(FilledGridUnsorted, FilledGrid).

fill_grid([], _, FilledGrid, FilledGrid).

fill_grid([X-Ys|Rest], PrevCol, GridSoFar, GridOut):-
	%(Ys=[] -> true ;
	%(writeln([X-Ys, PrevCol]),nl)),
	next_column(Ys, PrevCol, NewCol, GridLine),
	fill_grid(Rest, NewCol, [X-GridLine|GridSoFar], GridOut).

consolidate_edges(Col, Col):-
	\+ (member(_YLow-Y, Col),
        member(Y-_YHigh, Col)).
consolidate_edges(Col, ColFinal):-
	select(YLow-Y, Col, Col1),
	select(Y-YHigh, Col1, Col2),
	consolidate_edges([YLow-YHigh|Col2], ColFinal).



% next_column/4
%
% Tricky one, this... return a mask for the next row, as well as how
% the existing row is filled. I'm going to take advantage of the fact
% that there is at most one vertical edge for each value of X.


% Subsumption
subsume(Y1-Y4, Y2-Y3):-
	Y1 < Y2,
	Y3 < Y4.
subsume(Y2-Y3, Y1-Y4):-
	Y1 < Y2,
	Y3 < Y4.


% If there's no edge in this column, the mask goes through unchanged,
% and the gridline is just the same as the mask.
next_column([], PrevCol, PrevCol, PrevCol).

% If there's an edge, and it doesn't contain any existing edges, can
% just add it (the consolidation takes care of the addition)
next_column([YLow-YHigh], PrevCol, ColConsolidated, ColConsolidated):-
	\+ (member(YLowCol-YHighCol, PrevCol),
	    YLow =< YLowCol,
        YHighCol =< YHigh),
	\+ (member(YLowCol-YHighCol, PrevCol),
	    YLowCol =< YLow,
        YHigh =< YHighCol),
	consolidate_edges([YLow-YHigh|PrevCol], ColConsolidated).

% Now, an internal abutment (is that a word?) with a longer edge.
% For the first step, apply if there's no additional crossers.

next_column([YLow-Y], PrevCol, ColConsolidated, ColConsolidated):-
	\+ (member(FillEdge, PrevCol), subsume(YLow-Y, FillEdge)),
	select(YLow-YHigh, PrevCol, PrevCol1),
	Y < YHigh,
	consolidate_edges([Y-YHigh|PrevCol1], ColConsolidated).

next_column([Y-YHigh], PrevCol, ColConsolidated, ColConsolidated):-
	\+ (member(FillEdge, PrevCol), subsume(Y-YHigh, FillEdge)),
	select(YLow-YHigh, PrevCol, PrevCol1),
	YLow < Y,
	consolidate_edges([YLow-Y|PrevCol1], ColConsolidated).

% Now, if an edge is completely contained by an edge in the filled grid,
% then it splits it for the next column. Don't need to consolidate, because
% there won't be new abutting edges.

next_column([YLow-YHigh], PrevCol, [YLowCol-YLow, YHigh-YHighCol|PrevCol1], [YLowCol-YLow, YHigh-YHighCol|PrevCol1]):-
	select(YLowCol-YHighCol, PrevCol, PrevCol1),
	YLowCol < YLow,
	YHigh < YHighCol.

% Now, if an edge matches an edge in the filled grid, remove it, but add to
% the final column.
next_column([Edge], PrevCol, PrevCol1, PrevCol):-
	select(Edge, PrevCol, PrevCol1).

% Phew!