
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



% q:-
% 	retractall(columns(_, _)),
% 	t(Pairs),
% 	 init(Pairs, Grid),
% 	fill_grid(Grid, Out),
% 	retractall(column(_, _)),
% 	!,
% 	member(X - Ys, Out),
% 	member(Y, Ys),
% 	assert(column(X, Y)),
% 	fail.
% q.

% q1:-
% 	retractall(columns(_, _)),
% 	t1(Pairs),
% 	 init(Pairs, Grid),
% 	fill_grid(Grid, Out),
% 	retractall(column(_, _)),
% 	!,
% 	member(X - Ys, Out),
% 	member(Y, Ys),
% 	assert(column(X, Y)),
% 	fail.
% q1.

% contains_point(X, Y):-
% 	column(X, YLow-YHigh),
% 	between(YLow, YHigh, Y).

contains_rectangle([X1, Y1], [X2, Y2]):-
	msort([X1, X2], [XMin, XMax]),
	msort([Y1, Y2], [YMin, YMax]),
	\+ (between(XMin, XMax, X),
	 \+ (column(X, YMinCol - YMaxCol),
		YMinCol =< YMin,
		YMax =< YMaxCol)).








% r(Out):-
% 	t(Out),
% 	\+ q.

% r1(Out):-
% 	t1(Out),
% 	\+ q1.




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

/*
fill_grid([X-Ys|Rest], PrevCol, GridSoFar, GridOut):-
	writeln([X-Ys, PrevCol]),nl,
	next_column(Ys, PrevCol, NewCol, GridLine),
	fill_grid(Rest, NewCol, [X-GridLine|GridSoFar], GridOut).

*/

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
%
% The mask is the line that's passed along from the previous column.
/*


% Two edges can touch if they abut, or if one subsumes the other

touching(Edge1, Edge2):-
	abut_external(Edge1, Edge2).
touching(Edge1, Edge2):-
	abut_internal(Edge1, Edge2).
touching(Edge1, Edge2):-
	subsume(Edge1, Edge2).

% Abutting
abut_external(_Y1-Y2, Y2-_Y3).
abut_external(Y2-_Y3, _Y1-Y2).

% Internal edge match
abut_internal(Y1-_Y3, Y1-_Y2).
abut_internal(_Y1-Y3, _Y2-Y3).

*/

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


/*
next_column([YLow-Y], PrevCol, ColConsolidated, ColConsolidated):-
	\+ (member(FillEdge, PrevCol), subsume(YLow-Y, FillEdge)),
	select(YLow-YHigh, PrevCol, PrevCol1),
	Y < YHigh,
	succ(Y, Y1),
	consolidate_edges([Y1-YHigh|PrevCol1], ColConsolidated).

next_column([Y-YHigh], PrevCol, ColConsolidated, ColConsolidated):-
	\+ (member(FillEdge, PrevCol), subsume(Y-YHigh, FillEdge)),
	select(YLow-YHigh, PrevCol, PrevCol1),
	YLow < Y,
	succ(Y1, Y),
	consolidate_edges([YLow-Y1|PrevCol1], ColConsolidated).
*/



/*

% If they match, then return an empty column

next_column([YLow-YHigh], [YLow-YHigh], [], [YLow-YHigh]).

% If the previous edge meets an edge which expands it:
next_column([YLow-YHigh], [YPrev-YLow], [YPrev-YHigh], [YPrev-YHigh]).
next_column([YLow-YHigh], [YHigh-YPrev], [YLow-YPrev], [YLow-YPrev]).

% If the previous edge meets an edge which reduces it:
next_column([YLow-YHigh], [YLow-YPrev], [YPrev-YHigh], [YPrev-YHigh]):-
	YHigh > YPrev.
next_column([YLow-YHigh], [YPrev-YHigh], [YLow-YPrev], [YLow-YPrev]):-
	YLow < YPrev.


*/

/*
% If there is an edge that doesn't match any of the segments in the
% mask, then add it to the line and the mask
apply_mask([YLow-YHigh], PreviousRow, [YLow-YHigh|PreviousRow], [YLow-YHigh|PreviousRow]):-
	 \+ (member(Y1 - Y2, PreviousRow),
		Y1 < YLow,
		YHigh < Y2),
	 \+ (member(Y1 - Y2, PreviousRow),
		YLow < Y1,
		Y2 < YHigh).



apply_mask([YLow-YHigh], PreviousRow)

	
*/







































/* 


% initialise(+Redtiles)

initialise(RedTiles):-
	retractall(tile_col(_, _)),
	sort(1, @<, RedTiles, [[X1, _]|Rest]), 
	last(Rest, [X2, _]),
	numlist(X1, X2, Xs),
	maplist([X]>>assert(tile_col(X, [])), Xs, _).

tile_in_grid(X, Y):-
	tile_col(X, Ys),
	member(Y1-Y2, Ys),
	between(Y1, Y2, Y).

% assumes that X is in the grid
add_tile(X, Y):-
	retract(tile_col(X, Ys)),
	merge_sections([Y-Y|Ys], NewYs),
	assert(tile_col(X, NewYs)).

% build_loop(+RedTiles)
%
% asserts tile(X, Y) for each tile in the loop

build_loop(RedTiles):-
	loop(RedTiles, Loop),
	build_loop_(Loop).

build_loop_([]).
build_loop_([[X, Y]|Rest]):-
	add_tile(X, Y),
	build_loop_(Rest).

% loop(RedTiles, AllTiles)
%
% True if RedTiles is the set of red tile coords, and
% AllTiles is all the red and green tiles in the loop

loop([Start|RedTiles], LoopOut):-
	last(RedTiles, End),
	loop(Start, End, Edge1),
	loop_([Start|RedTiles], Edges),
	append(Edge1, Edges, LoopOut).

loop_([_], []).
loop_([C1, C2|Rest], LoopOut):-
	loop(C1, C2, Edge),
	loop_([C2|Rest], LoopRest),
	append(Edge, LoopRest, LoopOut).

loop([X, Y1], [X, Y2], EdgeCoords):-
	(Y2 > Y1 ->
	numlist(Y1, Y2, Ys);
	numlist(Y2, Y1, Ys)),
	maplist({
			X
			}/[Y, Z] >> =([X, Y], Z),
		Ys,
		EdgeCoords).
loop([X1, Y], [X2, Y], EdgeCoords):-
	(X2 > X1 ->
	numlist(X1, X2, Xs);
	numlist(X2, X1, Xs)),
	maplist({
			Y
			}/[X, Z] >> =([X, Y], Z),
		Xs,
		EdgeCoords).

% And with the floodfill...

flood_fill(X, Y):-
	retractall(agenda(_, _)),
	asserta(agenda(X, Y)),
	flood_fill.

flood_fill:-
	\+ agenda(_, _).
flood_fill:-
	agenda(X, Y),
	retract(agenda(X, Y)),
	(
		tile_in_grid(X, Y) 
	->
		!,
		flood_fill
	;
		add_tile(X, Y),
		extend_agenda(X, Y),
		!,
		flood_fill).


extend_agenda(X, Y):-
	succ(X0, X),
	succ(X, X1),
	succ(Y0, Y),
	succ(Y, Y1),
	add_to_agenda(X0, Y),
	add_to_agenda(X1, Y),
	add_to_agenda(X, Y0),
	add_to_agenda(X, Y1).

% Don't add if already on the agenda
add_to_agenda(X, Y):-
	agenda(X, Y),
	!.
% Don't add if already a tile
add_to_agenda(X, Y):-
	tile_in_grid(X, Y),
	!.
% Otherwise, add to agenda
add_to_agenda(X, Y):-
	asserta(agenda(X, Y)).







% Run-Length Encoding

points_to_rle(Points, Rle):-
	bounding_rectangle(Points, BottomLeft, TopRight),
	empty_grid(BottomLeft, TopRight, EmptyRle),
	add_loop_to_rle(Points, EmptyRle, Rle).



add_loop_to_rle([Start|RedTiles], RleIn, RleOut):-
	last(RedTiles, End),
	add_line_to_rle(Start, End, RleIn, Rle1),
	add_loop_to_rle_([Start|RedTiles], Rle1, RleOut).

add_loop_to_rle_([_], Rle, Rle).
add_loop_to_rle_([Next1, Next2|Rest], RleIn, RleOut):-
	add_line_to_rle(Next1, Next2, RleIn, Rle1),
	add_loop_to_rle_([Next2|Rest], Rle1, RleOut).

add_point_to_rle([X, Y], RleIn, RleOut):-
	get_assoc(X, RleIn, Row),
	merge_sections([Y - Y|Row], MergedRow),
	get_assoc(X, RleIn, Row, RleOut, MergedRow).


add_line_to_rle([X, Y1], [X, Y2], RleIn, RleOut):-
	get_assoc(X, RleIn, Row),
	(Y1 < Y2 ->
	merge_sections([Y1 - Y2|Row], MergedRow);
	merge_sections([Y2 - Y1|Row], MergedRow)),
	get_assoc(X, RleIn, Row, RleOut, MergedRow).


add_line_to_rle([X1, Y], [X2, Y], RleIn, RleOut):-
	(X1 < X2 ->
	numlist(X1, X2, NumList)
	;
	numlist(X2, X1, NumList)),
	foldl({
			Y
			}/[X, RLE, OUT] >> add_line_to_rle([X, Y], [X, Y], RLE, OUT),
		NumList,
		RleIn,
		RleOut).

% 
merge_sections(SectionsIn, SectionsOut):-
	 \+ (select(X - Y, SectionsIn, LS1),
		member(X1 - _Y1, LS1),
		between(X, Y, X1)),
	 \+ (member(_A - B, SectionsIn),
		member(C - _C, SectionsIn),
		succ(B, C)),
	sort(SectionsIn, SectionsOut).

% Overlapping sections
merge_sections(ListOfSections, SectionsOut):-
	select(X - Y, ListOfSections, LS1),
	select(X1 - Y1, LS1, LS2),
	between(X, Y, X1),
	YO is max(Y, Y1),
	merge_sections([X - YO|LS2], SectionsOut).

% Adjacent sections
merge_sections(ListOfSections, SectionsOut):-
	select(X - Y, ListOfSections, LS1),
	select(X1 - Y1, LS1, LS2),
	succ(Y, X1),
	merge_sections([X - Y1|LS2], SectionsOut).





in_rle([X, Y], Rle):-
	get_assoc(X, Rle, XRow),
	member(Y1-Y2, XRow),
	between(Y1, Y2, Y).


flood_fill_rle_rec(Coord, FilledRle, FilledRle):-
	in_rle(Coord, FilledRle),!.

flood_fill_rle_rec([X, Y], Rle, FilledRle):-
	 \+ in_rle([X, Y], Rle),
	succ(X0, X),
	succ(X, X1),
	succ(Y0, Y),
	succ(Y, Y1),
	add_point_to_rle([X, Y], Rle, Rle1),!,
	flood_fill_rle_rec([X0, Y], Rle1, Rle2),!,
	flood_fill_rle_rec([X1, Y], Rle2, Rle3),!,
	flood_fill_rle_rec([X, Y0], Rle3, Rle4),!,
	flood_fill_rle_rec([X, Y1], Rle4, FilledRle).	

% Do the flood fill with a stack rather than recursively

flood_fill_rle(Coord, Rle, FilledRle):-
	flood_fill_rle_([Coord], Rle, FilledRle).

flood_fill_rle_([], FilledRle, FilledRle).

flood_fill_rle_([Coord|Stack], Rle, FilledRle):-
	in_rle(Coord, Rle),
	flood_fill_rle_(Stack, Rle, FilledRle).

flood_fill_rle_([[X, Y]|Stack], Rle, FilledRle):-
	\+ in_rle([X, Y], Rle),
	add_point_to_rle([X, Y], Rle, Rle1),
	succ(X0, X),
	succ(X, X1),
	succ(Y0, Y),
	succ(Y, Y1),
	list_to_set([[X0, Y], [X1, Y], [X, Y0], [X, Y1]|Stack], NewStack),
	!,
	flood_fill_rle_(NewStack, Rle1, FilledRle).	


% I think that actually, we should have a valid rectangle if it
% doesn't cross any edges in the loop
%
% crossing_edges(Start1, End1, Start2, End2)
%
% succeeds if the line Start1->End1 crosses the line Start2->End2
%
% Note that all lines are either horizontal or vertical.
%
% Ah... (after some failed tries) there are a couple of scenarios
% in which the rectangle covers a gap. They are:






% 1. One of the lines of the rectangle strictly crosses a line
% of the loop

% crossing_edges([L1StartX, L1StartY], [L1EndX, L1EndY], [L2StartX, L2StartY], [L2EndX, L2EndY]

strictly_between(Low, High, X):-
	Low < High,
	Low < X,
	X < High.
strictly_between(Low, High, X):-
	High < Low,
	High < X,
	X < Low.

between_(Low, High, X):-
	Low < High,
	between(Low, High, X).
between_(High, Low, X):-
	Low < High,
	between(Low, High, X).



crossing_edges([[L1X, L1StartY], [L1X, L1EndY]],
	           [[L2StartX, L2Y], [L2EndX, L2Y]]):-
	strictly_between(L2StartX, L2EndX, L1X),
	strictly_between(L1StartY, L1EndY, L2Y).

crossing_edges([[L1StartX, L1Y], [L1EndX, L1Y]],
	           [[L2X, L2StartY], [L2X, L2EndY]]):-
	strictly_between(L1StartX, L1EndX, L2X),
	strictly_between(L2StartY, L2EndY, L1Y).

% Let's have a predicate that is true if a line crosses a loop

line_crosses_loop(Line, Loop):-
	member(LoopLine, Loop),
	crossing_edges(Line, LoopLine).

loop_edges(RedTiles, Edges):-
	append(RT, [Last], RedTiles),
	maplist([X, Y, Z] >> =(Z, [X, Y]), [Last|RT], RedTiles, Edges).


rectangle_crosses_loop(Corner1, Corner2, Loop):-
	once(rectangle_crosses_loop_(Corner1, Corner2, Loop)).

rectangle_crosses_loop_([X1, Y1], [X2, _Y2], Loop):-
	line_crosses_loop([[X1, Y1], [X2, Y1]], Loop).
rectangle_crosses_loop_([_X1, Y1], [X2, Y2], Loop):-
	line_crosses_loop([[X2, Y1], [X2, Y2]], Loop).
rectangle_crosses_loop_([X1, _Y1], [X2, Y2], Loop):-
	line_crosses_loop([[X2, Y2], [X1, Y2]], Loop).
rectangle_crosses_loop_([X1, Y1], [_X2, Y2], Loop):-
	line_crosses_loop([[X1, Y2], [X1, Y1]], Loop).

% 2. Alternatively, an edge from the loop starts on an 
% edge of the rectangle, and cuts into the rectangle.

rectangle_cut([[X1, Y1], [X2, Y2]], Loop):-
	X1 < X2,
	member([[X1, YL], [XL, YL]], Loop),
	strictly_between(Y1, Y2, YL),
	X1< XL.
rectangle_cut([[X1, Y1], [X2, Y2]], Loop):-
	X1 < X2,
	member([[XL, YL], [X1, YL]], Loop),
	strictly_between(Y1, Y2, YL),
	X1< XL.

rectangle_cut([[X2, Y2], [X1, Y1]], Loop):-
	X1 < X2,
	member([[X1, YL], [XL, YL]], Loop),
	strictly_between(Y1, Y2, YL),
	X1< XL.
rectangle_cut([[X2, Y2], [X1, Y1]], Loop):-
	X1 < X2,
	member([[XL, YL], [X1, YL]], Loop),
	strictly_between(Y1, Y2, YL),
	X1< XL.


rectangle_cut([[X1, Y1], [X2, Y2]], Loop):-
	Y1 < Y2,
	member([[XL, Y1], [XL, YL]], Loop),
	strictly_between(X1, X2, XL),
	Y1< YL.
rectangle_cut([[X1, Y1], [X2, Y2]], Loop):-
	Y1 < Y2,
	member([[XL, YL], [XL, Y1]], Loop),
	strictly_between(X1, X2, XL),
	Y1< YL.

rectangle_cut([[X2, Y2], [X1, Y1]], Loop):-
	Y1 < Y2,
	member([[XL, Y1], [XL, YL]], Loop),
	strictly_between(X1, X2, XL),
	Y1< YL.
rectangle_cut([[X2, Y2], [X1, Y1]], Loop):-
	Y1 < Y2,
	member([[XL, YL], [XL, Y1]], Loop),
	strictly_between(X1, X2, XL),
	Y1< YL.




% Get a point in the loop

contained_point(X1, Y1):-
	% Bottom leftmost coordinate...
	findall([X, Ys], tile_col(X, Ys), Tiles),
	min_member([XMin, YRanges], Tiles),
	succ(XMin, X1),
	sort(YRanges, [YMin-_|_]),
	succ(YMin, Y1).

	
% Let's play with intervals.

build_constraint_grid(RedTiles, Constraints):-
	bounding_rectangle(RedTiles, BottomLeft, TopRight),
	constraint_grid(BottomLeft, TopRight, CGrid),
	CGrid = Constraints.

bounding_rectangle(Points, [X1, Y1], [X2, Y2]):-
	sort(1, @=< , Points, [[XMin, _]|SortedX]),
	sort(2, @=< , Points, [[_, YMin]|SortedY]),
	last(SortedX, [XMax, _]),
	last(SortedY, [_, YMax]),
	succ(X1, XMin),
	succ(XMax, X2),
	succ(Y1, YMin),
	succ(YMax, Y2).
	
empty_grid([X1, _Y1], [X2, _Y2], Grid):-
	numlist(X1, X2, Xs),
	maplist([X, Z] >> =(X - [], Z),
		Xs,
		GridList),
	list_to_assoc(GridList, Grid).




	


 */