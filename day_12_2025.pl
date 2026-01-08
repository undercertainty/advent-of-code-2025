

%%% Day 12
%%%

% :- table path/3.
:- dynamic connected/2, node/1.

:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(readutil)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).


aoc_12_test_part1(Out):-
	solve_aoc_12_part1("test_data/data_12_2025_test.txt", Out).

aoc_12_part1(Out):-
	solve_aoc_12_part1("data/data_12_2025.txt", Out).

solve_aoc_12_part1(FileName, Out):-
	read_file_to_codes(FileName, FileCodes, []),
	phrase(presents(Presents, Regions),
		FileCodes),
    !,
    initial_partition([Presents, Regions], _TooBig, EasyFit, _Other), 
    length(EasyFit, Out).


t(Out):-
    aoc_12_test_part1(Out).

t1(Out):-
    aoc_12_part1(Out).


% parse the input
%
% I've done up the input so that it includes the flipped
% and rotated variants of each shape. Easier just to do it
% by hand.
%
% I will also assume that everything's 3x3 here. No need
% to be unnecessarily general.

presents(Shapes, Regions)-->
	shapesets(Shapes),
	blanks,
	regions(Regions).

shapesets([])-->
	[].
shapesets([ShapeSet|ShapeSets])-->
	shapeset(ShapeSet),
	blanks,
	shapesets(ShapeSets).

shapeset(Int-ShapeSet)-->
	 integer(Int),
	":",
	blanks,
	shapes(ShapeSet).

shapes([])-->
	[].
shapes([Shape|Shapes])-->
	shape(Shape),
	blanks,
	shapes(Shapes).

shape(Shape)-->
	nonblanks(L1),
	eol,
	nonblanks(L2),
	eol,
	nonblanks(L3),
	{
		convert_shape([L1, L2, L3], Shape)
		}.

% We know that there are exactly 6 numbers for each region

regions([])-->
	[].
regions([Region|Regions])-->
	region(Region),
	eol,
	regions(Regions).

region([size-[X, Y], num-[P0, P1, P2, P3, P4, P5]])-->
	 integer(X),
	"x",
	 integer(Y),
	":",
	blanks,
	 integer(P0),
	blanks,
	 integer(P1),
	blanks,
	 integer(P2),
	blanks,
	 integer(P3),
	blanks,
	 integer(P4),
	blanks,
	 integer(P5).



% Get the elements of the grid which are 35 (='#')
convert_shape(ShapeCodes, Coords):-
	findall([X, Y],
		(nth0(Y, ShapeCodes, Row),
			nth0(X, Row, 35)),
		Coords).
    

%%%
%
% Hmmm... still taking far too long. I suspect the constrained
% version takes longer than memoised DFS would :-(.
%
% Let's identify the obvious quick wins and quick non-possibilities:

% If the output requires more squares to be filled than are in
% the region, then there will be an immediate fail.

too_big_for_region(Shapes, [size-[X, Y], num-Nums]):-
    Size is X * Y,
    pairs_values(Shapes, ShapesValues),
    maplist([A, B]>>(A=[B|_]), ShapesValues, ShapeValues1),
    maplist(length, ShapeValues1, ShapeSizes),
    maplist([A, B, C]>>(C is A*B), Nums, ShapeSizes, FilledSizes),
    sum_list(FilledSizes, Out),
    Out > Size.

% And the presents fit easily if the region can accept that 
% many 3x3 squares. Don't even need the Shapes for this one.

easy_fit([size-[X, Y], num-Nums]):-
    sum_list(Nums, NumPresents),
    EasyCapacity is (X//3) * (Y//3),
    NumPresents =< EasyCapacity.
    
initial_partition([S, P], TooBig, EasyFit, Other):-
    partition(easy_fit, P, EasyFit, NonFit),
    partition({S}/[ProblemCase]>>too_big_for_region(S, ProblemCase), NonFit, TooBig, Other).
    
% Oh Jesus Christ. Is that it?
%
% Please admire my beautiful constraint solving code
% below while basking in the schadenfreude of time
% wasted.

    
    
%%%%%
%
% OK, now we've parsed the input, we want to set up a set
% of constrained variables for each present. Needs to be a
% disjunction of the various orientations of the present.
%
% Shape is a set of internal coordinates for a 3x3 shape.

% Take a set of shapes and return the completely constrained
% variable list. The shapes are the alternative shapes that 
% represent the different orientations of a given shape.
% RegionX and RegionY are the X and Y dimensions of the area
% to put the shapes into.
%
% Need AllVars for the labelling, ZVars to ensure there's no
% overlap between the shapes.

constrain_shape_set(ShapeSet, RegionX, RegionY, AllVars, ZVars):-
    constrain_shape_set(ShapeSet, RegionX, RegionY, AllVars, ZVars, Constraint),
    Constraint.

constrain_shape_set([Shape|Shapes], RegionX, RegionY, AllVars, Zs, Constraint):-
    general_shape_constraint(Shape, RegionX, RegionY, [Xs, Ys, Zs]),
    flatten([Xs, Ys, Zs], AllVars),
    constrain_shape_set([Shape|Shapes], Xs, Ys, Constraint).

constrain_shape_set([LastShape], Xs, Ys, Constraint):-
    specific_shape_constraint(LastShape, Xs, Ys, Constraint).

constrain_shape_set([Shape|Shapes], Xs, Ys, Constraint #\/ Constraints):-
    specific_shape_constraint(Shape, Xs, Ys, Constraint),
    constrain_shape_set(Shapes, Xs, Ys, Constraints).



% general_shape_constraint gets the constraints that apply to
% the shape before flipping etc.

general_shape_constraint(Shape, RegionX, RegionY, [Xs, Ys, Zs]):-
    length(Shape, L),
    length(Xs, L),
    length(Ys, L),
    length(Zs, L),
    
    succ(RegionX0, RegionX),
    succ(RegionY0, RegionY),

    Xs ins 0..RegionX0,
    Ys ins 0..RegionY0,

    RegionSize is (RegionX * RegionY) - 1,
    Zs ins 0..RegionSize,

    constrain_zs(RegionX, Xs, Ys, Zs).

constrain_zs(_, [], [], []).
constrain_zs(RegionX, [X|Xs], [Y|Ys], [Z|Zs]):-
    Z #= X + RegionX * Y,
    constrain_zs(RegionX, Xs, Ys, Zs).

% specific_shape_constraint updates a given set of Xs and Ys.
% Constraint is a constraint description that we can use for a
% disjunction

specific_shape_constraint([[X, Y]|Shape], [Xvar|Xvars], [Yvar|Yvars], Constraint):-
    specific_shape_constraint(X, Y, Shape, Xvar, Yvar, Xvars, Yvars, Constraint).

% Keep constraints in the recursion, and reify them afterwards


specific_shape_constraint(X, Y, [[X1, Y1]], Xvar, Yvar, [Xvar1], [Yvar1], (Xvar1 #= Xvar + Xdiff) #/\ (Yvar1 #= Yvar + Ydiff)):-
    Xdiff is X1-X,
    Ydiff is Y1-Y.
specific_shape_constraint(X, Y, [[X1, Y1]|Shape], Xvar, Yvar, [Xvar1|Xvars], [Yvar1|Yvars], (Xvar1 #= Xvar + Xdiff) #/\ (Yvar1 #= Yvar + Ydiff) #/\ Constraints):-
    Xdiff is X1-X,
    Ydiff is Y1-Y,
    specific_shape_constraint(X, Y, Shape, Xvar, Yvar, Xvars, Yvars, Constraints).

%%%%
%
%

% OK, that's the hard bit done (I hope: if it struggles with this, I'm
% giving up)

% For each row in the arrangement lines, need to create that many presents
% of each type, and check that they can all be arranged without overlapping

solve_part1_row([0-Shapes0, 1-Shapes1, 2-Shapes2, 3-Shapes3, 4-Shapes4, 5-Shapes5], [size-[X, Y], num-[N0, N1, N2, N3, N4, N5]], Vars, Zs):-
	fit_n_presents(Shapes0, X, Y, N0, Vars0, Zs0),
	fit_n_presents(Shapes1, X, Y, N1, Vars1, Zs1),
	fit_n_presents(Shapes2, X, Y, N2, Vars2, Zs2),
	fit_n_presents(Shapes3, X, Y, N3, Vars3, Zs3),
	fit_n_presents(Shapes4, X, Y, N4, Vars4, Zs4),
	fit_n_presents(Shapes5, X, Y, N5, Vars5, Zs5),
	flatten([Vars0, Vars1, Vars2, Vars3, Vars4, Vars5], Vars),
	flatten([Zs0, Zs1, Zs2, Zs3, Zs4, Zs5], Zs).



fit_n_presents(_, _, _, 0, [], []).
fit_n_presents(Shapes, X, Y, N1, Vars, Zs):-
    N1>0,
    succ(N, N1),
    fit_n_presents(Shapes, X, Y, N, Vars1, Zs1),
    constrain_shape_set(Shapes, X, Y, Vars2, Zs2),
    append(Vars2, Vars1, Vars),
    append(Zs2, Zs1, Zs).


