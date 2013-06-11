:- module(str8ts, [str8ts_load/2, str8ts_print/1, str8ts_solve/2]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module('util.pl').

%! str8ts_load(+Stream, -Result) is semidet
%
%  Loads a Kakuro puzzle from Stream and unifies it with Result
%
%  @arg Stream Input stream from which the puzzle is loaded
%  @arg Result Loaded puzzle
str8ts_load(Stream, Result) :- 
	read(Stream, Loaded),
	read(Stream, end_of_file),
	str8ts_check_convert(Loaded, Result).

% Checks the puzzle for correctness and converts it to the internal format
str8ts_check_convert(Loaded, Result) :- 
	length(Loaded, L), L < 10,
	check(Loaded, L),
	convert(Loaded, Result).

% Check a str8ts puzzle
check([], _).
check([R|Rs], L) :-	length(R, L), check_row(R, L), check(Rs, L).

% Check a row
check_row([], _).
check_row([C|Cs], L) :- check_cell(C, L), check_row(Cs, L).

% Check a cell
check_cell(w, _).
check_cell(b, _).
check_cell(w(N), L) :- integer(N), N>=1, N=<L.
check_cell(b(N), L) :- integer(N), N>=1, N=<L.

% Convert a str8ts puzzle
convert([], []).
convert([R|Rs], [RC|RCs]) :- convert_row(R, RC), convert(Rs, RCs).

% Convert a row
convert_row([], []).
convert_row([C|Cs], [CC|CCs]) :- convert_cell(C, CC), convert_row(Cs, CCs).

%convert a cell
convert_cell(b, b).
convert_cell(b(N), b(N)).
convert_cell(w, w(_)).
convert_cell(w(N), w(N)).

%! str8ts_print(+Puzzle)
%
%  Prints the Puzzle on the screen using coloured output
%
%  @arg Puzzle The puzzle to print
str8ts_print([]).
str8ts_print([R|Rs]) :- str8ts_print_row(R), str8ts_print(Rs).

% Print a row
str8ts_print_row([]) :- writeln('\033[m').
str8ts_print_row([C|Cs]) :- str8ts_print_cell(C), str8ts_print_row(Cs).

% Print a cell
str8ts_print_cell(w(N)) :- var(N), !, write('\033[107;90m[ ]').
str8ts_print_cell(w(N)) :- integer(N), !, N>=1, N=<9, write('\033[107;90m[\033[30m'), write(N), write('\033[90m]').
str8ts_print_cell(b) :- !, write('\033[40;90m[ ]').
str8ts_print_cell(b(N)) :- !, write('\033[40;90m[\033[97m'), write(N), write('\033[90m]').

% Retrieve a list with the variables and integers of a str8ts puzzle
str8ts_rows_nums([], []).
str8ts_rows_nums([C|Cs], [R|Rs]) :- str8ts_row_nums(C, R), str8ts_rows_nums(Cs, Rs).

% Retrieve a list with the variables and integer of a str8ts row
str8ts_row_nums([], []).
str8ts_row_nums([b|Cs], Rs) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([b(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([w(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).

%! str8ts_lines(+Puzzle:list, -Lines:list) is semidet
%
%  Gets a list of the Lines of a str8ts puzzle. 
str8ts_lines(S, L) :- 
	str8ts_lines_rows(S, L1),
	transpose(S, ST),
	str8ts_lines_rows(ST, L2),
	merge(L1, L2, L).

str8ts_lines_rows([], []).
str8ts_lines_rows([R|Rs], L) :- str8ts_lines_row(R, L1), str8ts_lines_rows(Rs, L2), append(L1, L2, L).

str8ts_lines_row([], []).
str8ts_lines_row([b|Rs], Ls) :- str8ts_lines_row(Rs, Ls).
str8ts_lines_row([b(_)|Rs], Ls) :- str8ts_lines_row(Rs, Ls).
str8ts_lines_row([w(N)|Rs], [[N|Ws]|Ls]) :- str8ts_whites(Rs, Rs1, Ws), str8ts_lines_row(Rs1, Ls).

str8ts_whites([w(N)|Ws], Ws1, [N|Rs]) :- !, str8ts_whites(Ws, Ws1, Rs).
str8ts_whites(Vs, Vs, []).


%! str8ts_solve(?Puzzle, +Strategy) is nondet
%
%  Solves the given str8ts using the given strategy.
%
%  @arg Puzzle Puzzle to solve
%  @arg Strategy Strategy to solve the puzzle with
str8ts_solve(Puzzle, brute) :- str8ts_solve_brute(Puzzle).
str8ts_solve(Puzzle, clpfd) :- str8ts_solve_clpfd(Puzzle).
str8ts_solve(Puzzle, optimized) :- str8ts_solve_optimized(Puzzle).


%% (Almost) BRUTE FORCE %%
str8ts_solve_brute(Rows) :-
	transpose(Rows, Cols),
	merge(Rows, Cols, Merged),
	brute_rows(Merged). % Apply the same strategy to every row and column

% Gives values to a row, then checks them, then continues
brute_rows([]).
brute_rows([R|Rs]) :-
	length(R, L),
	range(1, L, Vals),

	give_values(R, Vals),
	check_straights_row(R),

	brute_rows(Rs).

% Gives values to a row. This ensures there are no repetitions, even with black cells
give_values([], _).
give_values([w(V)|Vs], Ns) :- take(Ns, V, Ns1), give_values(Vs, Ns1).
give_values([b|Vs], Ns) :- give_values(Vs, Ns).
give_values([b(V)|Vs], Ns) :- take(Ns, V, Ns1), give_values(Vs, Ns1).

% Check that a row or column has only straights
check_straights_row(R) :-
	str8ts_lines_row(R, Ns),
	check_straights(Ns).

% Check for straights
check_straights([]).
check_straights([S|Ss]) :-
	check_straight(S), check_straights(Ss).

% Checksa single straight
check_straight(Ns) :-
	length(Ns, L),
	max_list(Ns, Max),
	min_list(Ns, Min),
	L - 1 =:= Max - Min.


%% CLPFD %%
str8ts_solve_clpfd(Puzzle) :- !,
	length(Puzzle, L),
	term_variables(Puzzle, Vars),
	Vars ins 1..L,

	clpfd_differents(Puzzle),
	clpfd_straights(Puzzle),

	label(Vars).

% Sets restrictions so each row and column has only different numbers
clpfd_differents(Rows) :-
	str8ts_rows_nums(Rows, NR),
	maplist(all_different, NR),
	transpose(Rows, Cols),
	str8ts_rows_nums(Cols, NC),
	maplist(all_different, NC).

% Sets restrictions so each row and column contains only straights
clpfd_straights(Board) :-
	str8ts_lines(Board, Lines),
	clpfd_straights_lines(Lines).

clpfd_straights_lines([]).
clpfd_straights_lines([L|Ls]) :-
	clpfd_straight(L), clpfd_straights_lines(Ls).

% Restricts a single straight
clpfd_straight(Line) :-
	length(Line, L),
	clpfd_max(Line, Max),
	clpfd_min(Line, Min),
	L - 1 #= Max - Min.

clpfd_max([X], X) :- !.
clpfd_max([X|Xs], Y) :- clpfd_max(Xs, Z), Y #= max(X, Z).

clpfd_min([X], X) :- !.
clpfd_min([X|Xs], Y) :- clpfd_min(Xs, Z), Y #= min(X, Z).


%% OPTIMIZED %%
str8ts_solve_optimized(Rows) :-
	transpose(Rows, Cols),
	append(Rows, Cols, All),

	opti_convert(All, CAll),
	opti_solve(CAll).

% Converts a list of list of straights in a more suitable structure.
% In particular, this returns a list af pairs. Those pairs contain:
% - A list of numbers not seen on that row/column black cells
% - A list of compartments (lists of vars/numbers)
opti_convert([], []).
opti_convert([R|Rs], [C|Cs]) :-
	length(R, L),
	range(1, L, Ns),

	str8ts_blacks(R, Bs),
	subtract(Ns, Bs, Nums),

	str8ts_lines_row(R, Lines),

	C=(Nums,Lines),
	opti_convert(Rs, Cs).

% Returns the numbers in black cells from a row
str8ts_blacks([], []).
str8ts_blacks([b(N)|Vs], [N|Rs]) :- !, str8ts_blacks(Vs, Rs).
str8ts_blacks([_|Vs], Rs) :- str8ts_blacks(Vs, Rs).

% Solves the puzzle!
opti_solve([]) :- !.
opti_solve(All) :- 
	opti_select(All, All1, Sel),	% Select a compartment
	Sel=(Rest, Comps, Comp),

	opti_give(Rest, Rest1, Comp),	% Give values to the selected compartment

	(	Comps=[],  All2=All1;
		Comps\=[], All2=[(Rest1, Comps)|All1]
	),

	opti_check(All2),	% Check for consistency
	opti_solve(All2).	% Keep going

% Select the compartment with the least possibilities
opti_select(All, All1, Sel) :- 
	All=[(Nums,S)|All1],
	S=[Comp|Comps],
	Sel=(Nums,Comps,Comp).

% Calculates the cost of a compartment
opti_cost(Rest, Comp, Cost) :-
	length(Comp, L),
	copy_term(Comp, Sets),
	findall('', take_straight(Rest, L, _, Sets), Strs),
	length(Strs, S),

	nums(Comp, Ns), length(Ns, N),
	factdiv(L, N, P),
	Cost is P*S.

% Give values to vars
opti_give(Nums, Rest, Comp) :-
	length(Comp, L), !,
	take_straight(Nums, L, Rest, Str),
	permutation(Str, Comp).

% Check consistency
opti_check([]).
opti_check([(_,Ss)|Ps]) :- 
	append(Ss, Ns),
	opti_check_reps(Ns),
	opti_check_strs(Ss),
	opti_check(Ps).

% Check for repetitions
opti_check_reps(Ns) :- nums(Ns, Rs), opti_check_reps_(Rs).
opti_check_reps_([]).
opti_check_reps_([N|Ns]) :- \+ member(N, Ns), opti_check_reps_(Ns).

% Check for partial straights -- Things that might be straight if given suitable values to variables
opti_check_strs([]).
opti_check_strs([S|Ss]) :-
	opti_check_str(S),
	opti_check_strs(Ss).

% Checks a partial straight
opti_check_str(Ns) :- 
	length(Ns, L),
	nums(Ns, Nums),
	(	Nums=[]
	;
		max_list(Nums, Max),
		min_list(Nums, Min),
		L1 is L-1,
		MM is Max-Min,
		MM =< L1
	).

% Returns all numbers from a list of vars and nums
nums([], []).
nums([V|Ns], Rs) :- var(V), !, nums(Ns, Rs).
nums([N|Ns], [N|Rs]) :- nums(Ns, Rs).

% Unifies Str with a valid straight of length L taken from values in nums, and removes that numbers in Rest
take_straight(Nums, L, Rest, Str) :-
	sort(Nums, SNums), take_straight(SNums, L, [], Rest, Str).

take_straight(Rest, L, Str, Rest, Str) :- length(Str, L).
take_straight([N|Ns], L, Acc, Rest, Str) :- length(Acc, L1), L1 < L,
	(Acc=[A|_], N is A+1; Acc=[]), take_straight(Ns, L, [N|Acc], Rest, Str).
take_straight([N|Ns], L, [], [N|Rest], Str) :-
	length(Ns, L1), L1 >= L, take_straight(Ns, L, [], Rest, Str).
