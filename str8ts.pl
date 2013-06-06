:- module(str8ts, [str8ts_load/2, str8ts_print/1, str8ts_solve/2]).

:- use_module(library(clpfd)).
:- use_module('util.pl').

% Loading %
str8ts_load(Stream, Result) :- 
	read(Stream, Loaded),
	read(Stream, end_of_file),
	str8ts_check_convert(Loaded, Result).

str8ts_check_convert(Loaded, Result) :- 
	length(Loaded, L), L < 10,
	check(Loaded, L),
	convert(Loaded, Result).

check([], _).
check([R|Rs], L) :-	length(R, L), check_row(R, L), check(Rs, L).

check_row([], L).
check_row([C|Cs], L) :- check_cell(C, L), check_row(Cs, L).

check_cell(w, _).
check_cell(b, _).
check_cell(w(N), L) :- integer(N), N>=1, N=<L.
check_cell(b(N), L) :- integer(N), N>=1, N=<L.

convert([], []).
convert([R|Rs], [RC|RCs]) :- convert_row(R, RC), convert(Rs, RCs).

convert_row([], []).
convert_row([C|Cs], [CC|CCs]) :- convert_cell(C, CC), convert_row(Cs, CCs).

convert_cell(b, b).
convert_cell(b(N), b(N)).
convert_cell(w, w(_)).
convert_cell(w(N), w(N)).

% Printing %
str8ts_print([]).
str8ts_print([R|Rs]) :- str8ts_print_row(R), str8ts_print(Rs).

str8ts_print_row([]) :- writeln('\033[m').
str8ts_print_row([C|Cs]) :- str8ts_print_cell(C), str8ts_print_row(Cs).

str8ts_print_cell(w(N)) :- var(N), !, write('\033[107;90m[ ]').
str8ts_print_cell(w(N)) :- integer(N), !, N>=1, N=<9, write('\033[107;90m[\033[30m'), write(N), write('\033[90m]').
str8ts_print_cell(b) :- !, write('\033[40;90m[ ]').
str8ts_print_cell(b(N)) :- !, write('\033[40;90m[\033[97m'), write(N), write('\033[90m]').

%%%%%%%%%%%
% Solving %
str8ts_solve(Puzzle, brute) :- str8ts_solve_brute(Puzzle).
str8ts_solve(Puzzle, clpfd) :- str8ts_solve_clpfd(Puzzle).
str8ts_solve(Puzzle, optimized) :- str8ts_solve_optimized(Puzzle).

str8ts_solve_clpfd(Puzzle) :-
	length(Puezzle, L),
	term_variables(Puzzle, Vars),
	Vars ins 1..L,

	clpfd_differents(Puzzle),
	clpfd_straights(Puzzle),

	label(Vars).

clpfd_differents(Rows) :-
	str8ts_rows_nums(Rows, NR),
	maplist(all_different, NR),
	transpose(Rows, Cols),
	str8ts_rows_nums(Cols, NC),
	maplist(all_different, NC.

clpfd_straights(Board) :-
	str8ts_lines(Board, Lines),
	clpfd_straights_lines(Lines).

clpfd_straights_lines([L|Ls]) :-
	clpfd_straight(L), clpfd_straights_lines(Ls).

clpfd_straight(Line) :-
	length(Line, L),
	clpfd_max(Line, Max),
	clpfd_min(Line, Min),
	L + 1 #= Max - Min.

clpfd_max([X], X).
clpfd_max([X|Xs], Y) :- clpfd_max(Xs, Z), Y #= max(X, Y).

clpfd_min([X], X).
clpfd_min([X|Xs], Y) :- clpfd_min(Xs, Z), Y #= min(X, Y).