:- module(str8ts, [str8ts_load/2, str8ts_print/1, str8ts_solve/2]).

:- use_module('util.pl').

% Lectura de fichero %
str8ts_load(Stream, Result) :- 
	read(Stream, Loaded),
	read(Stream, end_of_file),
	str8ts_check_convert(Loaded, Result).

str8ts_check_convert(Loaded, Result) :- 
	length(Loaded, L),
	check(Loaded, L),
	convert(Loaded, Result).

check([], _).
check([R|Rs], L) :-	length(R, L), check_row(R, L), check(Rs, L).

check_row([], L).
check_row([C|Cs], L) :- check_cell(C, L), check_row(Cs, L).

check_cell(w, _).
check_cell(b, _).
check_cell(w(N), L) :- integer(N), N>=0, N=<L.
check_cell(b(N), L) :- integer(N), N>=0, N=<L.

convert([], []).
convert([R|Rs], [RC|RCs]) :- convert_row(R, RC), convert(Rs, RCs).

convert_row([], []).
convert_row([C|Cs], [CC|CCs]) :- convert_cell(C, CC), convert_row(Cs, CCs).

convert_cell(b, b).
convert_cell(b(N), b(N)).
convert_cell(w, w(_)).
convert_cell(w(N), w(N)).

%%%
str8ts_print([]).
str8ts_print([R|Rs]) :- str8ts_print_row(R), str8ts_print(Rs).

str8ts_print_row([]) :- writeln('\033[m').
str8ts_print_row([C|Cs]) :- str8ts_print_cell(C), str8ts_print_row(Cs).

str8ts_print_cell(w(N)) :- var(N), !, write('\033[107;90m[ ]').
str8ts_print_cell(w(N)) :- integer(N), !, N>=1, N=<9, write('\033[107;90m[\033[30m'), write(N), write('\033[90m]').
str8ts_print_cell(b) :- !, write('\033[40;90m[ ]').
str8ts_print_cell(b(N)) :- !, write('\033[40;90m[\033[97m'), write(N), write('\033[90m]').

str8ts_row_nums([], []).
str8ts_row_nums([b|Cs], Rs) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([b(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([w(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).

str8ts_solve(Puzzle, brute) :- str8ts_solve_brute(Puzzle).
str8ts_solve(Puzzle, clpfd) :- str8ts_solve_clpfd(Puzzle).
str8ts_solve(Puzzle, optimized) :- str8ts_solve_optimized(Puzzle).