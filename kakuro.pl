:- module(kakuro, [kakuro_load/2, kakuro_print/1, kakuro_solve/2]).

:- use_module(library(clpfd)).

%! kakuro_load(+Stream, -Result)
%  Loads a Kakuro puzzle from Stream and unifies it with Result
kakuro_load(Stream, Result) :- 
	read_pending_input(Stream, Codes, []),
	kakdcg_main(Result, Codes, []).

%! kakuro_print(+Puzzle)
%  Prints the Puzzle on the screen using coloured output
kakuro_print([]).
kakuro_print([R|Rs]) :- kakuro_print_row(R), kakuro_print(Rs).

kakuro_print_row([]) :- writeln('\033[m').
kakuro_print_row([C|Cs]) :- kakuro_print_cell(C), kakuro_print_row(Cs).

kakuro_print_cell(H) :- var(H), !, write('\033[107m       ').
kakuro_print_cell(N) :- integer(N), !, N >= 0, N =< 9, !, write('\033[107;30m       ').
kakuro_print_cell(nothing) :- !, write('\033[40;90m[     ]').
kakuro_print_cell(v(V)) :- !, write('\033[40;90m[\033[97m'), kakuro_print_hint(V), write('\\  \033[90m]').
kakuro_print_cell(h(H)) :- !, write('\033[40;90m[\033[97m  \\'), kakuro_print_hint(H), write('\033[90m]').
kakuro_print_cell(vh(V,H)) :- !,
	write('\033[40;90m[\033[97m'), kakuro_print_hint(V), write('\\'), kakuro_print_hint(H), write('\033[90m]').

kakuro_print_hint(N) :- N < 10, !, write(' '), write(N).
kakuro_print_hint(N) :- write(N).

%%% GRAMMAR %%%
kakdcg_main(B) --> "kakuro\n", kakdcg_board(B).

kakdcg_board([R|Rs]) --> kakdcg_row(R), !, kakdcg_board(Rs).
kakdcg_board([]) --> [].

kakdcg_row([C|Cs]) --> kakdcg_cell(C), kakdcg_row(Cs).
kakdcg_row([]) --> "\n".

kakdcg_cell(C) --> "[", kakdcg_cell_int(C), "]".

kakdcg_cell_int(_) --> "_____", !.
kakdcg_cell_int(nothing) --> "#####", !.
kakdcg_cell_int(h(H)) --> "##\\", kakdcg_hint(H), !.
kakdcg_cell_int(v(V)) --> kakdcg_hint(V), "\\##",!.
kakdcg_cell_int(vh(V,H)) --> kakdcg_hint(V), "\\", kakdcg_hint(H), !.
kakdcg_cell_int(error) --> [_,_,_,_,_].

kakdcg_hint(N) --> [D1,D2],
	{code_type(D1, digit), code_type(D2, digit), number_codes(N, [D1,D2])}.

% Solve a kakuro puzzle with brute force
kakuro_solve(Puzzle, brute) :- !.

% Solve a kakuro puzzle with clpfd restrictions
kakuro_solve(Puzzle, clpfd) :- !.

% Solve a kakuro puzzle with optimized methods
kakuro_solve(Puzzle, optimized) :- !.