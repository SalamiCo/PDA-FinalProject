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

kakuro_print_cell(H) :- var(H), !, write('\033[107;90m[     ]').
kakuro_print_cell(N) :- integer(N), !, N >= 0, N =< 9, !, write('\033[107;90m[\033[30m  '), write(N), write('  \033[90m]').
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

kakuro_lines(K, L) :- 
	kakuro_lines_rowsh(K, L1),
	transpose(K, KT),
	kakuro_lines_rowsv(KT, L2),
	append(L1, L2, L).

kakuro_lines_rowsh([], []).
kakuro_lines_rowsh([R|Rs], L) :- kakuro_lines_rowh(R, L1), kakuro_lines_rowsh(Rs, L2), append(L1,L2,L).

kakuro_lines_rowh([], []).
kakuro_lines_rowh([nothing|Rs], Ls) :- kakuro_lines_rowh(Rs, Ls).
kakuro_lines_rowh([v(_)|Rs], Ls) :- kakuro_lines_rowh(Rs, Ls).
kakuro_lines_rowh([h(N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowh(Rs1, Ls).
kakuro_lines_rowh([vh(_,N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowh(Rs1, Ls).


kakuro_lines_rowsv([], []).
kakuro_lines_rowsv([R|Rs], L) :- kakuro_lines_rowv(R, L1), kakuro_lines_rowsv(Rs, L2), append(L1,L2,L).

kakuro_lines_rowv([], []).
kakuro_lines_rowv([nothing|Rs], Ls) :- kakuro_lines_rowv(Rs, Ls).
kakuro_lines_rowv([h(_)|Rs], Ls) :- kakuro_lines_rowv(Rs, Ls).
kakuro_lines_rowv([v(N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowv(Rs1, Ls).
kakuro_lines_rowv([vh(N,_)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowv(Rs1, Ls).


kakuro_vars([V|Vs], Vs1, [V|Rs]) :- var(V), !, kakuro_vars(Vs, Vs1, Rs).
kakuro_vars(Vs, Vs, []).

% Solve a kakuro puzzle with brute force
kakuro_solve(Puzzle, brute) :- !,
	kakuro_lines(Puzzle, _).

% Solve a kakuro puzzle with clpfd restrictions
kakuro_solve(Puzzle, clpfd) :- !,
	kakuro_lines(Puzzle, Lines),
	kakuro_restr(Lines, _),
	term_variables(Lines,Vars),
	label(Vars),
	writeln(Puzzle).

kakuro_restr([], []).
kakuro_restr([(Sum,Vars)|Ls], [Vars|VarsR]) :-
	Vars ins 1..9,
	all_different(Vars),
	sum(Vars, #=, Sum),
	kakuro_restr(Ls, VarsR).

% Solve a kakuro puzzle with optimized methods
kakuro_solve(_, optimized) :- !.