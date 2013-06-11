:- module(kakuro, [kakuro_load/2, kakuro_print/1, kakuro_solve/2]).

:- use_module(library(clpfd)).
:- use_module(library(lists)).

%! kakuro_load(+Stream, -Result) is semidet
%
%  Loads a Kakuro puzzle from Stream and unifies it with Result
%
%  @arg Stream Input stream from which the puzzle is loaded
%  @arg Result Loaded puzzle
kakuro_load(Stream, Result) :- 
	read_pending_input(Stream, Codes, []),
	kakdcg_main(Result, Codes, []).

%! kakuro_print(+Puzzle)
%
%  Prints the Puzzle on the screen using coloured output
%
%  @arg Puzzle The puzzle to print
kakuro_print([]).
kakuro_print([R|Rs]) :- kakuro_print_row(R), kakuro_print(Rs).

% Prints a row
kakuro_print_row([]) :- writeln('\033[m').
kakuro_print_row([C|Cs]) :- kakuro_print_cell(C), kakuro_print_row(Cs).

% Prints a cell
kakuro_print_cell(H) :- var(H), !, write('\033[107;90m[     ]').
kakuro_print_cell(N) :- integer(N), !, N >= 0, N =< 9, !, write('\033[107;90m[\033[30m  '), write(N), write('  \033[90m]').
kakuro_print_cell(nothing) :- !, write('\033[40;90m[     ]').
kakuro_print_cell(v(V)) :- !, write('\033[40;90m[\033[97m'), kakuro_print_hint(V), write('\\  \033[90m]').
kakuro_print_cell(h(H)) :- !, write('\033[40;90m[\033[97m  \\'), kakuro_print_hint(H), write('\033[90m]').
kakuro_print_cell(vh(V,H)) :- !,
	write('\033[40;90m[\033[97m'), kakuro_print_hint(V), write('\\'), kakuro_print_hint(H), write('\033[90m]').

% Prints a hint -- makes sure each hint is printed with 2 digits
kakuro_print_hint(N) :- N < 10, !, write(' '), write(N).
kakuro_print_hint(N) :- write(N).

%%% GRAMMAR %%%
kakdcg_main(B) --> kakdcg_board(B).

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

%! kakuro_lines(+Kakuro:list, -Lines:list) is semidet
%
%  Extracts all "lines" from a kakuro puzzle, making no distinction betweenrows and columns.
%  A line is a list of consecutive white cells, along with their specific hint.
%  All variables or already filled hints are returned as they are, so multiple lines might have the same variable,
%  making the process of resolving the puzzle really easy,
%
%  @arg Kakuro The puzzle to extract lines from
%  @arg Lines The extracted lines
kakuro_lines(K, L) :- 
	kakuro_lines_rowsh(K, L1),
	transpose(K, KT),
	kakuro_lines_rowsv(KT, L2),
	merge(L1, L2, L).

% Extracts lines from rows
kakuro_lines_rowsh([], []).
kakuro_lines_rowsh([R|Rs], L) :- kakuro_lines_rowh(R, L1), kakuro_lines_rowsh(Rs, L2), append(L1,L2,L).

% Extracts lines from a single row
kakuro_lines_rowh([], []).
kakuro_lines_rowh([nothing|Rs], Ls) :- kakuro_lines_rowh(Rs, Ls).
kakuro_lines_rowh([v(_)|Rs], Ls) :- kakuro_lines_rowh(Rs, Ls).
kakuro_lines_rowh([h(N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowh(Rs1, Ls).
kakuro_lines_rowh([vh(_,N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowh(Rs1, Ls).

% Extracts lines from columns 
kakuro_lines_rowsv([], []).
kakuro_lines_rowsv([R|Rs], L) :- kakuro_lines_rowv(R, L1), kakuro_lines_rowsv(Rs, L2), append(L1,L2,L).

% Extracts lines from a single column
kakuro_lines_rowv([], []).
kakuro_lines_rowv([nothing|Rs], Ls) :- kakuro_lines_rowv(Rs, Ls).
kakuro_lines_rowv([h(_)|Rs], Ls) :- kakuro_lines_rowv(Rs, Ls).
kakuro_lines_rowv([v(N)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowv(Rs1, Ls).
kakuro_lines_rowv([vh(N,_)|Rs], [(N,L)|Ls]) :- kakuro_vars(Rs, Rs1, L), kakuro_lines_rowv(Rs1, Ls).

% Extracts consecutive variables from a row/column
kakuro_vars([V|Vs], Vs1, [V|Rs]) :- var(V), !, kakuro_vars(Vs, Vs1, Rs).
kakuro_vars(Vs, Vs, []).


%! kakuro_solve(?Puzzle, +Strategy) is nondet
%
%  Solves the given kakuro using the given strategy.
%
%  @arg Puzzle Puzzle to solve
%  @arg Strategy Strategy to solve the puzzle with
kakuro_solve(Puzzle, brute) :- kakuro_solve_brute(Puzzle).
kakuro_solve(Puzzle, clpfd) :- kakuro_solve_clpfd(Puzzle).
kakuro_solve(Puzzle, optimized) :- kakuro_solve_optimized(Puzzle).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solve a kakuro puzzle with brute force %
kakuro_solve_brute(Puzzle) :- !,
	kakuro_lines(Puzzle, Lines),	% Obtains the lines
	give_and_check_values(Lines).	% Gives/checks values to every line

% Gives values to a line, then checks their validity
give_and_check_values([]).
give_and_check_values([(S, Ns)|Ls]) :-
	give_values(Ns),	% Gives values
	check_sum(S, Ns),	% Checks that they sum whatthey should
	give_and_check_values(Ls).

% Gives values to a line, ensuring they are all different.
give_values(Vs) :- give_values(Vs, [1,2,3,4,5,6,7,8,9]).
give_values([], _).
give_values([V|Vs], Ns) :- take(Ns, V, Ns1), give_values(Vs, Ns1).

% Checks that a line sums the given value
check_sum(S, Ns) :- check_sum(S, Ns, 0).
check_sum(S, [], S).
check_sum(S, [N|Ns], Acc) :- Acc =< S, Acc1 is Acc + N, check_sum(S, Ns, Acc1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solve a kakuro puzzle with clpfd restrictions %
kakuro_solve_clpfd(Puzzle) :- !,
	kakuro_lines(Puzzle, Lines),
	kakuro_restr(Lines, _),
	term_variables(Lines,Vars),
	label(Vars).

kakuro_restr([], []).
kakuro_restr([(Sum,Vars)|Ls], [Vars|VarsR]) :-
	Vars ins 1..9,
	all_different(Vars),
	sum(Vars, #=, Sum),
	kakuro_restr(Ls, VarsR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Solve a kakuro puzzle with optimized methods %
kakuro_solve_optimized(Puzzle) :- 
	kakuro_lines(Puzzle, Lines),
	kakuro_combs(Lines), !,
	kakuro_opti(Lines).

kakuro_opti([]) :- !.
kakuro_opti(Ls) :- 
	opti_select(Ls, (S,Ns), Ls1),

	length(Ns,L), !,
	kcomb(S, L, Comb),
	permutation(Comb, Ns),
	
	opti_check(Ls1), 
	kakuro_opti(Ls1).

opti_check([]).
opti_check([L|Ls]) :- opti_check_one(L), opti_check(Ls).

opti_check_one((S, Vs)) :- !, sort(Vs, VsS), reverse(VsS, VsSR), opti_check_one(S, [], [1,2,3,4,5,6,7,8,9], [9,8,7,6,5,4,3,2,1], 0, 0, VsSR).
opti_check_one(S, _, _, _, H, L, []) :- !, H >= S, L =< S.
opti_check_one(S, Rs, [U|Us], [D|Ds], H, L, [V|Vs]) :- var(V), !,
	H1 is H+D, L1 is L+U, opti_check_one(S, Rs, Us, Ds, H1, L1, Vs).
opti_check_one(S, Rs, Hs, Ls, H, L, [N|Vs]) :- !,
	\+ member(N, Rs),
	H1 is H+N, L1 is L+N,
	take(Hs, N, Hs1), take(Ls, N, Ls1),
	opti_check_one(S, [N|Rs], Hs1, Ls1, H1, L1, Vs).

opti_select(L, E, LR) :- !, opti_select(L, [], inf, _, _, _, E, LR).
opti_select([], _, _, E, L1, L2, E, LR) :- !, append(L1, L2, LR).
opti_select([L|Ls], R, M, _, _, _, E, LR) :- opti_cost(L, C), (M=inf; C<M), !,
	opti_select(Ls, [L|R], C, L, Ls, R, E, LR).
opti_select([L|Ls], R, M, ME, L1, L2, E, LR) :- !,
	opti_select(Ls, [L|R], M, ME, L1, L2, E, LR).

opti_cost((S, Vs), C) :- !, length(Vs, L),
	term_variables(Vs, AVs), length(AVs, LA),
	factdiv(L,LA, F),
	findall(0, kcomb(S, L, _), Q), length(Q,W),
	C is F*W.

factdiv(0, _, 1) :- !.
factdiv(_, 0, 1) :- !.
factdiv(X, Y, Z) :- X1 is X-1, Y1 is Y-1, factdiv(X1, Y1, Z1), Z is Z1*X.

kakuro_combs([]).
kakuro_combs([(S, Ns)|Ls]) :- kakuro_comb(S, Ns), kakuro_combs(Ls).

kakuro_comb(S, Ns) :- length(Ns, L),
	retractall(kcomb(S, L, _)),
	kakuro_comb(S, L, Comb),
	\+ kcomb(S, L, Comb),
	assert(kcomb(S, L, Comb)),
	fail.
kakuro_comb(_,_).

kakuro_comb(Sum, Length, Comb) :- kakuro_comb(Sum, Length, [9,8,7,6,5,4,3,2,1], 0, 0, [], Comb).

kakuro_comb(S, L, _, S, L, C, C).
kakuro_comb(S, L, [N|Ns], SC, LC, NC, C) :- SC1 is SC+N, LC1 is LC+1, kakuro_comb(S, L, Ns, SC1, LC1, [N|NC], C).
kakuro_comb(S, L, [_|Ns], SC, LC, NC, C) :- kakuro_comb(S, L, Ns, SC, LC, NC, C).