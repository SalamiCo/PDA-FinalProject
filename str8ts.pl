:- module(str8ts, [str8ts_load/2, str8ts_print/1, str8ts_solve/2]).

:- use_module(library(clpfd)).
:- use_module('util.pl').
:- use_module(library(lists)).

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

check_row([], _).
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

str8ts_rows_nums([], []).
str8ts_rows_nums([C|Cs], [R|Rs]) :- str8ts_row_nums(C, R), str8ts_rows_nums(Cs, Rs).

str8ts_row_nums([], []).
str8ts_row_nums([b|Cs], Rs) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([b(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).
str8ts_row_nums([w(N)|Cs], [N|Rs]) :- str8ts_row_nums(Cs, Rs).

str8ts_lines(S, L) :- 
	str8ts_lines_rows(S, L1),
	transpose(S, ST),
	str8ts_lines_rows(ST, L2),
	append(L1, L2, L).

str8ts_lines_rows([], []).
str8ts_lines_rows([R|Rs], L) :- str8ts_lines_row(R, L1), str8ts_lines_rows(Rs, L2), append(L1, L2, L).

str8ts_lines_row([], []).
str8ts_lines_row([b|Rs], Ls) :- str8ts_lines_row(Rs, Ls).
str8ts_lines_row([b(_)|Rs], Ls) :- str8ts_lines_row(Rs, Ls).
str8ts_lines_row([w(N)|Rs], [[N|Ws]|Ls]) :- str8ts_whites(Rs, Rs1, Ws), str8ts_lines_row(Rs1, Ls).

str8ts_whites([w(N)|Ws], Ws1, [N|Rs]) :- !, str8ts_whites(Ws, Ws1, Rs).
str8ts_whites(Vs, Vs, []).

str8ts_solve(Puzzle, brute) :- str8ts_solve_brute(Puzzle).
str8ts_solve(Puzzle, clpfd) :- str8ts_solve_clpfd(Puzzle).
str8ts_solve(Puzzle, optimized) :- str8ts_solve_optimized(Puzzle).


%% (Almost) BRUTE FORCE %%
str8ts_solve_brute(Rows) :-
	transpose(Rows, Cols),
	merge(Rows, Cols, Merged),
	brute_rows(Merged).

brute_rows([]).
brute_rows([R|Rs]) :-
	length(R, L),
	range(1, L, Vals),

	give_values(R, Vals),
	check_straights_row(R),

	brute_rows(Rs).

give_values(Vs) :- give_values(Vs, [1,2,3,4,5,6,7,8,9]).
give_values([], _).
give_values([w(V)|Vs], Ns) :- take(Ns, V, Ns1), give_values(Vs, Ns1).
give_values([b|Vs], Ns) :- give_values(Vs, Ns).
give_values([b(V)|Vs], Ns) :- take(Ns, V, Ns1), give_values(Vs, Ns1).

check_straights_row(R) :-
	str8ts_lines_row(R, Ns),
	check_straights(Ns).

check_straights([]).
check_straights([S|Ss]) :-
	check_straight(S), check_straights(Ss).

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

clpfd_differents(Rows) :-
	str8ts_rows_nums(Rows, NR),
	maplist(all_different, NR),
	transpose(Rows, Cols),
	str8ts_rows_nums(Cols, NC),
	maplist(all_different, NC).

clpfd_straights(Board) :-
	str8ts_lines(Board, Lines),
	clpfd_straights_lines(Lines).

clpfd_straights_lines([]).
clpfd_straights_lines([L|Ls]) :-
	clpfd_straight(L), clpfd_straights_lines(Ls).

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
	append([Rows, Cols], All),

	opti_convert(All, OAll),
	opti_solve(OAll).

opti_convert([], []).
opti_convert([R|Rs], [C|Cs]) :-
	length(R, L),
	range(1, L, Ns),

	str8ts_row_nums(R, RN),
	takenums(Ns, RN, Nums),

	str8ts_lines_row(R, Lines),

	C=(Nums,Lines),
	opti_convert(Rs, Cs).

takenums(Ns, [], Ns).
takenums(Ns, [V|Vs], Rs) :- var(V), !, takenums(Ns, Vs, Rs).
takenums(Ns, [I|Is], Rs) :- take(Ns, I, Ns1), takenums(Ns1, Is, Rs).

opti_solve([]) :- !.
opti_solve(All) :- 
	opti_select(All, All1, Sel),
	Sel=(Rest, Comps, Comp),

	opti_give(Rest, Rest1, Comp),

	(	Comps=[],  All2=All1;
		Comps\=[], All2=[(Rest1, Comps)|All1]
	),

	opti_check(All2),
	opti_solve(All2).

% Select the comprartment with the least possibilities
opti_select(All, All1, Sel) :- 
	All=[(Nums,S)|All1],
	S=[Comp|Comps],
	Sel=(Nums,Comps,Comp).

% Give values to vars
opti_give(Nums, Rest, Comp) :-
	nums(Comp, CNums), append([Nums, CNums], ANums),
	length(Comp, L), !,
	take_straight(ANums, L, Rest, Str),
	permutation(Str, Comp).

% Check consistency
opti_check([]).
opti_check([(_,Ss)|Ps]) :- 
	append(Ss, Ns),
	opti_check_reps(Ns),
	opti_check_strs(Ss),
	opti_check(Ps).

opti_check_reps(Ns) :- nums(Ns, Rs), opti_check_reps_(Rs).
opti_check_reps_([]).
opti_check_reps_([N|Ns]) :- \+ member(N, Ns), opti_check_reps_(Ns).

opti_check_strs([]).
opti_check_strs([S|Ss]) :-
	opti_check_str(S),
	opti_check_strs(Ss).

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


nums([], []).
nums([V|Ns], Rs) :- var(V), !, nums(Ns, Rs).
nums([N|Ns], [N|Rs]) :- nums(Ns, Rs).

take_straight(Nums, L, Rest, Str) :-
	sort(Nums, SNums), take_straight(SNums, L, [], Rest, Str).

take_straight(Rest, L, Str, Rest, Str) :- length(Str, L).
take_straight([N|Ns], L, Acc, Rest, Str) :- length(Acc, L1), L1 < L,
	(Acc=[A|_], N is A+1; Acc=[]), take_straight(Ns, L, [N|Acc], Rest, Str).
take_straight([N|Ns], L, [], [N|Rest], Str) :-
	length(Ns, L1), L1 >= L, take_straight(Ns, L, [], Rest, Str).

prtls([]).
prtls([X|Xs]) :- writeln(X), prtls(Xs).