:- use_module('util.pl').
:- use_module('kakuro.pl').
:- use_module('str8ts.pl').

puzzle_register(Name, puzzle(Type, Puz)) :-
	atom(Name), atom(Type), nonvar(Puz),
	\+ puzzle(Name, _, _),
	assert(puzzle(Name, Type, Puz)).

puzzle_unregister(Name) :-
	atom(Name),
	retractall(puzzle(Name, _, _)).

puzzle_clear :- retractall(puzzle(_,_,_)).

puzzle_list(L) :- findall(Name, puzzle(Name,_,_), L).

puzzle_load(File, Name) :- open(File, read, Stream, []),
	(	kakuro_load(Stream, Puz), !, Type=kakuro;
		str8ts_load(Stream, Puz), !, Type=str8ts;
		close(Stream), !, fail
	),
	puzzle_unregister(Name),
	puzzle_register(Name, puzzle(Type, Puz)),
	close(Stream).

puzzle_solve(Name, Strategy, Solved) :-
	puzzle(Name, kakuro, Puzzle),
	copy_term(Puzzle, SPuzz),
	kakuro_solve(SPuzz, Strategy),
	puzzle_unregister(Solved),
	puzzle_register(Solved, puzzle(kakuro, SPuzz)).
puzzle_solve(Name, Strategy, Solved) :-
	puzzle(Name, str8ts, Puzzle), !, str8ts_solve(Puzzle, Strategy).

puzzle_print(Name) :- puzzle(Name, kakuro, Puzzle), !, kakuro_print(Puzzle).
puzzle_print(Name) :- puzzle(Name, str8ts, Puzzle), !, str8ts_print(Puzzle).


puzzle_load_and_solve(File, Strategy) :-
	P='_puz', S='_solved',
	write('\033[mLoading puzzle on \033[4m'), write(File), writeln('\033[24m...'),
	timed(puzzle_load(File, P), T1),
	write('\033[mLoaded a \033[1m'), write('\033[21m puzzle in '), write(T1), writeln('s'),
	puzzle_print(P),
	write('Solving the puzzle using the \033[1m'), write(Strategy), writeln('\033[21m strategy'),
	timed(puzzle_solve(P, Strategy, S), T2),
	write('\033[mSolved puzzle in '), write(T2), writeln('s'),
	puzzle_print(S),
	puzzle_unregister(P),
	puzzle_unregister(S).

puzzle_usage :-
	writeln('\033[;1mpuzzle_load_and_solve(+\033[4mFile\033[24m, ?\033[4mStrategy\033[24m)\033[21m'),
	writeln('  Loads a puzzle File and solves it using the given Strategy.'),
	writeln('  This is the most simple way of loading and solving a puzzle.'),
	writeln('  This predicate performs the following steps:'),
	writeln('  - Loads the specified File into a puzzle with a default name.'),
	writeln('  - Prints the original, unsolved version of the puzzle.'),
	writeln('  - Solves the puzzle using the specified Strategy.'),
	writeln('  - Prints the solved puzzle and shows the time it took.'),
	writeln('  - Unregisters both the solved and the unsolved puzzles.'),
	writeln('  For more information about the available strategies, see the full help'),
	writeln('  on the \033[4mpuzzle_solve\033[24m predicate.'),
	nl.

puzzle_help :-
	puzzle_usage,
	writeln('\033[1mpuzzle_load(+\033[4mFile\033[24m, ?\033[4mName\033[24m)\033[21m'),
	writeln('  Loads a puzzle file specified in File, and names is Name if provided.'),
	writeln('  If not provided, a new Name is generated and returned.'),
	nl,
	writeln('\033[1mpuzzle_solve(+\033[4mName\033[24m, +\033[4mStrat\033[24m, ?\033[4mSolved\033[24m)\033[21m\033[21m'),
	writeln('  Solves the puzzle associated with Name using the specified strategy.'),
	writeln('  The solved puzzle is registered under the given Solved name, if provided.'),
	writeln('  If not provided, Solved returns the name of the solved puzzle.'),
	writeln('  Puzzles already registered under the Solved name are overwritten.'),
	writeln('  The Strat argument may be one of the following strategies:'),
	writeln('  - \033[4mbrute\033[24m for a brute-force or almost brute-force search.'),
	writeln('  - \033[4mclpfd\033[24m for a CLP(FD) based search.'),
	writeln('  - \033[4moptimized\033[24m for a prolog-only, manually optimized search.'),
	nl,
	writeln('\033[;1mpuzzle_print(+\033[4mName\033[24m)\033[21m'),
	writeln('  Prints the puzzle associated with Name on the screen.'),
	writeln('  This may print both solved or unsolved puzzles of any kind.'),
	nl.

:- puzzle_clear,
	writeln('\033[1;4;33mPuzzle Solver\033[24;32m v0.1.0\033[21;37m'),
	nl,
	writeln('Use \033[94mpuzzle_usage\033[37m to show basic usage'),
	writeln('Use \033[94mpuzzle_help\033[37m to show full help'),
	writeln('\033[m').