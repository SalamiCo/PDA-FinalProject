:- use_module(screen).

puzzle_register(Name, puzzle(Type, Puz)) :-
	atom(Name), atom(Type), nonvar(Puz),
	\+ puzzle(Name, _, _),
	assert(puzzle(Name, Type, Puz)).

puzzle_unregister(Name) :-
	atom(Name),
	retractall(puzzle(Name, _, _)).

puzzle_clear :- retractall(puzzle(_,_,_)).

puzzle_list(L) :- findall(Name, puzzle(Name,_,_), L).

puzzle_load(File, Name).

puzzle_solve(Name, Strategy).



puzzle_usage :-
	writeln('  \033[37;1mpuzzle_load(+\033[4mFile\033[24m, ?\033[4mName\033[24m)\033[21m'),
	writeln('    Loads a puzzle file specified in File, and names is Name if provided.'),
	writeln('    If not provided, a new Name is generated and returned.'),
	nl,
	writeln('  \033[1mpuzzle_solve(+\033[4mName\033[24m, +\033[4mStrat\033[24m, ?\033[4mSolved\033[24m)\033[21m\033[21m'),
	writeln('    Solves the puzzle associated with Name using the specified strategy.'),
	writeln('    The solved puzzle is registered under the given Solved name, if provided.'),
	writeln('    If not provided, Solved returns the name of the solved puzzle.'),
	writeln('    Puzzles already registered under the Solved name are overwritten.'),
	writeln('    The Strat argument may be one of the following strategies:'),
	writeln('    - \033[4mbrute\033[24m for a brute-force or almost brute-force search.'),
	writeln('    - \033[4mclpfd\033[24m for a CLP(FD) based search.'),
	writeln('    - \033[4moptimized\033[24m for a prolog-only, manually optimized search.'),
	nl.

puzzle_help :-
	puzzle_usage.

:- puzzle_clear,
	writeln('\033[1;4;33mPuzzle Solver\033[24;32m v0.1.0\033[21;37m'),
	writeln('> \033[1;4;33mKakoru\033[24m solver \033[32mv0.1.0\033[21;37m'),
	writeln('> \033[1;4;33mStr8ts\033[24m solver \033[32mv0.1.0\033[21;37m'),
	nl,
	writeln('Use \033[94mpuzzle_usage\033[37m to show basic usage'),
	writeln('Use \033[94mpuzzle_help\033[37m to show full help'),
	writeln('\033[m').