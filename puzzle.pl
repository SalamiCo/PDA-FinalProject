:- use_module('util.pl').
:- use_module('kakuro.pl').
:- use_module('str8ts.pl').

%! puzzle_load(+File:atom, -Puzzle:term) is semidet
%
%  Loads a puzzle from a file and returns it in Puzzle.
%  The puzzle file starts with the name of the puzzle, a full stop, and an EOL.
%  The restof the file must follow the specific format of the puzzle, defined in the puzzle module.
%
%  @arg File Name of the file to load
%  @arg Puzzle Puzzle loaded from the specified file
puzzle_load(File, (Type,Puz)) :- open(File, read, Stream, []),
	read(Stream, Type),
	(	Type=kakuro, !, kakuro_load(Stream, Puz);
		Type=str8ts, !, str8ts_load(Stream, Puz);
		close(Stream), !, fail
	),
	close(Stream).

%! puzzle_solve(+Puzzle:term, +Strategy:atom, -Solved:term) is nondet
%
%  Resolves the given Puzzle using the specified Strategy and unifies the result in Solved.
%
%  The Stretegy might be one of:
%  + brute: Solves the puzzle using an (almost) brute-force approach.
%  + clpfd: Solves the puzzle using CLP(FD) restrictions.
%  + optimized: Solves the puzzle using an optimized algorithm that drastically reduces computation choices. 
%
%  @arg Puzzle The puzzle to be solved
%  @arg Strategy The strategy to solve the puzzle with
%  @arg Solved The solved puzzle
puzzle_solve((Type, Puzzle), Strategy, (Type, Solved)) :-
	copy_term(Puzzle, Solved), 
	(	Type=kakuro, !, kakuro_solve(Solved, Strategy);
		Type=str8ts, !, str8ts_solve(Solved, Strategy);
		!, fail
	).

%! puzzle_print(+Puzzle:term) is semidet
%  
%  Prints a puzzle using colored text to represent what would be written on paper.
%
%  @arg Puzzle The puzle to be printed
puzzle_print((kakuro, Puzzle)) :- kakuro_print(Puzzle).
puzzle_print((str8ts, Puzzle)) :- str8ts_print(Puzzle).

puzzle_load_and_solve(File, Strategy) :-
	write('\033[mLoading puzzle on \033[4m'), write(File), writeln('\033[24m...'),
	timed(puzzle_load(File, P), T1),
	P=(Type, _),
	write('\033[mLoaded a \033[1m'), write(Type), write('\033[21m puzzle in '), write(T1), writeln('s'),
	puzzle_print(P),
	write('Solving the puzzle using the \033[1m'), write(Strategy), writeln('\033[21m strategy'),
	timed(puzzle_solve(P, Strategy, S), T2),
	write('\033[mSolved puzzle in '), write(T2), writeln('s'),
	puzzle_print(S).

%! puzzle_help is det
%
%  Prints help about the solver.
puzzle_help :-
	writeln('\033[;1mpuzzle_load_and_solve(+\033[4mFile\033[24m, ?\033[4mStrategy\033[24m)\033[21m'),
	writeln('  Loads a puzzle File and solves it using the given Strategy.'),
	writeln('  This is the most simple way of loading and solving a puzzle.'),
	writeln('  This predicate performs the following steps:'),
	writeln('  - Loads the specified File.'),
	writeln('  - Prints the original, unsolved version of the puzzle.'),
	writeln('  - Solves the puzzle using the specified Strategy.'),
	writeln('  - Prints the solved puzzle and shows the time it took.'),
	writeln('  Note that this predicate solves the puzzle only once, so the time might')
	writeln('  not represent the actual efficiency of the Strategy.')
	writeln('  For more information about the available strategies, see the full help'),
	writeln('  on the \033[4mpuzzle_solve\033[24m predicate.'),
	nl,
	nl,
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

:- 	writeln('\033[1;4;33mPuzzle Solver\033[21;37m'),
	writeln('Use \033[94mpuzzle_help\033[37m to show full help'),
	writeln('\033[m').