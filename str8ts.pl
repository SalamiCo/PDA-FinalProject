:- module(str8ts, [str8ts_load/2, str8ts_print/1, str8ts_solve/2]).

str8ts_load(Stream, Result) :- fail.

str8ts_print(Puzzle).

str8ts_solve(Puzzle, brute) :- str8ts_solve_brute(Puzzle).
str8ts_solve(Puzzle, clpfd) :- str8ts_solve_clpfd(Puzzle).
str8ts_solve(Puzzle, optimized) :- str8ts_solve_optimized(Puzzle).