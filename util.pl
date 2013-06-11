:- module(util, [timed/2, take/3, range/3, merge/3, factdiv/3]).

%! timed(+Goal:goal, -Time:float) is nondet
%  
%  Runs the given goal and calculates the time it took.
%  Note that, in the case of nondeterministic calculations, if the Prolog prompt is waiting for the user to press a key
%  to allow for more solutions, the returned time mey be inaccutrate.
%
%  @arg Goal The goal to be run
%  @arg Time The time that took to run the Goal
timed(Goal, Time) :-
	get_time(T1),
	call(Goal),
	get_time(T2),
	Time is round((T2 - T1)*1000000)/1000000.

%! factdiv(+X:integer, +Y:integer, -Z:integer) is det
%
%  Returns in Z the result of (X! / Y!).
%
%  @arg X First argument
%  @arg Y Seconds argument
%  @arg Result
factdiv(0, _, 1) :- !.
factdiv(X, Y, 1) :- X =< Y, !.
factdiv(X, Y, Z) :- X1 is X-1, factdiv(X1, Y, Z1), Z is Z1*X.

%! take(+List:list, ?Elem:mixed, -Rest:list) is nondet
%  
%  Succeeds if Elem is in List. Rest is unified with the list with Elem removed.
%  If Elem can be unified with more than one element, this predicate will succeed more than once.
%
%  @arg List Original list
%  @arg Elem An element from List
%  @arg Rest The List without the selected Elem
take([N|Ns], N, Ns).
take([N|Ns], N1, [N|Ns1]) :- take(Ns, N1, Ns1).

%! range(+Low:integer, +High:integer, -List:list) is det
%
%  Generates a list with all consegutive integer from Low to High, both included
%
%  @arg Low Lower bound for the list
%  @arg High Higher bound for the list
%  @arg List The resulting list
range(L, H, []) :- H < L, !.
range(L, H, [L|Vs]) :- L1 is L+1, range(L1, H, Vs).

%! merge(+List1:list, +List2:list, -Merged:list) is det
%  
%  Merges two lists in such a way that the resulting list has the elements of the two lists interleaved.
%
%  @arg List1 Fisrt list to merge
%  @arg List2 Second list to merge
%  @arg Merged Resulting list
merge([], Ys, Ys) :- !.
merge(Xs, [], Xs) :- !.
merge([X|Xs], [Y|Ys], [X,Y|Ms]) :- merge(Xs, Ys, Ms).