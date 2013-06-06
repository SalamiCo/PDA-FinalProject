:- module(util, [timed/2, take/3, range/3, log/2, log/3]).

%! timed(+Goal:goal, -Time:float)
timed(Goal, Time) :-
	get_time(T1),
	call(Goal),
	get_time(T2),
	Time is round((T2 - T1)*1000000)/1000000.

take([N|Ns], N, Ns).
take([N|Ns], N1, [N|Ns1]) :- take(Ns, N1, Ns1).

range(L, H, []) :- H < L, !.
range(L, H, [L|Vs]) :- L1 is L+1, range(L1, H, Vs).

log_(Fmt, Args) :- format(Fmt, Args).

log(Level, Fmt, Args) :- log_prefix(Level), log_(Fmt, Args).
log(Level, Fmt) :- log(Level, Fmt, []).

log_prefix(inf) :- write('inf: ').
log_prefix(wrn) :- write('wrn: ').
log_prefix(err) :- write('err: ').