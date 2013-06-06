:- module(util, [timed/2, log/2, log/3]).

%! timed(+Goal:goal, -Time:float)
timed(Goal, Time) :-
	get_time(T1),
	call(Goal),
	get_time(T2),
	Time is round((T2 - T1)*1000000)/1000000.

log_(Fmt, Args) :- format(Fmt, Args).

log(Level, Fmt, Args) :- log_prefix(Level), log_(Fmt, Args).
log(Level, Fmt) :- log(Level, Fmt, []).

log_prefix(inf) :- write('inf: ').
log_prefix(wrn) :- write('wrn: ').
log_prefix(err) :- write('err: ').