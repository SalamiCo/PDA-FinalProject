:- module(util, [timed/2]).

%! timed(+Goal:goal, -Time:float) is 
timed(Goal, Time) :-
	get_time(T1),
	call(Goal),
	get_time(T2),
	Time is round((T2 - T1)*1000000)/1000000.