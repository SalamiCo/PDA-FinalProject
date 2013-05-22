:- use_module(screen).

main :- main_menu, ansi_clean, ansi_pos(1, 1), !.

main_menu :-
	menu([
		title('Resolutor de Puzzles'),
		subtitle('Menú Principal'),
		blank(1),
		text('¡Bienvenido al mejor resolutor de puzzles de la historia!'),
		blank(2),
		menu(f, file, 'Open file'),
		menu(e, exit, 'Exit application')
	], Opt), write(Opt), nl, (main_menu_proc(Opt), main_menu; !).

% Exits the loop
main_menu_proc(exit) :- !, fail.
main_menu_proc(file).

