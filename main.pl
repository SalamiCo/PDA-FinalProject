:- use_module(screen).

main :- main_menu.

main_menu :-
	menu([
		title('Reslutor de Puzzles'),
		subtitle('Reslutor de Puzzles'),
		blank(1),
		text('¡Bienvenido al mejor resolutor de puzzles de la historia!'),
		blank(2),
		menu(f, file, 'Open file'),
		menu(e, exit, 'Exit application')
	], Opt), write(Opt), nl, (main_menu_proc(Opt), main_menu; !).

% Exits the loop
main_menu_proc(exit) :- !, fail.
main_menu_proc(file).

