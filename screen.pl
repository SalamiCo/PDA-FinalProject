:- module(screen, [
	menu/2,
	ansi_code/3,
	ansi_clean/0,   ansi_clean/1,
	ansi_pos/2,     ansi_pos/3,
	ansi_pos_col/1, ansi_pos_col/2,
	ansi_move/2,    ansi_move/3,
	ansi_sgr/1,     ansi_sgr/2
]).
/** <module> Utilidades de escritura por pantalla
 *
 * Módulo que incluye una serie de utilidades para la escritura por pantalla
 * en terminal, así como un predicado específicamente diseñado para.
 */

%! ansi_code(+Nums:list, +Letter:atom -Res:atom) is det
%  
%  Devuelve un nombre con la secuencia ANSI correspondiente a Letter con los
%  parámetros de Nums. Por ejemplo, ansi_code([1,33],m) devolverá 'ESC[1;33m'.
ansi_code(Nums, Letter, Res) :- ansi_code_(Nums, Letter, "", Res).

ansi_code_([], Lt, Str, R) :- name(Lt, S), append(["\033[",Str,S],RS), name(R,RS).
ansi_code_([N|Ns], Lt, Str, R) :- name(N,S),
	(Str=="", append([Str,S], Str2); append([Str,";",S],Str2)),
	ansi_code_(Ns, Lt, Str2, R).

%! ansi_clean is det
%! ansi_clean(-Res:atom) is det
%  Escribe o devuelve en Res la secuencia ANSI de borrado de pantalla
ansi_clean :- ansi_clean(R), write(R).
ansi_clean(R) :- ansi_code([2], 'J', R).

%! ansi_pos(+Line:int, +Column:int)
%! ansi_pos(+Line:int, +Column:int, -Res:atom)
%  Escribe o devuelve en Res la secuencia ANSI para posicionar el cursor
%  en la línea Line y columna Column.
ansi_pos(L, C) :- ansi_pos(L,C,R), write(R).
ansi_pos(L, C, R) :- ansi_code([L,C], 'H', R).

%! ansi_pos_col(+Column:int)
%! ansi_pos_col(+Column:int, -Res:atom)
%  Escribe o devuelve en Res la secuencia ANSI para posicionar el cursor
%  en la columna Column.
ansi_pos_col(C) :- ansi_pos_col(C, R), write(R).
ansi_pos_col(C, R) :- ansi_code([C], 'G', R).

%! ansi_move(+Dir:atom, +N:int)
%! ansi_move(+Dir:atom, +N:int, -Res:atom)
%  Escribe o devuelve en Res la secuencia ANSI para mover el cursor N
%  posiciones en la dirección indicada por Dir (up, down, right o left).
ansi_move(Dir, N) :- ansi_move(Dir,N,R), write(R).
ansi_move(up,   N, R) :- ansi_code([N], 'A', R).
ansi_move(down, N, R) :- ansi_code([N], 'B', R).
ansi_move(right,N, R) :- ansi_code([N], 'C', R).
ansi_move(left, N, R) :- ansi_code([N], 'D', R).

%! ansi_sgr(+Opts:list)
%! ansi_sgr(+Opts:list, -Res:atom)
%  Escribe o devuelve en Res la secuencia ANSI que modifica el texto para que
%  cumpla las propiedades de Opts. Opts es una lista que puede contener:
%  - 'reset' para reestablecer el formato original
%  - 'bold' para utilizar texto en negrita
%  - 'nobold' para utilizar texto SIN negrita
%  - 'under' para utilizar texto subrayado
%  - 'nounder' para utilizar texto NO subrayado
%  - 'fg(Color)' para utilizar el color de letra dado
%  - 'bg(Color)' para utilizar el color de fondo dado
%  Color puede ser un color de black, red, green, blue, yellow, purple, cyan y
%  white, o bien hi(X), donde X es un color de los anteriores.
ansi_sgr(Opts) :- ansi_sgr(Opts, Res), write(Res).
ansi_sgr(Opts, Res) :- ansi_sgr_(Opts,Nums), ansi_code(Nums,'m',Res).

ansi_sgr_([],[]) :- !.
ansi_sgr_([C|Cs],[N|Ns]) :- ansi_sgr_(Cs,Ns), ansi_sgr_item(C,N).

ansi_sgr_item(reset, 0).
ansi_sgr_item(bold, 1).
ansi_sgr_item(under, 4).
ansi_sgr_item(nobold, 21).
ansi_sgr_item(nounder, 24).
ansi_sgr_item(fg(C), N1)     :- ansi_color_(C,N), N1 is 30+N.
ansi_sgr_item(fg(hi(C)), N1) :- ansi_color_(C,N), N1 is 90+N.
ansi_sgr_item(bg(C), N1)     :- ansi_color_(C,N), N1 is 40+N.
ansi_sgr_item(bg(hi(C)), N1) :- ansi_color_(C,N), N1 is 100+N.

ansi_color_(black,0).
ansi_color_(red,1).
ansi_color_(green,2).
ansi_color_(yellow,3).
ansi_color_(blue,4).
ansi_color_(purple,5).
ansi_color_(cyan,6).
ansi_color_(white,7).

%! menu(+Data:list, -Result:any)
%
%  Muestra un menú por pantalla y espera a la pulsación de una tecla.
%  El parámetro Data es una lista de términos que dan forma al menú.
%  El valor resultado del menú se unificará con Result.
%
%  Data es una lista de términos, donde cada término puede ser:
%  - title(T)     Escribe el título del menú, dado por T.
%  - subtitle(T)  Escribe el subtítulo del menú, dado por T.
%  - blank(N)     Escribe un total de N líneas en blanco.
%  - text(T)      Escribe una línea de text, dada en T
%  - menu(K,R,T)  Define una opción del menú. K es la tecla que el usuario debe
%                 pulsar para seleccionarla, R es el valor resultado que se
%                 devolverá en Result al seleccionar esta opción, y T es el
%                 texto que se mostrará al usuario como opción.
%  - code(L)      Ejecuta una serie de instrucciones, dadas en L como una lista
%
%  Estos términos se interpretan en orden. Debe definirse al menos una opción
%  de menú, o la llamada no podrá terminar nunca.
%
%  En todos los casos, T se imprimirá sin cambios, por lo que se recomienda
%  pasar átomos como valor.
menu(Data, Result) :- menu_build([clear|Data]), (menu_select(Data, Result), menu_item(clear); menu(Data,Result)).

menu_build([]).
menu_build([X|Xs]) :- menu_item(X), menu_build(Xs).

menu_item(title(T)) :-
	box_draw_, ansi_code([], s, R1), write(R1), ansi_pos(1,1),
	name(T, S), append([" ",S," "], S1), length(S1, L), name(T1, S1),
	ansi_sgr([bg(hi(cyan)), fg(black)]), center_(L), write(T1), ansi_sgr([reset]),
	ansi_code([], u, R2), write(R2), ansi_sgr([reset]).

menu_item(subtitle(T)) :- name(T, S), append(["— ", S, " —"], S1), length(S1,L), name(T1, S1),
	center_(L), write(T1), nl.

menu_item(clear) :- !, ansi_clean, ansi_pos(3, 1).
menu_item(blank(N)) :- N>0, !, nl, menu_item(blank(N-1)).
menu_item(text(T)) :- write(T), nl.
menu_item(menu(K,_,T)) :- !,
	ansi_sgr([fg(cyan)]), write('['), ansi_sgr([bold]), write(K),
	ansi_sgr([nobold]), write('] '), ansi_sgr([reset]), write(T), nl.

menu_item(code([C|Cs])) :- !, call(C), menu_item(code(Cs)).
menu_item(code([])) :- !.

menu_item(_).

menu_select(Data,R) :- get_single_char(K), name(C, [K]), menu_select_(Data,C,R).

menu_select_([menu(C, R, _)|_], C, R) :- !.
menu_select_([_|Xs], C, R) :- !, menu_select_(Xs, C, R).

box_draw_ :-
	tty_size(L, C), box_chars_(unicode, BH, BV, BTL, BTR, BBL, BBR),
	ansi_code([], s, R1), write(R1), ansi_sgr([fg(hi(cyan))]),
	box_hor_(1, C, BH), box_hor_(L, C, BH),
	box_ver_(1, L, BV), box_ver_(C, L, BV),
	ansi_pos(1, 1), write(BTL),
	ansi_pos(1, C), write(BTR),
	ansi_pos(L, 1), write(BBL),
	ansi_pos(L, C), write(BBR),
	ansi_code([], u, R2), write(R2), ansi_sgr([reset]).

box_hor_(_, 0, _).
box_hor_(L, C, BC) :- ansi_pos(L, C), write(BC), C1 is C - 1, box_hor_(L, C1, BC).

box_ver_(_, 0, _).
box_ver_(C, L, BC) :- ansi_pos(L, C), write(BC), L1 is L - 1, box_ver_(C, L1, BC).

box_chars_(unicode, '═', '║', '╔', '╗', '╚', '╝').

center_(L) :- tty_size(_, C), L1 is L//2, C1 is C//2, N is C1-L1, ansi_pos_col(N).