:- use_module(utils).
:- use_module(create).
:- use_module(delete).
:- use_module(modify).
:- use_module(consult_ops).
% :- use_module(class_extension).
% :- use_module(classes_of).
% :- use_module(property_extension).
% :- use_module(properties_of).
% :- use_module(relation_extension).
% :- use_module(relations_of).

%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------
%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).
 

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

atom_to_term(ATOM, TERM) :-
	 atom(ATOM),
	 atom_to_chars(ATOM,STR),
	 atom_to_chars('.',PTO),
	 append(STR,PTO,STR_PTO),
	 read_from_chars(STR_PTO,TERM).


%--------------------------------------------------------------------------------------------------
% Pruebas
%--------------------------------------------------------------------------------------------------
pruebas :- 
	%----------------------------------------------------------------------------------------------
	%Pruebas para la extensión de una relación y relaciones de un individuo
	%----------------------------------------------------------------------------------------------
	open_kb('KB2.txt', KB),
	relation_extension(not(friend), KB, Ans),
	write(Ans),
	nl,
	relation_extension(eat, KB, Ans2),
	write(Ans2),
	nl,
	relations_of_individual(pete, KB, Ans3),
	write(Ans3),
	nl,
	relation_extension(friend, KB, Ans4),
	write(Ans4),
	nl,
	relations_of_class(eagles, KB, Ans5),
	write(Ans5).
	%----------------------------------------------------------------------------------------------
	%Pruebas de extensión de una propiedad
	%----------------------------------------------------------------------------------------------
	% open_kb('KB.txt', KB),
	% property_extension(on_discount, KB, Ans),
	% write(Ans),
	% nl,
	% property_extension(not(on_discount), KB, Ans2),
	% write(Ans2),
	% nl,
	% property_extension(brand, KB, Ans3),
	% write(Ans3),
	% nl,
	% property_extension(brand=>heineken, KB, Ans4),
	% write(Ans4),
	% nl,
	% property_extension(delicious, KB, Ans5),
	% write(Ans5),
	% nl,
	% property_extension(not(delicious), KB, Ans6),
	% write(Ans6),
	% nl,
	% property_extension(nutrients=>high, KB, Ans7),
	% write(Ans7),
	% nl,
	% property_extension(not(nutrients=>high), KB, Ans8),
	% write(Ans8).
	%----------------------------------------------------------------------------------------------
	%Pruebas para la extensión de una clase
	%----------------------------------------------------------------------------------------------
	% open_kb('KB.txt', KB),
    % add_class(taco, food, KB, NewKB),
	% existencia_clase(taco, KB, Answer),
	% write('Is taco in KB: '),
	% write(Answer),
	% write('\n'),
	% existencia_clase(taco, NewKB, NewAnswer),
	% write('Is taco in NewKB: '),
	% write(NewAnswer),
	% write('\n\n'),
    % write('KB: '),
	% write(KB),
	% write('\n\n'),
    % write('NewKB: '),
	% write(NewKB),
	% write('\n\n'),
	% write('All objects names: '),
	% nl,
	% extension_clase_nombres(top, NewKB, Result3),
	% write(Result3),
	% nl,
	% write('Food class extension names: '),
	% extension_clase_nombres(food, NewKB, Result),
	% write(Result),
	% nl,
	% write('Comestible class extension names: '),
	% extension_clase_nombres(comestible, NewKB, Ans),
	% write(Ans),
	% nl,
	% write('Food class extension complete: '),
	% extension_clase_objetos(food, NewKB, Result2),
	% write(Result2),
	% nl,
	% write('Comestible class extension complete: '),
	% extension_clase_objetos(comestible, NewKB, Ans2),
	% write(Ans2),
	% nl,nl,
	% ----------------------------------------------------------------------------------------------
	% Pruebas para las propiedades de un individuo o de una clase
	% ----------------------------------------------------------------------------------------------
	% write('d1 properties: '),
	% propiedades_individuo(d1, NewKB, Props),
	% write(Props),
	% nl,
	% write('s1 properties: '),
	% propiedades_individuo(s1, NewKB, Props2),
	% write(Props2),
	% nl,
	% write('v1 properties: '),
	% propiedades_individuo(v1, NewKB, Props3),
	% write(Props3),
	% nl,
	% write('s2 properties: '),
	% propiedades_individuo(s2, NewKB, Props4),
	% write(Props4),
	% nl,
	% write('s3 properties: '),
	% propiedades_individuo(s3, NewKB, Props5),
	% write(Props5),
	% nl,
	% write('taco class properties: '),
	% class_properties(taco, NewKB, Props6),
	% write(Props6),
	% nl,
	% write('top class properties: '),
	% class_properties(top, NewKB, Props7),
	% write(Props7),
	% nl,
	% write('drink class properties: '),
	% class_properties(drink, NewKB, Props8),
	% write(Props8),
	% nl,
	% write('unexistant class properties: '),
	% class_properties(unexistant, NewKB, Props9),
	% write(Props9).  
