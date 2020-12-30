:- use_module(utils).
:- use_module(create).
:- use_module(delete).
:- use_module(modify).
:- use_module(consult_ops).
:- use_module(class_extension).
:- use_module(classes_of).
:- use_module(property_extension).
:- use_module(properties_of).
:- use_module(relation_extension).
:- use_module(relations_of).

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
    open_kb('KB.txt', KB),
    add_class(taco, food, KB, NewKB),
	there_is_class(taco, KB, Answer),
	write('Is taco in KB: '),
	write(Answer),
	write('\n'),
	there_is_class(taco, NewKB, NewAnswer),
	write('Is taco in NewKB: '),
	write(NewAnswer),
	write('\n\n'),
    class_extension(food, NewKB, Result),
    write('KB: '),
	write(KB),
	write('\n\n'),
    write('NewKB: '),
	write(NewKB),
	write('\n\n'),
    write('Food class extension: '),
    write(Result).
