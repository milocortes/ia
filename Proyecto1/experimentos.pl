%%% get_objets
get_objets([_,_,_,_ | E], E).
%%% get_class
get_class([E,_,_,_ | _], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% se define un operador "=>"
:-op(15,xfx,'=>').
a=>b.
% caso base, la lista es vacia
valor(_,[],[]).
% si encuentra X en el primer elemento de la lista
% siendo una regla de correspondencia, la senala
valor(X,[X=>Y|_],Y).
% si no encuentra X en el primer valor, continua con el resto de la lista
valor(X,[_=>_|T],Ys):-
valor(X,T,Ys).
%%%%%%%%%%%%%%%%%%%%%
%%% get_all_classes
%%%%%%%%%%%%%%%%%%%%%
get_all_classes([],[]).
get_all_classes([H|T],[C|L]):-
    arg(1, H, C),
    get_all_classes(T,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_all_objects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_all_objects([],[]).
get_all_objects([H|T],[Z|L]):-
    arg(5,H,Z),
    write(Z),
    get_all_objects(T,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% search_class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_class(_,[],[]).
search_class(Class,L,C):-
    member(Class,L),
    C= Class.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% search_object
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
search_object(_,[],[]).
search_object(Object,L,O):-
    member(Object,L),
    O = Object.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_antecesor
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_antecesor(_,[],unknown).
get_antecesor(Class,Predicado,Antecesor):-
    arg(1, Predicado, ClassP),
    ClassP = Class,
    arg(2, Predicado, Antecesor).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_tree_of_classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_tree_of_classes([],[]).
get_tree_of_classes([H|T],[Mother=>Class|L]):-
    arg(1,H,Class),
    arg(2,H,Mother),
    get_tree_of_classes(T,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_mothers_of_classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_mothers_of_classes([],[]).
get_mothers_of_classes([H|T],[Class =>Mother|L]):-
    arg(1,H,Class),
    arg(2,H,Mother),
    get_mothers_of_classes(T,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% isElement
%% En lugar de este predicado usamos member
%% el cual ya está definido en prolog
%% The member/2 predicate checks whether an element is a member of a list:
%% ?- member(a, [b, c, a]).
%% true.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% deleteElement
%% En lugar de este predicado usamos delete
%% el cual ya está definido en prolog
%% The delete/3 predicate deletes a given element from a list.
%% Its synopsis is:
%% delete(List, Element, ListWithoutElement).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% changeElement
%Change all ocurrences of an element X in a list for the value Y
%changeElement(X,Y,InputList,OutputList).
%Example (p,b,[p,a,p,a,y,a],[p,b,p,b,y,b])

changeElement(_,_,[],[]).

changeElement(X,Y,[X|T],[Y|N]):-
	changeElement(X,Y,T,N).

changeElement(X,Y,[H|T],[H|N]):-
	changeElement(X,Y,T,N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% change_mother_single
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_mother_single(_,_,[],[]).
change_mother_single(Mold,Mnew,Pold,Pnew):-
  arg(2,Pold,MadreActual),
  not(Mold = MadreActual) -> Pnew=Pold ;
  Pold=.. Poldlist,
  changeElement(Mold,Mnew,Poldlist,Poldlist2),
  Pnew=.. Poldlist2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% change_mother_all_classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_mother_all_classes(_,_,[],[]).
change_mother_all_classes(Mold,Mnew,[H|T],[C|R]):-
  change_mother_all_classes(Mold,Mnew,T,R),
  change_mother_single(Mold,Mnew,H,C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% delete_all_elements_with_property
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

delete_all_elements_with_property(_,[],[]).
delete_all_elements_with_property(K,[K => _|T],R):-
  delete_all_elements_with_property(K,T,R).
delete_all_elements_with_property(K,[P => V|T],[P1=>V1|R]):-
  P1=P,
  V1=V,
  delete_all_elements_with_property(K,T,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_relaciones_con_objeto
%% El predicado remueve todas las relaciones
%% en las que está presente un objeto 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elimina_relaciones_con_objeto(_,[],[]).
elimina_relaciones_con_objeto(Objeto,[Class|T],[NC|TN]):-
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,Instances),
  get_instancia(Objeto,Instances,NR),
  NC=..[class,O,M,P,R,NR],
  write(NC),nl,
  elimina_relaciones_con_objeto(Objeto,T,TN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_instancia
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_instancia(_,[],[]).
get_instancia(Objeto,[I|T],[[Id,P,NR]|R]):-
  get_relations(I,RI),
  get_id(I,Id),
  get_properties(I,P),
  remove_relation(Objeto,RI,NR),
  get_instancia(Objeto,T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_instance
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_instances([]).
get_instances([Class|T]):-
  arg(5,Class,Instance),
  write(Instance),nl,
  get_relations_of_instances(Instance),
  get_instances(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_relations_of_instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_relations_of_instances([]).
get_relations_of_instances([Instance|T]):-
  get_relations(Instance,Relations),
  write(Relations),nl,
  get_relations_of_instances(T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_relations
%%% Obtiene todas las relaciones del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_relations([_,_| E], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_properties
%%% Obtiene todas las propiedades del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_properties([_,E| _], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% get_id
%%% Obtiene el id del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_id([E,_| _], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remove_relation
%% Remueve la relación en la que aparece como 
%% valor un objeto específico
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_relation(_,[],[]).
remove_relation(Objeto,[[_ =>Objeto]|T],NR):-
  remove_relation(Objeto,T,NR).
remove_relation(Objeto,[[K =>V]|T],[Knew =>Vnew|NR]):-
  Knew = K,
  Vnew = V,
  remove_relation(Objeto,T,NR).
remove_relation(Objeto,[[not(_ =>Objeto)]|T],NR):-
  remove_relation(Objeto,T,NR).
remove_relation(Objeto,[[not(K =>V)]|T],[not(Knew =>Vnew)|NR]):-
  Knew = K,
  Vnew = V,
  remove_relation(Objeto,T,NR).

  %--------------------------------------------------
  % Load and Save from files
  %--------------------------------------------------


  %KB open and save

  open_kb(Route,KB):-
  	open(Route,read,Stream),
  	readclauses(Stream,X),
  	close(Stream),
  	atom_to_term_conversion(X,KB).

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

  atom_to_term_conversion(ATOM, TERM) :-
  	 atom(ATOM),
  	 atom_to_chars(ATOM,STR),
  	 atom_to_chars('.',PTO),
  	 append(STR,PTO,STR_PTO),
  	 read_from_chars(STR_PTO,TERM).
