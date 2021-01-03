
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% se define un operador "=>"
:-op(15,xfx,'=>').
a=>b.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_clase/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye a una clase
%% además que para aquellas clases en las que esta
%% era su madre, la sustituye por la madre de la clase 
%% eliminada.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_clase(_,[],[]).
elimina_clase(Clase,KB,KBnew):-
    remueve_clase(Clase,KB,KBP),
    obten_madres_de_clases(KB,LM),
    valor(Clase,LM,M),
    cambia_madre_todas_clases(Clase,M,KBP,KBP2),
    elimina_relaciones_con_clase(Clase,KBP2,KBnew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_clase/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye a una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_clase(_,[],[]).
remueve_clase(Clase,[CO|T],NT):-
    obten_nombre_clase(CO,Clase),
    !,
    remueve_clase(Clase,T,NT).
remueve_clase(Clase,[CO|T],[NC|NT]):-
    not(obten_nombre_clase(CO,Clase)),
    !,
    NC=CO,
    remueve_clase(Clase,T,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% obten_nombre_clase/2
%% el predicado prueba si el nombre de 
%% la clase es C.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_nombre_clase([],_).
obten_nombre_clase(class(C,_,_,_,_),C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% obten_madres_de_clases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_madres_de_clases([],[]).
obten_madres_de_clases([H|T],[Class =>Mother|L]):-
    arg(1,H,Class),
    arg(2,H,Mother),
    obten_madres_de_clases(T,L).

% caso base, la lista es vacia
valor(_,[],[]).
% si encuentra X en el primer elemento de la lista
% siendo una regla de correspondencia, la senala
valor(X,[X=>Y|_],Y).
% si no encuentra X en el primer valor, continua con el resto de la lista
valor(X,[_=>_|T],Ys):-
valor(X,T,Ys).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cambia_madre_de_clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambia_madre_de_clase(_,_,[],[]).
cambia_madre_de_clase(Mold,Mnew,Pold,Pnew):-
  arg(2,Pold,MadreActual),
  not(Mold = MadreActual) -> Pnew=Pold ;
  Pold=.. Poldlist,
  changeElement(Mold,Mnew,Poldlist,Poldlist2),
  Pnew=.. Poldlist2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% cambia_madre_todas_clases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cambia_madre_todas_clases(_,_,[],[]).
cambia_madre_todas_clases(Mold,Mnew,[H|T],[C|R]):-
  cambia_madre_todas_clases(Mold,Mnew,T,R),
  cambia_madre_de_clase(Mold,Mnew,H,C).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_relaciones_con_clase
%% El predicado remueve todas las relaciones
%% en las que está presente un Clase 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elimina_relaciones_con_clase(_,[],[]).
elimina_relaciones_con_clase(Clase,[CO|T],[NC|TN]):-
  arg(1,CO,O),
  arg(2,CO,M),
  arg(3,CO,P),
  arg(4,CO,R),
  arg(5,CO,I),
  remueve_relacion_con_clase(Clase,R,NR),
  NC=..[class,O,M,P,NR,I],
  write(NC),nl,
  elimina_relaciones_con_clase(Clase,T,TN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remueve_relacion_con_clase
%% Remueve la relación en la que aparece como 
%% valor una clase específica
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_relacion_con_clase(_,[],[]).
remueve_relacion_con_clase(Clase,[_ =>Clase|T],NR):-
  remueve_relacion_con_clase(Clase,T,NR).
remueve_relacion_con_clase(Clase,[not(_ =>Clase)|T],NR):-
  remueve_relacion_con_clase(Clase,T,NR).
remueve_relacion_con_clase(Clase,[K =>V|T],[Knew =>Vnew|NR]):-
  Knew = K,
  Vnew = V,
  remueve_relacion_con_clase(Clase,T,NR).
remueve_relacion_con_clase(Clase,[not(K =>V)|T],[not(Knew =>Vnew)|NR]):-
  Knew = K,
  Vnew = V,
  remueve_relacion_con_clase(Clase,T,NR).









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
