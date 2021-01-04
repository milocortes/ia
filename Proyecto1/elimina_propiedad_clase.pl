%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% se define un operador "=>"
:-op(15,xfx,'=>').
a=>b.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_propiedad_clase/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye la propiedad 
%% de una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_propiedad_clase/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye la propiedad
%% específica de una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_propiedad_clase(_,_,[],[]).
remueve_propiedad_clase(Clase,Propiedad,[Class|T],[NC|NT]):-   
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,I),
  cambia_propiedades_clase(Clase,O,Propiedad,P,NP),
  NC=..[class,O,M,NP,R,I],
  remueve_propiedad_clase(Clase,Propiedad,T,NT).
  

cambia_propiedades_clase(_,_,_,[],[]).
cambia_propiedades_clase(Clase,Clase,Propiedad,P,NP):-
    elimina_elementos_con_la_propiedad(Propiedad,P,NP).
cambia_propiedades_clase(_,_,_,P,NP):-
    NP=P.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_elementos_con_la_propiedad
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

elimina_elementos_con_la_propiedad(_,[],[]).
elimina_elementos_con_la_propiedad(K,[K => _|T],R):-
  elimina_elementos_con_la_propiedad(K,T,R).
elimina_elementos_con_la_propiedad(K,[K|T],R):-
  elimina_elementos_con_la_propiedad(K,T,R).
elimina_elementos_con_la_propiedad(K,[not(K)|T],R):-
  elimina_elementos_con_la_propiedad(K,T,R).
elimina_elementos_con_la_propiedad(K,[not(K => _)|T],R):-
  elimina_elementos_con_la_propiedad(K,T,R).
elimina_elementos_con_la_propiedad(K,[P => V|T],[P1=>V1|R]):-
  P1=P,
  V1=V,
  elimina_elementos_con_la_propiedad(K,T,R).
elimina_elementos_con_la_propiedad(K,[P|T],[P1|R]):-
  P1=P,
  elimina_elementos_con_la_propiedad(K,T,R).

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
