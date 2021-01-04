
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% se define un operador "=>"
:-op(15,xfx,'=>').
a=>b.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_propiedad_objeto/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye la propiedad 
%% de un objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_relacion_objeto/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye a un objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_relacion_objeto(_,_,[],[]).
remueve_relacion_objeto(Objeto,Relacion,[Class|T],[NC|NT]):-
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,I),
  obten_relaciones_objeto_rm(Objeto,Relacion,I,NI),
  NC=..[class,O,M,P,R,NI],
  remueve_relacion_objeto(Objeto,Relacion,T,NT).  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%% obten_relaciones_objeto_rm/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_relaciones_objeto_rm(_,_,[],[]).
obten_relaciones_objeto_rm(Objeto,Relacion,[[id=>Objeto,P,R]|T],[[id=>Objeto,P,NR]|RN]):-
    elimina_elementos_con_la_relacion(Relacion,R,NR),
    obten_relaciones_objeto_rm(Objeto,Relacion,T,RN).
obten_relaciones_objeto_rm(Objeto,Relacion,[[K=>V,P,R]|T],[[Knew=>Vnew,Pnew,Rnew]|RN]):-
    Knew=K,
    Vnew=V,
    Pnew=P,
    Rnew=R,
    obten_relaciones_objeto_rm(Objeto,Relacion,T,RN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_elementos_con_la_relacion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

elimina_elementos_con_la_relacion(_,[],[]).
elimina_elementos_con_la_relacion(K,[K => _|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[K|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[not(K => _)|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[not(K)|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[P => V|T],[P1=>V1|R]):-
  P1=P,
  V1=V,
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[P|T],[P1|R]):-
  P1=P,
  elimina_elementos_con_la_relacion(K,T,R).

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

