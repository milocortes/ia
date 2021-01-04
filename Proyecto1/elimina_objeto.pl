
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% se define un operador "=>"
:-op(15,xfx,'=>').
a=>b.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_objeto/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye a un objeto
%% además que se eliminan las relaciones  
%% en las que aparece dicho objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_objeto(_,[],[]).
elimina_objeto(Objeto,KB,KBnew):-
  remueve_objeto(Objeto,KB,KBP),
  elimina_relaciones_con_objeto(Objeto,KBP,KBnew).
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_objeto/3
%% el predicado itera sobre la KB y genera 
%% una nueva lista en la que se excluye a un objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remueve_objeto(_,[],[]).
remueve_objeto(Objeto,[Class|T],[NC|NT]):-
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,Instances),    
  obten_intancias_rm(Objeto,Instances,NI),
  NC=..[class,O,M,P,R,NI],
  write(NC),nl,
  remueve_objeto(Objeto,T,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%% obten_intancias_rm/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_intancias_rm(_,[],[]).
obten_intancias_rm(Objeto,[[_=>Objeto,_,_]|T],R):-
    obten_intancias_rm(Objeto,T,R).
obten_intancias_rm(Objeto,[[K=>V,P,R]|T],[[Knew=>Vnew,Pnew,Rnew]|RN]):-
    Knew=K,
    Vnew=V,
    Pnew=P,
    Rnew=R,
    obten_intancias_rm(Objeto,T,RN).

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
  obten_instancia(Objeto,Instances,NR),
  NC=..[class,O,M,P,R,NR],
  elimina_relaciones_con_objeto(Objeto,T,TN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% obten_instancia
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_instancia(_,[],[]).
obten_instancia(Objeto,[[_=>O,P,R]|T],[[id=>On,Pn,NR]|R]):-
  On=O,
  Pn=P,
  remueve_relacion_con_objeto(Objeto,R,NR),
  obten_instancia(Objeto,T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remueve_relacion_con_objeto
%% Remueve la relación en la que aparece como 
%% valor un objeto específico
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_relacion_con_objeto(_,[],[]).
remueve_relacion_con_objeto(Objeto,[_ =>Objeto|T],NR):-
  remueve_relacion_con_objeto(Objeto,T,NR).
remueve_relacion_con_objeto(Objeto,[K =>V|T],[Knew =>Vnew|NR]):-
  Knew = K,
  Vnew = V,
  remueve_relacion_con_objeto(Objeto,T,NR).
remueve_relacion_con_objeto(Objeto,[not(_ =>Objeto)|T],NR):-
  remueve_relacion_con_objeto(Objeto,T,NR).
remueve_relacion_con_objeto(Objeto,[not(K =>V)|T],[not(Knew =>Vnew)|NR]):-
  Knew = K,
  Vnew = V,
  remueve_relacion_con_objeto(Objeto,T,NR).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% obten_relaciones_objeto
%%% Obtiene todas las relaciones del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_relaciones_objeto([_,_| E], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% obten_propiedades_objeto
%%% Obtiene todas las propiedades del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_propiedades_objeto([_,E| _], E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% obten_id_objeto
%%% Obtiene el id del objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_id_objeto([E,_| _], E).

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
