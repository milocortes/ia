:- module(delete, [elimina_clase/3,
                   elimina_objeto/3,
                   elimina_propiedad_clase/4,
                   elimina_propiedad_objeto/4,
                   elimina_relacion_clase/4,
                   remueve_relacion_objeto/4]).
:- use_module(utils).


%%-------------------------------------
%% elimina_clase/3
%%-------------------------------------

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
    elimina_relaciones_con_clase(Clase,KBP2,KBP3),
    elimina_relaciones_con_objeto(Clase,KBP3,KBnew).

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
obten_instancia(Objeto,[[K=>V,P,R]|T],[[Kn=>Vn,Pn,RN]|RT]):-
  Kn=K,
  Vn=V,
  Pn=P,
  remueve_relacion_con_objeto(Objeto,R,RN),
  obten_instancia(Objeto,T,RT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remueve_relacion_con_objeto
%% Remueve la relación en la que aparece como
%% valor un objeto específico
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_relacion_con_objeto(_,[],[]).
remueve_relacion_con_objeto(Objeto,[_ =>Objeto|T],NR):-
  remueve_relacion_con_objeto(Objeto,T,NR).
remueve_relacion_con_objeto(Objeto,[_ =>Nombres|T],NR):-
    member(Objeto,Nombres),
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

%%-------------------------------------
%% elimina_objeto/3
%%-------------------------------------

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
  elimina_relaciones_con_objeto(Objeto,KBP,KBP1),
  elimina_relaciones_con_clase(Objeto,KBP1,KBnew).

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
obten_intancias_rm(Objeto,[[_=>Nombres,_,_]|T],R):-
    member(Objeto,Nombres),
    obten_intancias_rm(Objeto,T,R).
obten_intancias_rm(Objeto,[[_=>Objeto,_,_]|T],R):-
    obten_intancias_rm(Objeto,T,R).
obten_intancias_rm(Objeto,[[K=>V,P,R]|T],[[Knew=>Vnew,Pnew,Rnew]|RN]):-
    Knew=K,
    Vnew=V,
    Pnew=P,
    Rnew=R,
    obten_intancias_rm(Objeto,T,RN).

%%-------------------------------------
%% elimina_propiedad_clase/4
%%-------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_propiedad_clase/4
%% el predicado itera sobre la KB y genera
%% una nueva lista en la que se excluye la propiedad
%% de una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_propiedad_clase(_,_,[],[]).
elimina_propiedad_clase(Clase,Propiedad,KB,KBnew):-
  remueve_propiedad_clase(Clase,Propiedad,KB,KBnew).


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

%%-------------------------------------
%% elimina_propiedad_objeto/4
%%-------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_propiedad_objeto/4
%% el predicado itera sobre la KB y genera
%% una nueva lista en la que se excluye la propiedad
%% de un objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_propiedad_objeto(_,_,[],[]).
elimina_propiedad_objeto(Objeto,Propiedad,KB,KBnew):-
  remueve_propiedad_objeto(Objeto,Propiedad,KB,KBnew).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_propiedad_objeto/3
%% el predicado itera sobre la KB y genera
%% una nueva lista en la que se excluye a un objeto
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_propiedad_objeto(_,_,[],[]).
remueve_propiedad_objeto(Objeto,Propiedad,[Class|T],[NC|NT]):-
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,I),
  obten_propiedades_objeto_rm(Objeto,Propiedad,I,NI),
  NC=..[class,O,M,P,R,NI],
  remueve_propiedad_objeto(Objeto,Propiedad,T,NT).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% obten_propiedades_objeto_rm/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
obten_propiedades_objeto_rm(_,_,[],[]).
obten_propiedades_objeto_rm(Objeto,Propiedad,[[id=>Objeto,P,R]|T],[[id=>Objeto,NP,R]|RN]):-
    elimina_elementos_con_la_propiedad(Propiedad,P,NP),
    obten_propiedades_objeto_rm(Objeto,Propiedad,T,RN).
obten_propiedades_objeto_rm(Objeto,Propiedad,[[id=>Nombres,P,R]|T],[[id=>Objeto,NP,R]|RN]):-
    member(Objeto,Nombres),
    elimina_elementos_con_la_propiedad(Propiedad,P,NP),
    obten_propiedades_objeto_rm(Objeto,Propiedad,T,RN).
obten_propiedades_objeto_rm(Objeto,Propiedad,[[K=>V,P,R]|T],[[Knew=>Vnew,Pnew,Rnew]|RN]):-
    Knew=K,
    Vnew=V,
    Pnew=P,
    Rnew=R,
    obten_propiedades_objeto_rm(Objeto,Propiedad,T,RN).

%%-------------------------------------
%% elimina_relacion_clase/4
%%-------------------------------------

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_relacion_clase/4
%% el predicado itera sobre la KB y genera
%% una nueva lista en la que se excluye la relacion
%% de una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
elimina_relacion_clase(_,_,[],[]).
elimina_relacion_clase(Clase,Relacion,KB,KBN):-
  remueve_relacion_clase(Clase,Relacion,KB,KBN).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_relacion_clase/3
%% el predicado itera sobre la KB y genera
%% una nueva lista en la que se excluye la relacion
%% específica de una clase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remueve_relacion_clase(_,_,[],[]).
remueve_relacion_clase(Clase,Relacion,[Class|T],[NC|NT]):-
  arg(1,Class,O),
  arg(2,Class,M),
  arg(3,Class,P),
  arg(4,Class,R),
  arg(5,Class,I),
  cambia_relaciones_clase(Clase,O,Relacion,R,NR),
  NC=..[class,O,M,P,NR,I],
  remueve_relacion_clase(Clase,Relacion,T,NT).


cambia_relaciones_clase(_,_,_,[],[]).
cambia_relaciones_clase(Clase,Clase,Relacion,R,NR):-
    elimina_elementos_con_la_relacion(Relacion,R,NR).
cambia_relaciones_clase(_,_,_,R,NR):-
    NR=R.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% elimina_elementos_con_la_relacion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

elimina_elementos_con_la_relacion(_,[],[]).
elimina_elementos_con_la_relacion(K,[K => _|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[K|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[not(K)|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[not(K => _)|T],R):-
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[P => V|T],[P1=>V1|R]):-
  P1=P,
  V1=V,
  elimina_elementos_con_la_relacion(K,T,R).
elimina_elementos_con_la_relacion(K,[P|T],[P1|R]):-
  P1=P,
  elimina_elementos_con_la_relacion(K,T,R).
%%-------------------------------------
%% remueve_relacion_objeto/4
%%-------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% remueve_relacion_objeto/4
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
