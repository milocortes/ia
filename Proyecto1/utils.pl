:- module(utils,  [cambiar_elem/4, 
                  eliminar_elem/3, 
                  verifica_elem/2, 
                  aplana_un_nivel/2,
                  deleteAllElementsWithSameProperty/3,
                  deleteAllElementsWithSameNegatedProperty/3,
                  op(800,xfx,'=>')]).


				
%----------------------------------------
% Administration of lists
%----------------------------------------


%Cambiar las ocurrencias de un elemento O por un elemento R
%e.g. cambiar_elem(a,b,[b,a,n,a,n,a],A).
% A = [b,b,n,b,n,b]

%Caso base: 
%si la lista es vacía,
%cambiar cualquier cosa con cualquier cosa
%sigue dando una lista vacía
cambiar_elem(_,_,[],[]).

%Caso recursivo:
%Si el Head de la lista es el mismo
%que el elemento O (el elemento a remplazar)
%se reemplaza por R, y continuamos procesando el Tail
cambiar_elem(O,R,[O|T],[R|T2]):-
	cambiar_elem(O,R,T,T2).

%Caso recursivo:
%Si el Head de la lista no es el mismo
%que el elemento O, continuamos procesando el Tail
cambiar_elem(O,R,[H|T],[H|T2]):-
	cambiar_elem(O,R,T,T2).


%Elimina todas las apariciones de un elemento en una lista
%e.g.
%eliminar_elem(a, [b,a,n,a,n,a], A).
%A = [b, n, n] .

%Caso base: 
%La lista esta vacia,
%eliminar cualquier cosa de una lista vacía
%sigue dando una lista vacía
eliminar_elem(_, [], []).
%Caso recursivo:
%Si H (el elemento buscado) es el Head de la lista
% no se copia a L, y seguimos procesando el Tail de la lista
eliminar_elem(H, [H|T], L):- eliminar_elem(H, T, L).
%Caso recursivo:
%Si H, no es el Head de la lista (X)
%Copiamos el Head de la lista a la lista de salida
%y continuamos procesando el Tail de la lista
eliminar_elem(H, [X|T], [X|T2]):- eliminar_elem(H, T, T2).



%Verfica si un elemento está en la lista 
%e.g.
%verifica_elem(a, [b,a,n,a,n,a]).               
%true.

%Caso base:
%El elemento buscado (X), es el mismo que 
%el Head de la lista, por lo tanto ya terminé, el predicado es verdadero
verifica_elem(X,[X|_]).
%El elemento buscado, no es el mismo que el Head de la lista
%continuamos buscando en el Tail de la lista
verifica_elem(X,[_|T]):-
	verifica_elem(X,T).


%Aplana una lista de listas a un lista
%e.g.
%aplana_un_nivel([[b],[a,n],[a,n,[a]]],A).      
%A = [b, a, n, a, n, [a]].
%Caso base:
%Si aplanamos una lista vacía, el resultado seguirá siendo una lista vacía
aplana_un_nivel([],[]).
%Caso recursivo:
%Si la lista no es vacía, concatenamos al Head de la lista una T2,
%guardamos el resultado en la salida (X),
%y continuamos procesando el Tail de la lista recursivamente, tomando a T2 como la nueva salida
%Nota: no es un append, es un concatenation
%https://www.swi-prolog.org/pldoc/man?predicate=append/3
aplana_un_nivel([H|T],X):-
	append(H,T2,X),
	aplana_un_nivel(T,T2).


%Delete all elements with a specific property in a property-value list
%deleteAllElementsWithSameProperty(P,InputList,OutputList).
%Example (p2,[p1=>v1,p2=>v2,p3=>v3,p2=>v4,p4=>v4],[p1=>v1,p3=>v3,p4=>v4])

deleteAllElementsWithSameProperty(_,[],[]).

deleteAllElementsWithSameProperty(X,[X=>_|T],N) :-
	deleteAllElementsWithSameProperty(X,T,N).

deleteAllElementsWithSameProperty(X,[H|T],[H|N]):-
	deleteAllElementsWithSameProperty(X,T,N).


%Delete all elements with a specific negated property in a property-value list
%deleteAllElementsWithSameNegatedProperty(P,InputList,OutputList).
%Example (p2,[p1=>v1,not(p2=>v2),not(p3=>v3),p2=>v4,p4=>v4],[p1=>v1,not(p3=>v3),p2=>v4,p4=>v4])

deleteAllElementsWithSameNegatedProperty(_,[],[]).

deleteAllElementsWithSameNegatedProperty(X,[not(X=>_)|T],N):-
	deleteAllElementsWithSameNegatedProperty(X,T,N).

deleteAllElementsWithSameNegatedProperty(X,[H|T],[H|N]):-
	deleteAllElementsWithSameNegatedProperty(X,T,N).
