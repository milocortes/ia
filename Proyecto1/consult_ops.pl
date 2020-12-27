:- module(consult_ops, [objetos_de_una_clase/3,
						objetos_clase/3,
						filter_objects_with_property/4,
						object_property_value/4,
						eliminate_null_property/2,
						object_relation_value/4,
						existencia_clase/3,
						there_is_object/3,
						class_of_an_object/3,
						class_ancestors/3,
						object_properties/3,
						expand_classes_to_objects/3,
						object_relations/3]).
:- use_module(utils).


%--------------------------------------------------------------------------------------------------
%Operations for consulting 
%--------------------------------------------------------------------------------------------------


%Verifica si una clase o su negado existe
%Caso base:
%Para evitar la negación por falla,
%vamos a responder que no se sabe
%si existe la clase o no (no encontramos la clase o el negado de la clase)
existencia_clase(_,[],unknown).
%Caso recursivo:
%El Head de la lista que representa a la KB es not(Class) (el negado de la clase que estamos buscando)
%por lo tanto regresamos la no-existencia de la clase en el sentido fuerte
existencia_clase(Class,[class(not(Class),_,_,_,_)|_],no).
%Caso recursivo:
%El Head de la lista Class (la clase que estamos buscando)
%por lo tanto regresamos la existencia de la clase
existencia_clase(Class,[class(Class,_,_,_,_)|_],yes).
%Caso recursivo:
%Si el Head de la lista es una clase con cualquier otro nombre destinto a la clase que estamos buscando o su negado
%continuamos la procesando el Tail de la KB
existencia_clase(Class,[class(_,_,_,_,_)|T],Answer):-
	existencia_clase(Class,T,Answer).



%Verify if an object exists
there_is_object(_,[],unknown).

there_is_object(Object,[class(_,_,_,_,O)|_],no):-
	verifica_elem([id=>not(Object),_,_],O).

there_is_object(Object,[class(_,_,_,_,O)|_],yes):-
	verifica_elem([id=>Object,_,_],O).

there_is_object(Object,[_|T],Answer):-
	there_is_object(Object,T,Answer).



%Consult the mother of a class
mother_of_a_class(_,[],unknown).

mother_of_a_class(Class,[class(Class,Mother,_,_,_)|_],Mother).

mother_of_a_class(Class,[_|T],Mother):-
	mother_of_a_class(Class,T,Mother).



%Consult the ancestors of a class
class_ancestors(Class,KB,ClassAncestors):-
	existencia_clase(Class,KB,yes),
	list_of_ancestors(Class,KB,ClassAncestors).

class_ancestors(Class,KB,unknown):-
	existencia_clase(Class,KB,unknown).

list_of_ancestors(top,_,[]).

list_of_ancestors(Class,KB,Ancestors):-
	mother_of_a_class(Class,KB,Mother),
	append([Mother],GrandParents,Ancestors),
	list_of_ancestors(Mother,KB,GrandParents).



%Consult the properties of a class
class_properties(top,KB,Properties):-
	properties_only_in_the_class(top,KB,Properties).

class_properties(Class,KB,Properties):-
	existencia_clase(Class,KB,yes),
	properties_only_in_the_class(Class,KB,ClassProperties),
	append([ClassProperties],AncestorsProperties,AllProperties),
	concat_ancestors_properties(Ancestors,KB,AncestorsProperties),
	list_of_ancestors(Class,KB,Ancestors),
	cancel_repeated_property_values(AllProperties,Properties).

class_properties(Class,KB,unknown):-
	existencia_clase(Class,KB,unknown).


properties_only_in_the_class(_,[],[]).

properties_only_in_the_class(Class,[class(Class,_,Properties,_,_)|_],Properties).

properties_only_in_the_class(Class,[_|T],Properties):-
	properties_only_in_the_class(Class,T,Properties).


concat_ancestors_properties([],_,[]).

concat_ancestors_properties([Ancestor|T],KB,[Properties|NewT]):-
	concat_ancestors_properties(T,KB,NewT),
	properties_only_in_the_class(Ancestor,KB,Properties).

cancel_repeated_property_values(X,Z):-
	aplana_un_nivel(X,Y),
	delete_repeated_properties(Y,Z).

delete_repeated_properties([],[]).

delete_repeated_properties([P=>V|T],[P=>V|NewT]):-
	borraTodoElementoConIgualPropiedad(P,T,L1),
	eliminar_elem(not(P=>V),L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([not(P=>V)|T],[not(P=>V)|NewT]):-
	deleteAllElementsWithSameNegatedProperty(P,T,L1),
	eliminar_elem(P=>V,L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([not(H)|T],[not(H)|NewT]):-
	eliminar_elem(not(H),T,L1),
	eliminar_elem(H,L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([H|T],[H|NewT]):-
	eliminar_elem(H,T,L1),
	eliminar_elem(not(H),L1,L2),
	delete_repeated_properties(L2,NewT).



%Verify if a class has a specific property
class_has_property(Class,Property,KB,Answer):-
	class_properties(Class,KB,Properties),
	incomplete_information(Property,Properties,Answer).

incomplete_information(_,[], unknown).

incomplete_information(Atom, List, yes):- verifica_elem(Atom,List).

incomplete_information(not(Atom), List, no):- verifica_elem(Atom,List).

incomplete_information(Atom, List, no):- verifica_elem(not(Atom),List).

incomplete_information(_, _, unknown).



%Return the value of a class property
class_property_value(Class,Property,KB,Value):-
	class_properties(Class,KB,ClassProperties),
	find_value(Property,ClassProperties,Value).

find_value(_,[],unknown).

find_value(Attribute,[Attribute=>Value|_],Value).

find_value(Attribute,[not(Attribute)|_],no).

find_value(Attribute,[Attribute|_],yes).

find_value(Attribute,[_|T],Value):-
	find_value(Attribute,T,Value).



%Shows the class of an object
class_of_an_object(_,[],unknown):-!.

class_of_an_object(Object,[class(C,_,_,_,O)|_],C):-
	verifica_elem([id=>Object,_,_],O).

class_of_an_object(Object,[_|T],Class):-
	class_of_an_object(Object,T,Class).



%List all the properties of an object
object_properties(Object,KB,AllProperties):-
	there_is_object(Object,KB,yes),
	properties_only_in_the_object(Object,KB,ObjectProperties),
	class_of_an_object(Object,KB,Class),
	class_properties(Class,KB,ClassProperties),
	append(ObjectProperties,ClassProperties,Temp),
	delete_repeated_properties(Temp,AllProperties).

object_properties(_,_,unknown).

properties_only_in_the_object(_,[],[]).

properties_only_in_the_object(Object,[class(_,_,_,_,O)|_],Properties):-
	verifica_elem([id=>Object,Properties,_],O).

properties_only_in_the_object(Object,[_|T],Properties):-
	properties_only_in_the_object(Object,T,Properties).
	


%Return the value of an object property
object_property_value(Object,Property,KB,Value):-
	there_is_object(Object,KB,yes),
	object_properties(Object,KB,Properties),
	find_value(Property,Properties,Value).

object_property_value(_,_,_,unknown).



%Consult the relations of a class
class_relations(top,KB,Relations):-
	relations_only_in_the_class(top,KB,Relations).

class_relations(Class,KB,Relations):-
	existencia_clase(Class,KB,yes),
	relations_only_in_the_class(Class,KB,ClassRelations),
	append([ClassRelations],AncestorsRelations,AllRelations),
	concat_ancestors_relations(Ancestors,KB,AncestorsRelations),
	list_of_ancestors(Class,KB,Ancestors),
	cancel_repeated_property_values(AllRelations,Relations).

class_relations(_,_,unknown).


relations_only_in_the_class(_,[],[]).

relations_only_in_the_class(Class,[class(Class,_,_,Relations,_)|_],Relations).

relations_only_in_the_class(Class,[_|T],Relations):-
	relations_only_in_the_class(Class,T,Relations).


concat_ancestors_relations([],_,[]).

concat_ancestors_relations([Ancestor|T],KB,[Relations|NewT]):-
	concat_ancestors_relations(T,KB,NewT),
	relations_only_in_the_class(Ancestor,KB,Relations).



%Return the value of a class relation
class_relation_value(Class,Relation,KB,Value):-
	existencia_clase(Class,KB,yes),
	class_relations(Class,KB,Relations),
	find_value_relation(Relation,Relations,Value).

class_relation_value(_,_,_,unknown).


find_value_relation(not(Relation),Relations,Value):-
	find_value_negative_relation(Relation,Relations,Value).

find_value_relation(Relation,Relations,Value):-
	find_value_positive_relation(Relation,Relations,Value).


find_value_negative_relation(_,[],unknown).

find_value_negative_relation(Attribute,[not(Attribute=>Value)|_],Value).

find_value_negative_relation(Attribute,[_|T],Value):-
	find_value_negative_relation(Attribute,T,Value).


find_value_positive_relation(_,[],unknown).

find_value_positive_relation(Attribute,[Attribute=>Value|_],Value).

find_value_positive_relation(Attribute,[_|T],Value):-
	find_value_positive_relation(Attribute,T,Value).



%List all the relations of an object
object_relations(Object,KB,AllRelations):-
	there_is_object(Object,KB,yes),
	relations_only_in_the_object(Object,KB,ObjectRelations),
	class_of_an_object(Object,KB,Class),
	class_relations(Class,KB,ClassRelations),
	append([ObjectRelations],[ClassRelations],Temp),
	cancel_repeated_property_values(Temp,AllRelations).

object_relations(_,_,unknown).


relations_only_in_the_object(_,[],[]).

relations_only_in_the_object(Object,[class(_,_,_,_,O)|_],Relations):-
	verifica_elem([id=>Object,_,Relations],O).

relations_only_in_the_object(Object,[_|T],Relations):-
	relations_only_in_the_object(Object,T,Relations).



%Return the value of an object relation
object_relation_value(Object,Relation,KB,Value):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	find_value_relation(Relation,Relations,Value).

object_relation_value(_,_,_,unknown).



%Regresa las clases hijo de una clase
%validando antes la existencia de ésta
hijos_clase(_,_,unknown).

hijos_clase(C,KB,Res):-
	existencia_clase(C,KB,yes),
	hijos_clase_recur(C,KB,Res).

%Regresa las clases hijo de una clase 
%recorriendo la lista de la KB
%Caso base:
%Los hijos de una clase buscados en una lista vacía,
%serán una lista vacía
hijos_clase_recur(_,[],[]).
%Caso recursivo:
%Si el nombre de la clase madre de la clase que se encuentra en el Head de la lista
%es C (la clase que se busca), continuamos procesando el Tail hasta llegar al caso base
%resolviendo las llamadas recursivas y concatenando en cada resolución a Hijo (la clase hijo) y Hermanos (sus hermanos)
%produciendo la lista de Hijos al final
hijos_clase_recur(C,[class(Hijo,C,_,_,_)|T],Hijos):-
	hijos_clase_recur(C,T,Hermanos),	
	append([Hijo],Hermanos,Hijos).
%Caso recursivo:
%Si el nombre de la clase madre de la clase que se encuentra en el Head de la lista es 
%cualquier cosa distinta de C, continuamos procesando el Tail de la lista
hijos_clase_recur(C,[class(_,_,_,_,_)|T],Hijos):-
	hijos_clase_recur(C,T,Hijos).	
	

%Regresa los hijos de una lista de clases
%Caso base:
%	Los hijos de una lista de clases vacía son una lista vacía
hijos_clases([],_,[]).
%Caso recursivo:
%	Los hijos de una lista de clases (asumiendo que tienen el mismo padre)
%	son los hijos de la clase en el Head la lista y los hijos del Tail de la lista (los hijos de los hermanos)
%	es decir es una lista de primos
hijos_clases([C|T],KB,Primos):-
	hijos_clase_recur(C,KB,Hijos),
	hijos_clases(T,KB,Hijos_de_Hermanos),
	append(Hijos_de_Hermanos,Hijos,Primos).



%Regresa los ids de objetos de una clase respetando la cerradura de la relación de herencia.
%Los ids de objetos de una clase existen si dicha clase existe,
%si la clase (C) existe, obtenemos los objetos que pertenecen a ésta (Objetos_Clase),
%obtenemos todas las clases descendientes de C (Descendientes) y de esas clases a su vez obtenemos
%todos los objetos que pertenecen a ellas (Objetos_Descendientes)
%finalmente concatenamos Objetos_Clase y Objetos_Descendientes para obtener la lista final de objetos (Objetos)
objetos_de_una_clase(C,KB,Objetos):-
	existencia_clase(C,KB,yes),
	objetos_clase(C,KB,Objetos_Clase),
	descendientes_clase(C,KB,Descendientes),
	objetos_clases(Descendientes,KB,Objetos_Descendientes),
	append(Objetos_Clase,Objetos_Descendientes,Objetos).
%Si hay algún fallo en la consulta del predicado anterior (el predicado es falso), regresamos unknown (no sé)
%queriendo decir que no se encontró la clase o su negado.
objetos_de_una_clase(_,_,unknown).

%Regresa los ids de objetos únicamente dentro de una clase específica
%Caso base:
%Para una KB vacía, se regresa que no se sabe si hay objetos dentro de la 
%clase especificada para evitar la negación por falla.
objetos_clase(_,[],unknown).
%Caso recursivo:
%Si el nombre de la clase en el Head de la KB es el mismo que C (nombre la clase que se está buscando),
%extraemos los ids de los individuos de esta clase y los regresamos
objetos_clase(C,[class(C,_,_,_,O)|_],Objs):-
	ids_individuos(O,Objs).
%Caso recursivo:
%Si el nombre de la clase en el Head de la KB es cualquier otra cosa que C (nombre de la clase que se está buscando),
%continuamos procesando el Tail de la lista
objetos_clase(C,[class(_,_,_,_,_)|T],Objects):-
	objetos_clase(C,T,Objects).

%Obtiene los ids de los individuos dentro de una lista de individuos
%Caso base:
%La lista de individuos está vacía, por lo tanto no hay ids de individuos
ids_individuos([],[]).
%Caso recursivo:
%Obtenemos el id del Head de la lista de elementos y continuamos procesando 
%el Tail, hasta que se llegue al caso base, en ese momento se resuelven todas las 
%llamadas recursivas y se concatenaran todos los ids de individuos, regresando al final el resultado
ids_individuos([[id=>Nombre,_,_]|T],Objs):-
	ids_individuos(T,Rest),
	append([Nombre],Rest,Objs).

%Obtiene los descendientes de una clase
%Una clase tiene descendientes si existe,
%Si una clase existe, 
%sus descendientes son todos los descendientes de sus hijos.
descendientes_clase(C,KB,Descendientes):-
	existencia_clase(C,KB,yes),
	hijos_clase_recur(C,KB,Hijos),
	descendientes_clase_recur(Hijos,KB,Descendientes).
%Cualquier fallo en la operación regresamos: no sé
descendientes_clase(_,_,unknown).

%Obtiene todos los descendientes de una lista de clases
%Caso base:
%Los descendientes de una lista de clases vacía son una lista vacía
descendientes_clase_recur([],_,[]).
%Caso recursivo:
%Los descendientes (Descendientes) de una lista de clases (Cs) son los hijos de esas clases (Hijos)
%y los descendientes de esos hijos (Descendientes_de_hijos) 
descendientes_clase_recur(Cs,KB,Descendientes):-
	hijos_clases(Cs,KB,Hijos),
	descendientes_clase_recur(Hijos,KB,Descendientes_de_hijos),
	append(Cs,Descendientes_de_hijos,Descendientes).

%Obtiene los individuos (u objetos) dentro de una lista de clases
%Caso base:
%	Los objetos dentro de una lista de clases vacía son una lista vacía
objetos_clases([],_,[]).
%Caso recursivo:
%	Los objetos de una lista de clases (Objetos_Clases) son 
%   la concatenacion de la lista de los objetos dentro de la clase en el Head de la lista (Objs)
%   y la lista de los objetos del tail de la lista (Objetos)
objetos_clases([C|T],KB,Objetos_Clases):-
	objetos_clase(C,KB,Objs),
	objetos_clases(T,KB,Objetos),
	append(Objs,Objetos,Objetos_Clases).

%Eliminate null prop
eliminate_null_property([],[]).

eliminate_null_property([_:unknown|T],NewT):-
	eliminate_null_property(T,NewT).

eliminate_null_property([X:Y|T],[X:Y|NewT]):-
	eliminate_null_property(T,NewT).

%Expand classes to objects
expand_classes_to_objects([],[],_).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>Objects)|NewT],KB):-
	existencia_clase(Y,KB,yes),
	objetos_de_una_clase(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>Objects|NewT],KB):-
	existencia_clase(Y,KB,yes),
	objetos_de_una_clase(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>[Y])|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>[Y]|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).

%Filter objects with property
filter_objects_with_property(_,_,[],[]).

filter_objects_with_property(KB,Property,[H|T],[H:Value|NewT]):-
	object_property_value(H,Property,KB,Value),
	filter_objects_with_property(KB,Property,T,NewT).