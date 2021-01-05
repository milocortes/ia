:- module(consult_ops, [objetos_de_una_clase/3,
						objetos_clase_herencia/3,
						mapea_lista_objetos_propiedades/3,
						filtra_obj_props/4,
						filtra_obj_rels/4,
						mapea_todo_objeto_propiedades/2,
						nombre_objetos_clase_herencia/3,
						objetos_clase/3,
						filter_objects_with_property/4,
						object_property_value/4,
						eliminate_null_property/2,
						object_relation_value/4,
						existencia_clase/3,
						existencia_objeto/3,
						clase_de_objeto/3,
						class_ancestors/3,
						propiedades_de_un_objeto/3,
						expand_classes_to_objects/3,
						relaciones_de_un_objeto/3,
						propiedades_de_una_clase/3,
						propiedades_ancestros/3,
						expandir_relaciones_clase/3,
						mapea_todo_objeto_relaciones/2,
						extension_clase_nombres/3,
						extension_clase_objetos/3,
						classes_of_individual/3,
						relation_extension/3,
						relations_of_individual/3,
						property_extension/3,
						propiedades_individuo/3,
						class_properties/3,
						relations_of_class/3]).
:- use_module(utils).


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
existencia_objeto(_,[],unknown).

existencia_objeto(Object,[class(_,_,_,_,O)|_],no):-
	member([id=>not(Object),_,_],O).

existencia_objeto(Object,[class(_,_,_,_,O)|_],yes):-
	member([id=>Object,_,_],O).

existencia_objeto(Object, [class(_,_,_,_,O)|_], Answer):-
	existencia_objeto_lista_ids(Object, O, Answer).


existencia_objeto(Object,[class(_,_,_,_,_)|T],Answer):-
	existencia_objeto(Object,T,Answer).


existencia_objeto_lista_ids(Object, [Ind|_], yes):-
	evalua_lista_nombres_clase(Object, Ind).

existencia_objeto_lista_ids(Object, [Ind|_], no):-
	evalua_lista_nombres_clase(not(Object), Ind).

existencia_objeto_lista_ids(Object, [_|T], Answer):-
	existencia_objeto_lista_ids(Object, T, Answer).


%Obtiene el nombre de la clase madre de una clase a partir de su nombre
%Busca en una lista de clases (KB), una clase (Class)
%y en cuanto la encuentra, regresa el nombre de la clase madre (Mother)
%Caso base:
%	La clase madre de una clase, dada una lista vacía de clases, debe responderse con 'no sé'
mother_of_a_class(_,[],unknown).
%Caso base:
%	El nombre de la clase (Class) unifica con el nombre de la clase en el Head de la lista de clases
%	Y se hace un binding de Mother con el nombre de la clase madre de la clase en el Head
mother_of_a_class(Class,[class(Class,Mother,_,_,_)|_],Mother).
%Caso recursivo:
%	El nombre de la clase (Class) no unifica con el nombre de la clase en el Head de la lista de clases,
%	Entonces se prosigue a seguir buscando en el Tail de la lista.
mother_of_a_class(Class,[class(_,_,_,_,_)|T],Mother):-
	mother_of_a_class(Class,T,Mother).

propiedades_ancestros(C, KB, Props):-
	existencia_clase(C,KB,yes),
	lista_props_ancestros(C,KB,Props).	

propiedades_ancestros(C,KB,unknown):-
	existencia_clase(C,KB,unknown).

lista_props_ancestros(none,_,[]).

lista_props_ancestros(C,KB,[Props|T]):-
	mother_of_a_class(C,KB,M),
	lista_props_ancestros(M,KB,T),
	propiedades_clase(M,KB,Props).


relaciones_ancestros(C, KB, Rels):-
	existencia_clase(C,KB,yes),
	lista_rels_ancestros(C,KB,Rels).	

relaciones_ancestros(C,KB,unknown):-
	existencia_clase(C,KB,unknown).

lista_rels_ancestros(none,_,[]).

lista_rels_ancestros(C,KB,[Relaciones|T]):-
	mother_of_a_class(C,KB,M),
	lista_rels_ancestros(M,KB,T),
	relaciones_clase(M,KB,Relaciones).

%Obtiene los ancestros de la clase
%	-Valida la existencia de la clase
%	-Si la clase existe, se obtiene la lista de todos sus ancestros
class_ancestors(Class,KB,ClassAncestors):-
	existencia_clase(Class,KB,yes),
	list_of_ancestors(Class,KB,ClassAncestors).
%Si el predicado anterior falló, claramente habrá sido porque no existe la clase en su forma 
%afirmativa, por lo tanto se busca si no existe tampoco en su forma negativa para responder no sé y evitar la negación por falla
%Cabe recalcar que si éste predicado falla (y puesto que el anterior también fallo, para que se haya tenido que evaluar éste)
%querrá decir que la clase se encuentra en su forma negada, por lo tanto una evaluación falsa implica que la clase se encuentre
%en su forma negada.
class_ancestors(Class,KB,unknown):-
	existencia_clase(Class,KB,unknown).

%Obtiene una lista de todos los ancestros de una clase
%Caso base:
%	La clase top no tiene ancestros
list_of_ancestors(top,_,[]).
%Caso recursivo:
%	Obtenemos el nombre de la clase madre (Mother) de la clase (Class)
%	Proseguimos a buscar a los ancestros de la clase madre recursivamente
%	Hasta que finalmente se llegue al caso base y se resuelva cada llamada recursiva,
%	llenando, con cada resolución, la lista de ancestros, haciendo así el binding de la lista
%	final de ancestros (Ancestors) en la primera evaluación del predicado
list_of_ancestors(Class,KB,Ancestors):-
	mother_of_a_class(Class,KB,Mother),
	list_of_ancestors(Mother,KB,GrandParents),
	append([Mother],GrandParents,Ancestors).

%Obtiene las propiedades de una clase
%	Para cualquier clase dentro de la jerarquía que sea distinta de top
%	- Se valida la existencia de la clase.
%	- Se obtienen las propiedades directamente dentro de la clase
%	- Se obtienen las propiedades de las clases ancestros
%	- Se concatenan todas las propiedades de cada ancestro para obtener todas las propiedades
%	  asociadas a la clase
%	- Elimina todas las propiedades repetidas que se hayan obtenido del predicado anterior
%	  y aplica el principio de especificidad para eliminar propiedades contradictorias de niveles
%	  más abstractos de la jerarquía
propiedades_de_una_clase(C,KB,Props):-
	existencia_clase(C,KB,yes),
	propiedades_clase(C, KB, CProps),
	propiedades_ancestros(C, KB, AncestrosProps),
	append(CProps, AncestrosProps, AllProperties),
	cancel_repeated_property_values(AllProperties,Props).

propiedades_de_una_clase(Class,KB,unknown):-
	existencia_clase(Class,KB,unknown).

%Obtiene las propiedades de una clase especifica
%Caso base:
%	Las propiedades de una clase en una lista vacía
%	son una lista vacía
propiedades_clase(_,[],[]).
%Caso base:
%	La clase con el nombre de clase para la cuál se buscan sus propiedades (Class)
%	se encuentra en el Head de la lista (se unifican) y se hace el binding de Properties
propiedades_clase(C,[class(C,_,Props,_,_)|_],Props).
%Caso recursivo:
%	La clase en el Head no es la que se busca, se sigue evaluando el Tail de la lista.
propiedades_clase(C,[class(_,_,_,_,_)|T],Props):-
	propiedades_clase(C,T,Props).

%Concatena las propiedades de varias clases
%Caso base: 
%Las propiedades de una lista de nombres de clase vacía son una lista vacía
concat_ancestors_properties([],_,[]).
%Caso recursivo:
%Por cada clase dentro de la lista de obtenemos sus propiedades,
%Notese que T2 siempre estará vacía en cada llamada recursiva
%y en el caso base, se llenara el Head de la lista de propiedades
%con lo cual se resolverán todas las llamadas recursivas anteriores
%para finalmente obtener, en la primera evaluación del predicado
%la lista final de propiedades (Properties).
concat_ancestors_properties([Ancestor|T],KB,[Properties|T2]):-
	concat_ancestors_properties(T,KB,T2),
	propiedades_clase(Ancestor,KB,Properties).

%Elimina propiedades duplicadas dentro de una lista de propiedades
%Y elimina complementos lógicos de una propiedad despues de una primera ocurrencia,
%esto porque se espera una lista de propiedades de especificidad descendiente.
%	- Aplana la lista, es decir deposita todos los elementos en una lista anidada en una 
%     lista unidimensional, este predicado espera una lista anidada de propiedades.
%	- Con la obtención de la lista de propiedades aplanada, se procecede a 
%	  efectuar lo que se menciona en la descripción de este predicado.
cancel_repeated_property_values(X,Z):-
	flatten(X, Y),
	delete_repeated_properties(Y,Z).

%Elimina propiedades duplicadas dentro de una lista de propiedades
%Y aplica el principio de especificidad tomando en cuenta que la lista 
%está ordenada descendientemente respecto a especificidad de las propiedades
%Caso base:
%	Borrar duplicados y complementos lógicos de propiedades menos específicas
%	en una lista vacía de propiedades, es una lista vacía de propiedades
delete_repeated_properties([],[]).
%Caso recursivo:
%	El elemento en el Head de la lista es un par propiedad-valor.
%		- De nuevo, como se asume que la lista está ordenada por especificidad
%		  descendiente, eliminamos toda propiedad igual a la que tenemos en el Head
%		  del Tail de la lista
%		- También eliminamos toda negación de la propiedad-valor que tenemos en el Head
%		  del Tail de la lista
%		- Continuamos procesando el Tail de la lista
%	Nótese que se efectúa la concatencación de cada par cuando se resuelven las llamadas
%	recursivas, para finalmente hacer el binding a la lista de salida
delete_repeated_properties([P=>V|T],[P=>V|NewT]):-
	eliminar_elem(P=>_,T,L1),
	eliminar_elem(not(P=>V),L1,L2),
	delete_repeated_properties(L2,NewT).
%Caso recursivo:
%	El elemento en el Head de la lista es un par propiedad-valor negado.
%		- Eliminamos toda propiedad igual a la que tenemos en el Head,
%		  (en este caso el negado de la propiedad) del Tail de la lista
%		- También eliminamos toda negación de la propiedad-valor que tenemos en el Head
%		  del Tail de la lista (En este caso el negado de la propiedad-valor, es la propiedad-valor)
%		- Continuamos procesando el Tail de la lista
delete_repeated_properties([not(P=>V)|T],[not(P=>V)|NewT]):-
	eliminar_elem(not(P=>_),T,L1),
	eliminar_elem(P=>V,L1,L2),
	delete_repeated_properties(L2,NewT).
%Caso recursivo:
%	El elemento en el Head de la lista es una propiedad atómica negada.
%		- Eliminamos toda propiedad negada igual a la que tenemos en el Head,
%		  del Tail de la lista
%		- También eliminamos toda propiedad atómica que tenemos en el Head,
%		  del Tail de la lista
%		- Continuamos procesando el Tail de la lista
delete_repeated_properties([not(H)|T],[not(H)|NewT]):-
	eliminar_elem(not(H),T,L1),
	eliminar_elem(H,L1,L2),
	delete_repeated_properties(L2,NewT).
%Caso recursivo:
%	El elemento en el Head de la lista es un propiedad atómica.
%		- Eliminamos toda propiedad igual a la que tenemos en el Head
%		  del Tail de la lista
%		- También eliminamos toda negación de la propiedad atómica que tenemos en el Head,
%		  del Tail de la lista
%		- Continuamos procesando el Tail de la lista
delete_repeated_properties([H|T],[H|NewT]):-
	eliminar_elem(H,T,L1),
	eliminar_elem(not(H),L1,L2),
	delete_repeated_properties(L2,NewT).



%Verify if a class has a specific property
class_has_property(Class,Property,KB,Answer):-
	propiedades_de_una_clase(Class,KB,Properties),
	incomplete_information(Property,Properties,Answer).

incomplete_information(_,[], unknown).

incomplete_information(Atom, List, yes):- member(Atom,List).

incomplete_information(not(Atom), List, no):- member(Atom,List).

incomplete_information(Atom, List, no):- member(not(Atom),List).

incomplete_information(_, _, unknown).



%Return the value of a class property
class_property_value(Class,Property,KB,Value):-
	propiedades_de_una_clase(Class,KB,ClassProperties),
	find_value(Property,ClassProperties,Value).

find_value(_,[],unknown).

find_value(Attribute,[Attribute=>Value|_],Value).

find_value(Attribute,[not(Attribute)|_],no).

find_value(Attribute,[Attribute|_],yes).

find_value(Attribute,[_|T],Value):-
	find_value(Attribute,T,Value).



%Obtiene la clase de un objeto dado su nombre
%Caso base:
%	La clase de un objeto es desconocida si la KB
%	es vacía, evitando la negación por falla
clase_de_objeto(_,[],unknown):-!.
%Caso base:
%	El nombre del individuo (Object), se encuentra
%	dentro de la lista de los individuos (O) de la clase (C)
%	en el Head de la lista de clases de la KB
%	por lo tanto C es la clase del individuo
clase_de_objeto(Obj,[class(C,_,_,_,O)|_],C):-
	member([id=>Obj,_,_],O).

clase_de_objeto(Obj,[class(C,_,_,_,O)|_],C):-
	clase_lista_ids(Obj, O).

%Caso recursivo:
%	El nombre del individuo (Object), no se encuentra
%	dentro de la lista de los individuos de la clase
%	(El caso anterior fue falso)
%	Por lo tanto, no nos importan los parámetros
%	de la clase en el Head de la lista y continuamos
%	procesando el Tail (T) de la lista.
clase_de_objeto(Obj,[class(_,_,_,_,_)|T],C):-
	clase_de_objeto(Obj,T,C).

%clase_lista_ids(_, [], _):- false.

clase_lista_ids(Obj, [Ind|_]):-
	evalua_lista_nombres_clase(Obj, Ind).

clase_lista_ids(Obj, [_|T]):-
	clase_lista_ids(Obj, T).

evalua_lista_nombres_clase(Obj, [id => Names, _, _]):-
	member(Obj, Names).

%Regresa una lista de propiedades de un objeto tomando en cuenta
%la cerradura de la relación de herencia y el principio de especificidad.
propiedades_de_un_objeto(Obj,KB,AllProperties):-
	existencia_objeto(Obj,KB,yes),
	propiedades_objeto(Obj,KB,PropsIndividuo),
	propiedades_heredadas(Obj, KB, PropsHeredadas),
	append(PropsIndividuo, PropsHeredadas, Props),
	cancel_repeated_property_values(Props, AllProperties).

propiedades_de_un_objeto(_,_,unknown).

propiedades_heredadas(Obj, KB, Props):-
	clase_de_objeto(Obj,KB,C),
	propiedades_clase(C, KB, CProps),
	propiedades_ancestros(C, KB, AncestrosProps),
	append(CProps, AncestrosProps, Props).

%Regresa las propiedades unicamente dentro del objeto con un id específico
%Nota: 
%	Falta considerar cuando el objeto sea un objeto anónimo
%Caso base: 
%	Las propiedades de un objeto, dada una lista de clases vacía, 
%   sin importar el objeto, son una lista vacía
propiedades_objeto(_,[],[]).

%Caso base:
%	La lista de propiedades (Properties) de un objeto (de nombre Object) 
%   será la lista de propiedades del individuo que a su vez se encuentra dentro de la lista
%   de individuos (O) de una clase cualquiera en el Head de la KB, si efectivamente
%	existe un individuo con ese nombre en la lista de individuos de la clase.
propiedades_objeto(Obj,[class(_,_,_,_,O)|_],Props):-
	member([id=>Obj,Props,_],O).

propiedades_objeto(Obj,[class(_,_,_,_,O)|_], Props):-
	propiedades_lista_ids(Obj, O, Props).

%Caso recursivo:
%	En caso de fallo al encontrar Object en la lista de individuos,
%   sin importarnos ninguno de los campos de la clase en el Head,
%	proseguimos a seguir procesando el Tail de la KB
propiedades_objeto(Obj,[class(_,_,_,_,_)|T],Props):-
	propiedades_objeto(Obj,T,Props).


propiedades_lista_ids(Obj, [Ind|_], Props):-
	evalua_lista_nombres_props(Obj, Ind, Props).

propiedades_lista_ids(Obj, [_|T], Props):-
	propiedades_lista_ids(Obj,T, Props).


evalua_lista_nombres_props(Obj, [id => Names, Props,_], Props):-
	member(Obj, Names).


%Return the value of an object property
object_property_value(Object,Property,KB,Value):-
	existencia_objeto(Object,KB,yes),
	propiedades_de_un_objeto(Object,KB,Properties),
	find_value(Property,Properties,Value).

object_property_value(_,_,_,unknown).



%Consult the relations of a class
relaciones_clase_herencia(top,KB,Relations):-
	relaciones_clase(top,KB,Relations).

relaciones_clase_herencia(Class,KB,Relations):-
	existencia_clase(Class,KB,yes),
	relaciones_clase(Class,KB,ClassRelations),
	list_of_ancestors(Class,KB,Ancestors),
	concat_ancestors_relations(Ancestors,KB,AncestorsRelations),
	append([ClassRelations],AncestorsRelations,AllRelations),
	cancel_repeated_property_values(AllRelations,Relations).

relaciones_clase_herencia(_,_,unknown).


relaciones_clase(_,[],[]).

relaciones_clase(Class,[class(Class,_,_,Relations,_)|_],Relations).

relaciones_clase(Class,[_|T],Relations):-
	relaciones_clase(Class,T,Relations).


concat_ancestors_relations([],_,[]).

concat_ancestors_relations([Ancestor|T],KB,[Relations|NewT]):-
	concat_ancestors_relations(T,KB,NewT),
	relaciones_clase(Ancestor,KB,Relations).



%Return the value of a class relation
class_relation_value(Class,Relation,KB,Value):-
	existencia_clase(Class,KB,yes),
	relaciones_clase_herencia(Class,KB,Relations),
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
relaciones_de_un_objeto(Object,KB,AllRelations):-
	existencia_objeto(Object,KB,yes),
	relaciones_objeto(Object,KB,ObjectRelations),
	relaciones_heredadas(Object, KB, RelsHeredadas),
	append(ObjectRelations,RelsHeredadas,Temp),
	cancel_repeated_property_values(Temp,AllRelations).

relaciones_de_un_objeto(_,_,unknown).

relaciones_heredadas(Obj, KB, Rels):-
	clase_de_objeto(Obj, KB, C),
	relaciones_clase(C, KB, CRels),
	relaciones_ancestros(C, KB, AncestrosRels),
	append(CRels, AncestrosRels, Rels).

relaciones_objeto(_,[],[]).

relaciones_objeto(Object,[class(_,_,_,_,O)|_],Relations):-
	member([id=>Object,_,Relations],O).

relaciones_objeto(Object,[_|T],Relations):-
	relaciones_objeto(Object,T,Relations).


expandir_relaciones_clase([Relacion=>Clase|T], KB, [Relacion=>Objs|T2]):-
	existencia_clase(Clase, KB, yes),
	nombre_objetos_clase_herencia(Clase, KB, Objs),
	expandir_relaciones_clase(T, KB, T2).

expandir_relaciones_clase([not(Relacion=>Clase)|T], KB, [not(Relacion=>Objs)|T2]):-
	existencia_clase(Clase, KB, yes),
	nombre_objetos_clase_herencia(Clase, KB, Objs),
	expandir_relaciones_clase(T, KB, T2).

expandir_relaciones_clase([Relacion=>Ind|T], KB, [Relacion=>Ind|T2]):-
	expandir_relaciones_clase(T, KB, T2).

expandir_relaciones_clase([not(Relacion=>Ind)|T], KB, [not(Relacion=>Ind)|T2]):-
	expandir_relaciones_clase(T, KB, T2).

expandir_relaciones_clase([],_, []).

%Return the value of an object relation
object_relation_value(Object,Relation,KB,Value):-
	existencia_objeto(Object,KB,yes),
	relaciones_de_un_objeto(Object,KB,Relations),
	find_value_relation(Relation,Relations,Value).

object_relation_value(_,_,_,unknown).



%Regresa las clases hijo de una clase
%validando antes la existencia de ésta
hijos_clase(_,_,unknown).

hijos_clase(C,KB,FRes):-
	existencia_clase(C,KB,yes),
	hijos_clase_recur(C,KB,Res),
	flatten(Res, FRes).

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
hijos_clase_recur(C,[class(Hijo,C,_,_,_)|T],[Hijo|T2]):-
	hijos_clase_recur(C,T,T2).	
	%append([Hijo],Hermanos,Hijos).
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
hijos_clases([C|T],KB,[Hijos|T2]):-
	hijos_clase_recur(C,KB,Hijos),
	hijos_clases(T,KB,T2).
	%append(Hijos_de_Hermanos,Hijos,Primos).

nombre_objetos_clase_herencia(C, KB, Objs):-
	existencia_clase(C, KB, yes),
	objetos_clase(C, KB, CObjs),
	nombre_objetos_clases_cerradura([C], KB, PreObjs),
	aplana_un_nivel(PreObjs, FObjs),
	append(CObjs, FObjs, Objs).

nombre_objetos_clase_herencia(_,_,unknown).


nombre_objetos_clases_cerradura([],_,[]).

nombre_objetos_clases_cerradura(Clases, KB, [FObjs|Tail]):-
	hijos_clases(Clases, KB, Hijos),
	flatten(Hijos, FHijos),
	objetos_clases(FHijos, KB, Objs),
	aplana_un_nivel(Objs, FObjs),
	nombre_objetos_clases_cerradura(FHijos, KB, Tail).

nombre_objetos_de_una_clase(C, KB, Objetos):-
	nombre_objetos_clase_herencia(C, KB, Objetos).


objetos_clase_herencia(C, KB, Objs):-
	existencia_clase(C, KB, yes),
	objetos_clase_completos(C, KB, CObjs),
	objetos_clases_cerradura([C], KB, PreObjs),
	aplana_un_nivel(PreObjs, FObjs),
	append(CObjs, FObjs, Objs).

objetos_clase_herencia(_,_,unknown).


objetos_clases_cerradura([],_,[]).

objetos_clases_cerradura(Clases, KB, [FObjs|Tail]):-
	hijos_clases(Clases, KB, Hijos),
	flatten(Hijos, FHijos),
	objetos_clases_completos(FHijos, KB, Objs),
	aplana_un_nivel(Objs, FObjs),
	objetos_clases_cerradura(FHijos, KB, Tail).



clases_cerradura(top,_,[]).

clases_cerradura(C,KB,[M|T]):-
	mother_of_a_class(C,KB,M),
	clases_cerradura(M,KB,T).



objetos_de_una_clase(C, KB, Objetos):-
	objetos_clase_herencia(C, KB, Objetos).
%Regresa los ids de objetos de una clase respetando la cerradura de la relación de herencia.
%Los ids de objetos de una clase pueden existir si dicha clase existe,
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
objetos_clase(C,[class(_,_,_,_,_)|T],Objs):-
	objetos_clase(C,T,Objs).

%Regresa los ids de objetos únicamente dentro de una clase específica
%Caso base:
%Para una KB vacía, se regresa que no se sabe si hay objetos dentro de la 
%clase especificada para evitar la negación por falla.
objetos_clase_completos(_,[],unknown).
%Caso recursivo:
%Si el nombre de la clase en el Head de la KB es el mismo que C (nombre la clase que se está buscando),
%extraemos los ids de los individuos de esta clase y los regresamos
objetos_clase_completos(C,[class(C,_,_,_,O)|_],O).
%Caso recursivo:
%Si el nombre de la clase en el Head de la KB es cualquier otra cosa que C (nombre de la clase que se está buscando),
%continuamos procesando el Tail de la lista
objetos_clase_completos(C,[class(_,_,_,_,_)|T],O):-
	objetos_clase_completos(C,T,O).

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
objetos_clases([C|T],KB,[Objs|T2]):-
	objetos_clase(C,KB,Objs),
	objetos_clases(T,KB,T2).
	%append(Objs,Objetos,Objetos_Clases).

%Obtiene los individuos (u objetos) dentro de una lista de clases
%Caso base:
%	Los objetos dentro de una lista de clases vacía son una lista vacía
objetos_clases_completos([],_,[]).
%Caso recursivo:
%	Los objetos de una lista de clases (Objetos_Clases) son 
%   la concatenacion de la lista de los objetos dentro de la clase en el Head de la lista (Objs)
%   y la lista de los objetos del tail de la lista (Objetos)
objetos_clases_completos([C|T],KB,[Objs|T2]):-
	objetos_clase_completos(C,KB,Objs),
	objetos_clases_completos(T,KB,T2).
	%append(Objs,Objetos,Objetos_Clases).


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


mapea_todo_objeto_propiedades(KB, Objs_Props_Map):-
	nombre_objetos_clase_herencia(top,KB,TodoObjeto),
	mapea_lista_objetos_propiedades(TodoObjeto, KB, Objs_Props_Map).

mapea_lista_objetos_propiedades([],_,[]).

mapea_lista_objetos_propiedades([Obj|T], KB, [Obj_Prop_Map|T2]):-
	extrae_obj_props(Obj, KB, Obj_Prop_Map),
	mapea_lista_objetos_propiedades(T, KB, T2).

extrae_obj_props(Obj, KB, [Obj, Props]):-
	propiedades_de_un_objeto(Obj, KB, Props).

mapea_todo_objeto_relaciones(KB, Objs_Rels_Map):-
	nombre_objetos_clase_herencia(top,KB,TodoObjeto),
	mapea_lista_objetos_relaciones(TodoObjeto, KB, Objs_Rels_Map).

mapea_lista_objetos_relaciones([],_,[]).

mapea_lista_objetos_relaciones([Obj|T], KB, [Obj_Rel_Map|T2]):-
	extrae_obj_rels(Obj, KB, Obj_Rel_Map),
	mapea_lista_objetos_relaciones(T, KB, T2).

extrae_obj_rels(Obj, KB, [Obj, Rels]):-
	relaciones_de_un_objeto(Obj, KB, Rels).


filtra_obj_props([],_,_,[]).

%Verifica propiedad atómica
filtra_obj_props([[Obj, Props]|T], Prop, KB, [Obj|T2]):-
	member(Prop, Props),
	filtra_obj_props(T, Prop, KB, T2).

%Verifica propiedad en forma atributo-valor
filtra_obj_props([[Obj, Props]|T], Prop, KB, [Obj:Valor|T2]):-
	member(Prop=>_, Props),
	obtiene_valor(Prop, Props, Valor),
	filtra_obj_props(T, Prop, KB, T2).

%Verifica propiedad en forma not(atributo-valor)
filtra_obj_props([[Obj, Props]|T], not(Prop), KB, [Obj:Valor|T2]):-
	member(not(Prop=>_), Props),
	obtiene_valor(not(Prop), Props, Valor),
	filtra_obj_props(T, not(Prop), KB, T2).

filtra_obj_props([_|T], Prop, KB, Objs):-
	filtra_obj_props(T, Prop, KB, Objs).


obtiene_valor(_,[],_):-false.

obtiene_valor(Prop,[Prop=>Value|_],Value).

obtiene_valor(not(Prop),[not(Prop=>Value)|_],Value).

obtiene_valor(Prop,[_|T],Value):-
	obtiene_valor(Prop,T,Value).

filtra_obj_rels([],_,_,[]).

filtra_obj_rels([[Obj, Rels]|T], Rel, KB, [Obj:Valor|T2]):-
	member(Rel=>_, Rels),
	obtiene_valor(Rel, Rels, Valor),
	filtra_obj_rels(T, Rel, KB, T2).

filtra_obj_rels([[Obj, Rels]|T], not(Rel), KB, [Obj:Valor|T2]):-
	member(not(Rel=>_), Rels),
	obtiene_valor(not(Rel), Rels, Valor),
	filtra_obj_rels(T, not(Rel), KB, T2).

filtra_obj_rels([_|T], Rel, KB, Objs):-
	filtra_obj_rels(T, Rel, KB, Objs).



%---------------------------------
%Servicios principales
%---------------------------------
%Servicio para extensión de clase
extension_clase_nombres(C,KB,Objs):-
	nombre_objetos_clase_herencia(C,KB,Objs).	

extension_clase_objetos(C,KB,Objs):-
	objetos_clase_herencia(C,KB,Objs).	


%Servicio para clases de un individuo
classes_of_individual(Object,KB,Classes):-
	existencia_objeto(Object,KB,yes),
	clase_de_objeto(Object,KB,C),
	clases_cerradura(C, KB, Clases_Herencia),
	append([C], Clases_Herencia, Classes).

classes_of_individual(_,_,unknown).


%Servicio para obtener la extensión de una relación
relation_extension(Rel, KB, Res):-
	mapea_todo_objeto_relaciones(KB, Objs_Rels_Map),
	filtra_obj_rels(Objs_Rels_Map, Rel, KB, Res).

%Servicio para obtener las relaciones de un individuo
relations_of_individual(Object,KB,ExpandedRelations):-
	relaciones_de_un_objeto(Object,KB,Relations),
	expandir_relaciones_clase(Relations,KB, ExpandedRelations).

relations_of_individual(_,_,unknown).

relations_of_class(C, KB, FClassRelations):-
	existencia_clase(C, KB, yes),
	relaciones_clase(C, KB, CRels),
	relaciones_ancestros(C, KB, AncestrosRels),
	append(CRels, AncestrosRels, ClassRelations),
	flatten(ClassRelations, FClassRelations).

relations_of_class(_,_,unknown).

%Servicio para obtener la extensión de una propiedad
property_extension(Prop, KB, Res):-
	mapea_todo_objeto_propiedades(KB, Objs_Props_Map),
	filtra_obj_props(Objs_Props_Map, Prop, KB, Res).

%Servicio para obtener las propiedades de un individuo
%Regresa una lista con propiedades de un individuo
propiedades_individuo(Object,KB,Properties):-
	propiedades_de_un_objeto(Object,KB,Properties).

class_properties(C, KB, Props):-
	propiedades_de_una_clase(C, KB, Props).