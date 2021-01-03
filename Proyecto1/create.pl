:- module(create, [add_class/4,
                   add_class_property/5,
                   add_class_relation/5,
                   add_object/4,
                   add_object_property/5,
                   add_object_relation/5]).
:- use_module(utils).


%%%%%%%%%%%%%%%%%%%%%%% Operaciones para agregar clases, objetos, propiedades y relaciones en la base de conocimiento %%%%%%%%%%%%%%%%%%%%%%%%%%5


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar una nueva clase ++++++++++++++++++++++++++++++++++++++++++++++

add_class(NewClass,Mother,OriginalKB,NewKB) :-
	not(existencia_clase(NewClass,OriginalKB, no)),
	not(existencia_clase(NewClass,OriginalKB, yes)),
	not(existencia_objeto(NewClass,OriginalKB,no)),
	not(existencia_objeto(NewClass,OriginalKB,yes)),
	existencia_clase(Mother,OriginalKB, yes),
	append(OriginalKB,[class(NewClass,Mother,[],[],[])],NewKB).


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar nueva propiedad de clase ++++++++++++++++++++++++++++++++++++++++++++++

add_class_property(Class,NewProperty,Value,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	append_property(Props,NewProperty,Value,NewProps).

append_property(Props,NewProperty,yes,NewProps):-
	append(Props,[NewProperty],NewProps).

append_property(Props,NewProperty,no,NewProps):-
	append(Props,[not(NewProperty)],NewProps).

append_property(Props,NewProperty,Value,NewProps):-
	append(Props,[NewProperty=>Value],NewProps).


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar nueva relación de clase ++++++++++++++++++++++++++++++++++++++++++++++

add_class_relation(Class,NewRelation,OtherClass,OriginalKB,NewKB) :-
	existencia_clase(Class,OriginalKB, yes),
	existencia_clase(OtherClass,OriginalKB, yes),
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	append_relation(Rels,NewRelation,OtherClass,NewRels).

append_relation(Rels,not(NewRelation),OtherClass,NewRels):-
	append(Rels,[not(NewRelation=>OtherClass)],NewRels).

append_relation(Rels,NewRelation,OtherClass,NewRels):-
	append(Rels,[NewRelation=>OtherClass],NewRels).


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar un nuevo objeto ++++++++++++++++++++++++++++++++++++++++++++++

add_object(NewObject,Class,OriginalKB,NewKB) :- 
	not(existencia_clase(NewObject,OriginalKB, no)),
	not(existencia_clase(NewObject,OriginalKB, yes)),
	not(existencia_objeto(NewObject,OriginalKB,no)),
	not(existencia_objeto(NewObject,OriginalKB,yes)),
	existencia_clase(Class,OriginalKB, yes),
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	append(Objects,[[id=>NewObject,[],[]]],NewObjects).


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar nueva propiedad de objeto ++++++++++++++++++++++++++++++++++++++++++++++

add_object_property(Object,NewProperty,Value,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	verifica_elem([id=>Object,Properties,Relations],Objects),
	cambiar_elem([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	append_property(Properties,NewProperty,Value,NewProperties).


%++++++++++++++++++++++++++++++++++++++++++++++ Agregar nueva relacion de objeto ++++++++++++++++++++++++++++++++++++++++++++++

add_object_relation(Object,NewRelation,OtherObject,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	verifica_elem([id=>Object,Properties,Relations],Objects),
	cambiar_elem([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	append_relation(Relations,NewRelation,OtherObject,NewRelations).

