:- module(modify, [change_class_name/4,
                   change_object_name/4,
                   change_value_class_property/5,
                   change_value_object_property/5,
                   change_relations/4,
                   change_relations_with_object/4]).

:- use_module(utils).
:- use_module(create).
:- use_module(delete).


%%%%%%%%%%%%%%%%%%%%%%% Operaciones para modificar clases, objetos, propiedades y relaciones en la base de conocimiento %%%%%%%%%%%%%%%%%%%%%%%%%%5


%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar una propiedad del Objeto ++++++++++++++++++++++++++++++++++++++++++++++
change_value_object_property(Object,Property,NewValue,KB,NewKB):-
	existencia_objeto(Object,KB,yes),
	elimina_propiedad_objeto(Object,Property,KB,TemporalKB), % eliminamos la propiedad que le mandamos
	add_object_property(Object,Property,NewValue,TemporalKB,NewKB). %agregamos de nuevo la propiedad pero con el valor nuevo.


%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar valor de la relación del Objeto ++++++++++++++++++++++++++++++++++++++++++++++
change_value_object_relation(Object,Relation,NewObjectRelated,KB,NewKB):-
	existencia_objeto(Object,KB,yes),
	remueve_relacion_objeto(Object,Relation,KB,TemporalKB),
	add_object_relation(Object,Relation,NewObjectRelated,TemporalKB,NewKB).


%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar propiedad de una clase ++++++++++++++++++++++++++++++++++++++++++++++
change_value_class_property(Class,Property,NewValue,KB,NewKB):-
	existencia_clase(Class,KB, yes),
	elimina_propiedad_clase(Class,Property,KB,TemporalKB),
	add_class_property(Class,Property,NewValue,TemporalKB,NewKB).


%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar valor de la relación de una clase ++++++++++++++++++++++++++++++++++++++++++++++

change_value_class_relation(Class,Relation,NewClassRelated,KB,NewKB):-
	existencia_clase(Class,KB, yes),
	existencia_clase(NewClassRelated,KB, yes),
	elimina_relacion_clase(Class,Relation,KB,TemporalKB),
	add_class_relation(Class,Relation,NewClassRelated,TemporalKB,NewKB).


%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar el nombre de un Objeto ++++++++++++++++++++++++++++++++++++++++++++++

change_object_name(Object,NewName,OriginalKB,NewKB) :-
	existencia_objeto(Object,OriginalKB,yes),
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	verifica_elem([id=>Object|Properties],Objects),
	cambiar_elem([id=>Object|Properties],[id=>NewName|Properties],Objects,NewObjects),
	change_relations_with_object(Object,NewName,TemporalKB,NewKB).
	
change_relations_with_object(_,_,[],[]).

change_relations_with_object(Object,NewName,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	change_relations(Object,NewName,O,NewO),
	change_relation(Object,NewName,R,NewR), %no tiene mucho sentido, pero hay que darselo
	change_relations_with_object(Object,NewName,T,NewT).

change_relations(_,_,[],[]).

change_relations(Object,NewName,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	change_relation(Object,NewName,R,NewR),
	change_relations(Object,NewName,T,NewT).

change_relation(_,_,[],[]).

change_relation(OldName,NewName,[R=>OldName|T],[R=>NewName|NewT]):-
	change_relation(OldName,NewName,T,NewT).

change_relation(OldName,NewName,[not(R=>OldName)|T],[not(R=>NewName)|NewT]):-
	change_relation(OldName,NewName,T,NewT).

change_relation(OldName,NewName,[H|T],[H|NewT]):-
	change_relation(OldName,NewName,T,NewT).

%++++++++++++++++++++++++++++++++++++++++++++++ Cambiar el nombre de una Clase ++++++++++++++++++++++++++++++++++++++++++++++

change_class_name(Class,NewName,KB,NewKB):-
	existencia_clase(Class,KB, yes),
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(NewName,Mother,Props,Rels,Objects),KB,TemporalKB),
	changeMother(Class,NewName,TemporalKB,TemporalKB2),
	change_relations_with_object(Class,NewName,TemporalKB2,NewKB). %hay que revisar por qué sí hace sentido o por qué no lo hace,

changeMother(_,_,[],[]).
changeMother(OldMother,NewMother,[class(C,OldMother,P,R,O)|T],[class(C,NewMother,P,R,O)|N]):-
    changeMother(OldMother,NewMother,T,N).
changeMother(OldMother,NewMother,[H|T],[H|N]):-
    changeMother(OldMother,NewMother,T,N).