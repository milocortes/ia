:- module(delete, [rm_class/3,
                   rm_class_property/4,
                   rm_class_relation/4,
                   rm_object/3,
                   rm_object_property/4,
				   rm_object_relation/4, 
				   changeMother/4]).

:- use_module(utils).

%--------------------------------------------------------------------------------------------------
%Operations for removing classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------


%Remove a class property

rm_class_property(Class,Property,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	borraTodoElementoConIgualPropiedad(Property,Props,Aux),
	eliminar_elem(not(Property),Aux,Aux2),
	eliminar_elem(Property,Aux2,NewProps).


%Remove a class relation

rm_class_relation(Class,not(Relation),OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	deleteAllElementsWithSameNegatedProperty(Relation,Rels,NewRels).

rm_class_relation(Class,Relation,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	borraTodoElementoConIgualPropiedad(Relation,Rels,NewRels).


%Remove an object property

rm_object_property(Object,Property,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	verifica_elem([id=>Object,Properties,Relations],Objects),
	cambiar_elem([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	borraTodoElementoConIgualPropiedad(Property,Properties,Aux),
	eliminar_elem(not(Property),Aux,Aux2),
	eliminar_elem(Property,Aux2,NewProperties).


%Remove an object relation

rm_object_relation(Object,not(Relation),OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	verifica_elem([id=>Object,Properties,Relations],Objects),
	cambiar_elem([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	deleteAllElementsWithSameNegatedProperty(Relation,Relations,NewRelations).

rm_object_relation(Object,Relation,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	verifica_elem([id=>Object,Properties,Relations],Objects),
	cambiar_elem([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	borraTodoElementoConIgualPropiedad(Relation,Relations,NewRelations).
	

%Remove an object

rm_object(Object,OriginalKB,NewKB) :-
	cambiar_elem(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	verifica_elem([id=>Object|Properties],Objects),
	eliminar_elem([id=>Object|Properties],Objects,NewObjects),
	delete_relations_with_object(Object,TemporalKB,NewKB).
	
delete_relations_with_object(_,[],[]).

delete_relations_with_object(Object,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,O,NewO),
	delete_relations_with_object(Object,T,NewT).

del_relations(_,[],[]).

del_relations(Object,[[id=>N,P,R]|T],[[id=>N,P,NewR]|NewT]):-
	cancel_relation(Object,R,NewR),
	del_relations(Object,T,NewT).

cancel_relation(_,[],[]).

cancel_relation(Object,[_=>Object|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[not(_=>Object)|T],NewT):-
	cancel_relation(Object,T,NewT).

cancel_relation(Object,[H|T],[H|NewT]):-
	cancel_relation(Object,T,NewT).


% Remove a class

rm_class(Class,OriginalKB,NewKB) :-
	eliminar_elem(class(Class,Mother,_,_,_),OriginalKB,TemporalKB),
	changeMother(Class,Mother,TemporalKB,TemporalKB2),
	delete_relations_with_object(Class,TemporalKB2,NewKB).

changeMother(_,_,[],[]).

changeMother(OldMother,NewMother,[class(C,OldMother,P,R,O)|T],[class(C,NewMother,P,R,O)|N]):-
	changeMother(OldMother,NewMother,T,N).

changeMother(OldMother,NewMother,[H|T],[H|N]):-
	changeMother(OldMother,NewMother,T,N).
