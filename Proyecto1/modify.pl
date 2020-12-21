:- module(modify, [change_class_name/4,
                   change_object_name/4,
                   change_value_class_property/5,
                   change_value_object_property/5,
                   change_relations/4,
                   change_relations_with_object/4]).

:- use_module(utils).
:- use_module(create).
:- use_module(delete).

%--------------------------------------------------------------------------------------------------
%Operations for changing classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------


change_value_object_property(Object,Property,NewValue,KB,NewKB):-
	rm_object_property(Object,Property,KB,TemporalKB),
	add_object_property(Object,Property,NewValue,TemporalKB,NewKB).

change_value_object_relation(Object,Relation,NewObjectRelated,KB,NewKB):-
	rm_object_relation(Object,Relation,KB,TemporalKB),
	add_object_relation(Object,Relation,NewObjectRelated,TemporalKB,NewKB).
		
change_value_class_property(Class,Property,NewValue,KB,NewKB):-
	rm_class_property(Class,Property,KB,TemporalKB),
	add_class_property(Class,Property,NewValue,TemporalKB,NewKB).

change_value_class_relation(Class,Relation,NewClassRelated,KB,NewKB):-
	rm_class_relation(Class,Relation,KB,TemporalKB),
	add_class_relation(Class,Relation,NewClassRelated,TemporalKB,NewKB).


%Change the name of an object	

change_object_name(Object,NewName,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,TemporalKB),
	isElement([id=>Object|Properties],Objects),
	changeElement([id=>Object|Properties],[id=>NewName|Properties],Objects,NewObjects),
	change_relations_with_object(Object,NewName,TemporalKB,NewKB).
	
change_relations_with_object(_,_,[],[]).

change_relations_with_object(Object,NewName,[class(C,M,P,R,O)|T],[class(C,M,P,NewR,NewO)|NewT]):-
	change_relations(Object,NewName,O,NewO),
	change_relation(Object,NewName,R,NewR),
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


%Change the name of a class

change_class_name(Class,NewName,KB,NewKB):-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(NewName,Mother,Props,Rels,Objects),KB,TemporalKB),
	changeMother(Class,NewName,TemporalKB,TemporalKB2),
	change_relations_with_object(Class,NewName,TemporalKB2,NewKB).
