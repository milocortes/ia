:- module(create, [add_class/4,
                   add_class_property/5,
                   add_class_relation/5,
                   add_object/4,
                   add_object_property/5,
                   add_object_relation/5]).
:- use_module(utils).

%--------------------------------------------------------------------------------------------------
%Operations for adding classes, objects or properties into the Knowledge Base
%--------------------------------------------------------------------------------------------------

%Add new class

add_class(NewClass,Mother,OriginalKB,NewKB) :-
	append(OriginalKB,[class(NewClass,Mother,[],[],[])],NewKB).

%Add new class property

add_class_property(Class,NewProperty,Value,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,NewProps,Rels,Objects),OriginalKB,NewKB),
	append_property(Props,NewProperty,Value,NewProps).

append_property(Props,NewProperty,yes,NewProps):-
	append(Props,[NewProperty],NewProps).

append_property(Props,NewProperty,no,NewProps):-
	append(Props,[not(NewProperty)],NewProps).

append_property(Props,NewProperty,Value,NewProps):-
	append(Props,[NewProperty=>Value],NewProps).


%Add new class relation

add_class_relation(Class,NewRelation,OtherClass,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,NewRels,Objects),OriginalKB,NewKB),
	append_relation(Rels,NewRelation,OtherClass,NewRels).

append_relation(Rels,not(NewRelation),OtherClass,NewRels):-
	append(Rels,[not(NewRelation=>OtherClass)],NewRels).

append_relation(Rels,NewRelation,OtherClass,NewRels):-
	append(Rels,[NewRelation=>OtherClass],NewRels).


%Add new object

add_object(NewObject,Class,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	append(Objects,[[id=>NewObject,[],[]]],NewObjects).


%Add new object property

add_object_property(Object,NewProperty,Value,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,NewProperties,Relations],Objects,NewObjects),
	append_property(Properties,NewProperty,Value,NewProperties).


%Add new object relation

add_object_relation(Object,NewRelation,OtherObject,OriginalKB,NewKB) :-
	changeElement(class(Class,Mother,Props,Rels,Objects),class(Class,Mother,Props,Rels,NewObjects),OriginalKB,NewKB),
	isElement([id=>Object,Properties,Relations],Objects),
	changeElement([id=>Object,Properties,Relations],[id=>Object,Properties,NewRelations],Objects,NewObjects),
	append_relation(Relations,NewRelation,OtherObject,NewRelations).

