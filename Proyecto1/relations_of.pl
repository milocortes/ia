:- module(relations_of,[relations_of_individual/3]).
:- use_module(consult_ops).

% Relations of individual

relations_of_individual(Object,KB,ExpandedRelations):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	expand_classes_to_objects(Relations,ExpandedRelations,KB).

relations_of_individual(_,_,unknown).
