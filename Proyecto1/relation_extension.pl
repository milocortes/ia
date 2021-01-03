:- module(relation_extension, [relation_extension/3]).
							   %relation_extension2/3]).
:- use_module(consult_ops).

% Relation extension

% relation_extension(Relation,KB,FinalResult):-
% 	nombre_objetos_clase_herencia(top,KB,AllObjects),
% 	filter_objects_with_relation(KB,Relation,AllObjects,Objects),
% 	eliminate_null_property(Objects,Result),
% 	expanding_classes_into_objects(Result,FinalResult,KB).

% filter_objects_with_relation(_,_,[],[]).

% filter_objects_with_relation(KB,Relation,[H|T],[H:Value|NewT]):-
% 	object_relation_value(H,Relation,KB,Value),
% 	filter_objects_with_relation(KB,Relation,T,NewT).

% expanding_classes_into_objects([],[],_).

% expanding_classes_into_objects([X:Y|T],[X:Objects|NewT],KB):-
% 	existencia_clase(Y,KB,yes),
% 	nombre_objetos_clase_herencia(Y,KB,Objects),
% 	expanding_classes_into_objects(T,NewT,KB).

% expanding_classes_into_objects([X:Y|T],[X:[Y]|NewT],KB):-
% 	expanding_classes_into_objects(T,NewT,KB).


relation_extension(Rel, KB, Res):-
	mapea_todo_objeto_relaciones(KB, Objs_Rels_Map),
	filtra_obj_rels(Objs_Rels_Map, Rel, KB, Res).
