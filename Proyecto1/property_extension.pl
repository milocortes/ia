% :- module(property_extension, [property_extension/3]).
:- use_module(consult_ops).

% Property extension
% property_extension(Property,KB,Result):-
% 	nombre_objetos_clase_herencia(top,KB,AllObjects),
% 	filter_objects_with_property(KB,Property,AllObjects,Objects),
% 	eliminate_null_property(Objects,Result).


property_extension(Prop, KB, Res):-
	mapea_todo_objeto_propiedades(KB, Objs_Props_Map),
	filtra_obj_props(Objs_Props_Map, Prop, KB, Res).
