:- module(class_extension,[extension_clase_nombres/3,
						   extension_clase_objetos/3]).
:- use_module(consult_ops).

%Class extension
extension_clase_nombres(C,KB,Objs):-
	nombre_objetos_clase_herencia(C,KB,Objs).	

extension_clase_objetos(C,KB,Objs):-
	objetos_clase_herencia(C,KB,Objs).	
	