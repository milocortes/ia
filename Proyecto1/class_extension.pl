:- module(class_extension,[extension_clase/3]).
:- use_module(consult_ops).

%Class extension
extension_clase(C,KB,Objs):-
	objetos_de_una_clase(C,KB,Objs).	

