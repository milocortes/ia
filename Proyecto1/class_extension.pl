:- module(class_extension,[class_extension/3]).
:- use_module(consult_ops).

%Class extension
class_extension(Class,KB,Objects):-
	objects_of_a_class(Class,KB,Objects).	

