:- module(classes_of, [classes_of_individual/3]).
:-use_module(consult_ops).

%Classes of individual
classes_of_individual(Object,KB,Classes):-
	there_is_object(Object,KB,yes),
	class_of_an_object(Object,KB,X),
	class_ancestors(X,KB,Y),
	append([X],Y,Classes).

classes_of_individual(_,_,unknown).
