:-module(properties_of, [propiedades_individuo/3]).
:-use_module(consult_ops).

%Regresa una lista con propiedades de un individuo
propiedades_individuo(Object,KB,Properties):-
	object_properties(Object,KB,Properties).