:-module(properties_of, [properties_of_individual/3]).
:-use_module(consult_ops).

% Properties of individual

properties_of_individual(Object,KB,Properties):-
	object_properties(Object,KB,Properties).