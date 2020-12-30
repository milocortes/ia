:- module(property_extension, [property_extension/3]).
:- use_module(consult_ops).

% Property extension
property_extension(Property,KB,Result):-
	objects_of_a_class(top,KB,AllObjects),
	filter_objects_with_property(KB,Property,AllObjects,Objects),
	eliminate_null_property(Objects,Result).
