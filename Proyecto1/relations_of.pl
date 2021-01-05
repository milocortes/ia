%:- module(relations_of,[relations_of_individual/3]).
:- use_module(consult_ops).

% Relations of individual

relations_of_individual(Object,KB,ExpandedRelations):-
	relaciones_de_un_objeto(Object,KB,Relations),
	expandir_relaciones_clase(Relations,KB, ExpandedRelations).

relations_of_individual(_,_,unknown).
