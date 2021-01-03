:-module(properties_of, [propiedades_individuo/3,
						 propiedades_clase/3]).
:-use_module(consult_ops).

%Regresa una lista con propiedades de un individuo
propiedades_individuo(Object,KB,Properties):-
	propiedades_de_un_objeto(Object,KB,Properties).

propiedades_clase(C, KB, Props):-
	propiedades_de_una_clase(C, KB, Props).