:- use_module(utils).
:- use_module(create).
:- use_module(delete).
:- use_module(modify).

%--------------------------------------------------
% Load and Save from files
%--------------------------------------------------


%KB open and save

open_kb(Route,KB):-
	open(Route,read,Stream),
	readclauses(Stream,X),
	close(Stream),
	atom_to_term_conversion(X,KB).

save_kb(Route,KB):-
	open(Route,write,Stream),
	writeq(Stream,KB),
	close(Stream).

readclauses(InStream,W) :-
        get0(InStream,Char),
        checkCharAndReadRest(Char,Chars,InStream),
	atom_chars(W,Chars).
 

checkCharAndReadRest(-1,[],_) :- !.  % End of Stream	
checkCharAndReadRest(end_of_file,[],_) :- !.

checkCharAndReadRest(Char,[Char|Chars],InStream) :-
        get0(InStream,NextChar),
        checkCharAndReadRest(NextChar,Chars,InStream).

atom_to_term_conversion(ATOM, TERM) :-
	 atom(ATOM),
	 atom_to_chars(ATOM,STR),
	 atom_to_chars('.',PTO),
	 append(STR,PTO,STR_PTO),
	 read_from_chars(STR_PTO,TERM).


%--------------------------------------------------------------------------------------------------
% Pruebas
%--------------------------------------------------------------------------------------------------
pruebas :- 
    open_kb('KB.txt', KB),
    add_class(taco, food, KB, NewKB),
    there_is_class(taco, KB, Answer),
    class_extension(food, NewKB, Result),
    write('KB: '),
	write(KB),
    write('NewKB: '),
    write(NewKB),
    write('Food class extension: '),
    write(Result).

%--------------------------------------------------------------------------------------------------
%Operations for consulting 
%--------------------------------------------------------------------------------------------------


%Verify if a class exists

there_is_class(_,[],unknown).

there_is_class(Class,[class(not(Class),_,_,_,_)|_],no).

there_is_class(Class,[class(Class,_,_,_,_)|_],yes).

there_is_class(Class,[_|T],Answer):-
	there_is_class(Class,T,Answer).



%Verify if an object exists

there_is_object(_,[],unknown).

there_is_object(Object,[class(_,_,_,_,O)|_],no):-
	verifica_elem([id=>not(Object),_,_],O).

there_is_object(Object,[class(_,_,_,_,O)|_],yes):-
	verifica_elem([id=>Object,_,_],O).

there_is_object(Object,[_|T],Answer):-
	there_is_object(Object,T,Answer).



%Consult the mother of a class

mother_of_a_class(_,[],unknown).

mother_of_a_class(Class,[class(Class,Mother,_,_,_)|_],Mother).

mother_of_a_class(Class,[_|T],Mother):-
	mother_of_a_class(Class,T,Mother).



%Consult the ancestors of a class

class_ancestors(Class,KB,ClassAncestors):-
	there_is_class(Class,KB,yes),
	list_of_ancestors(Class,KB,ClassAncestors).

class_ancestors(Class,KB,unknown):-
	there_is_class(Class,KB,unknown).

list_of_ancestors(top,_,[]).

list_of_ancestors(Class,KB,Ancestors):-
	mother_of_a_class(Class,KB,Mother),
	append([Mother],GrandParents,Ancestors),
	list_of_ancestors(Mother,KB,GrandParents).



%Consult the properties of a class

class_properties(top,KB,Properties):-
	properties_only_in_the_class(top,KB,Properties).

class_properties(Class,KB,Properties):-
	there_is_class(Class,KB,yes),
	properties_only_in_the_class(Class,KB,ClassProperties),
	append([ClassProperties],AncestorsProperties,AllProperties),
	concat_ancestors_properties(Ancestors,KB,AncestorsProperties),
	list_of_ancestors(Class,KB,Ancestors),
	cancel_repeated_property_values(AllProperties,Properties).

class_properties(Class,KB,unknown):-
	there_is_class(Class,KB,unknown).


properties_only_in_the_class(_,[],[]).

properties_only_in_the_class(Class,[class(Class,_,Properties,_,_)|_],Properties).

properties_only_in_the_class(Class,[_|T],Properties):-
	properties_only_in_the_class(Class,T,Properties).


concat_ancestors_properties([],_,[]).

concat_ancestors_properties([Ancestor|T],KB,[Properties|NewT]):-
	concat_ancestors_properties(T,KB,NewT),
	properties_only_in_the_class(Ancestor,KB,Properties).

cancel_repeated_property_values(X,Z):-
	aplana_un_nivel(X,Y),
	delete_repeated_properties(Y,Z).

delete_repeated_properties([],[]).

delete_repeated_properties([P=>V|T],[P=>V|NewT]):-
	deleteAllElementsWithSameProperty(P,T,L1),
	eliminar_elem(not(P=>V),L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([not(P=>V)|T],[not(P=>V)|NewT]):-
	deleteAllElementsWithSameNegatedProperty(P,T,L1),
	eliminar_elem(P=>V,L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([not(H)|T],[not(H)|NewT]):-
	eliminar_elem(not(H),T,L1),
	eliminar_elem(H,L1,L2),
	delete_repeated_properties(L2,NewT).

delete_repeated_properties([H|T],[H|NewT]):-
	eliminar_elem(H,T,L1),
	eliminar_elem(not(H),L1,L2),
	delete_repeated_properties(L2,NewT).



%Verify if a class has a specific property

class_has_property(Class,Property,KB,Answer):-
	class_properties(Class,KB,Properties),
	incomplete_information(Property,Properties,Answer).

incomplete_information(_,[], unknown).

incomplete_information(Atom, List, yes):- verifica_elem(Atom,List).

incomplete_information(not(Atom), List, no):- verifica_elem(Atom,List).

incomplete_information(Atom, List, no):- verifica_elem(not(Atom),List).

incomplete_information(_, _, unknown).



%Return the value of a class property

class_property_value(Class,Property,KB,Value):-
	class_properties(Class,KB,ClassProperties),
	find_value(Property,ClassProperties,Value).

find_value(_,[],unknown).

find_value(Attribute,[Attribute=>Value|_],Value).

find_value(Attribute,[not(Attribute)|_],no).

find_value(Attribute,[Attribute|_],yes).

find_value(Attribute,[_|T],Value):-
	find_value(Attribute,T,Value).



%Shows the class of an object

class_of_an_object(_,[],unknown):-!.

class_of_an_object(Object,[class(C,_,_,_,O)|_],C):-
	verifica_elem([id=>Object,_,_],O).

class_of_an_object(Object,[_|T],Class):-
	class_of_an_object(Object,T,Class).



%List all the properties of an object

object_properties(Object,KB,AllProperties):-
	there_is_object(Object,KB,yes),
	properties_only_in_the_object(Object,KB,ObjectProperties),
	class_of_an_object(Object,KB,Class),
	class_properties(Class,KB,ClassProperties),
	append(ObjectProperties,ClassProperties,Temp),
	delete_repeated_properties(Temp,AllProperties).

object_properties(_,_,unknown).

properties_only_in_the_object(_,[],[]).

properties_only_in_the_object(Object,[class(_,_,_,_,O)|_],Properties):-
	verifica_elem([id=>Object,Properties,_],O).

properties_only_in_the_object(Object,[_|T],Properties):-
	properties_only_in_the_object(Object,T,Properties).
	


%Return the value of an object property

object_property_value(Object,Property,KB,Value):-
	there_is_object(Object,KB,yes),
	object_properties(Object,KB,Properties),
	find_value(Property,Properties,Value).

object_property_value(_,_,_,unknown).



%Consult the relations of a class


class_relations(top,KB,Relations):-
	relations_only_in_the_class(top,KB,Relations).

class_relations(Class,KB,Relations):-
	there_is_class(Class,KB,yes),
	relations_only_in_the_class(Class,KB,ClassRelations),
	append([ClassRelations],AncestorsRelations,AllRelations),
	concat_ancestors_relations(Ancestors,KB,AncestorsRelations),
	list_of_ancestors(Class,KB,Ancestors),
	cancel_repeated_property_values(AllRelations,Relations).

class_relations(_,_,unknown).


relations_only_in_the_class(_,[],[]).

relations_only_in_the_class(Class,[class(Class,_,_,Relations,_)|_],Relations).

relations_only_in_the_class(Class,[_|T],Relations):-
	relations_only_in_the_class(Class,T,Relations).


concat_ancestors_relations([],_,[]).

concat_ancestors_relations([Ancestor|T],KB,[Relations|NewT]):-
	concat_ancestors_relations(T,KB,NewT),
	relations_only_in_the_class(Ancestor,KB,Relations).



%Return the value of a class relation

class_relation_value(Class,Relation,KB,Value):-
	there_is_class(Class,KB,yes),
	class_relations(Class,KB,Relations),
	find_value_relation(Relation,Relations,Value).

class_relation_value(_,_,_,unknown).


find_value_relation(not(Relation),Relations,Value):-
	find_value_negative_relation(Relation,Relations,Value).

find_value_relation(Relation,Relations,Value):-
	find_value_positive_relation(Relation,Relations,Value).


find_value_negative_relation(_,[],unknown).

find_value_negative_relation(Attribute,[not(Attribute=>Value)|_],Value).

find_value_negative_relation(Attribute,[_|T],Value):-
	find_value_negative_relation(Attribute,T,Value).


find_value_positive_relation(_,[],unknown).

find_value_positive_relation(Attribute,[Attribute=>Value|_],Value).

find_value_positive_relation(Attribute,[_|T],Value):-
	find_value_positive_relation(Attribute,T,Value).



%List all the relations of an object

object_relations(Object,KB,AllRelations):-
	there_is_object(Object,KB,yes),
	relations_only_in_the_object(Object,KB,ObjectRelations),
	class_of_an_object(Object,KB,Class),
	class_relations(Class,KB,ClassRelations),
	append([ObjectRelations],[ClassRelations],Temp),
	cancel_repeated_property_values(Temp,AllRelations).

object_relations(_,_,unknown).


relations_only_in_the_object(_,[],[]).

relations_only_in_the_object(Object,[class(_,_,_,_,O)|_],Relations):-
	verifica_elem([id=>Object,_,Relations],O).

relations_only_in_the_object(Object,[_|T],Relations):-
	relations_only_in_the_object(Object,T,Relations).



%Return the value of an object relation

object_relation_value(Object,Relation,KB,Value):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	find_value_relation(Relation,Relations,Value).

object_relation_value(_,_,_,unknown).



% Return the son classes of a class

sons_of_class(Class,KB,Answer):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Answer).

sons_of_class(_,_,unknown).

sons_of_a_class(_,[],[]).

sons_of_a_class(Class,[class(Son,Class,_,_,_)|T],Sons):-
	sons_of_a_class(Class,T,Brothers),	
	append([Son],Brothers,Sons).

sons_of_a_class(Class,[_|T],Sons):-
	sons_of_a_class(Class,T,Sons).	
	

% Return the sons of a list of classes of a class

sons_of_a_list_of_classes([],_,[]).

sons_of_a_list_of_classes([Son|T],KB,Grandsons):-
	sons_of_a_class(Son,KB,Sons),
	sons_of_a_list_of_classes(T,KB,Cousins),
	append(Sons,Cousins,Grandsons).


% Return all the descendant classes of a class

descendants_of_a_class(Class,KB,Descendants):-
	there_is_class(Class,KB,yes),
	sons_of_a_class(Class,KB,Sons),
	all_descendants_of_a_class(Sons,KB,Descendants).

descendants_of_a_class(_,_,unknown).

all_descendants_of_a_class([],_,[]).

all_descendants_of_a_class(Classes,KB,Descendants):-
	sons_of_a_list_of_classes(Classes,KB,Sons),
	all_descendants_of_a_class(Sons,KB,RestOfDescendants),
	append(Classes,RestOfDescendants,Descendants).


% Return the names of the objects listed only in a specific class


objects_only_in_the_class(_,[],unknown).

objects_only_in_the_class(Class,[class(Class,_,_,_,O)|_],Objects):-
	extract_objects_names(O,Objects).

objects_only_in_the_class(Class,[_|T],Objects):-
	objects_only_in_the_class(Class,T,Objects).
	
extract_objects_names([],[]).

extract_objects_names([[id=>Name,_,_]|T],Objects):-
	extract_objects_names(T,Rest),
	append([Name],Rest,Objects).


% Return all the objects of a class

objects_of_a_class(Class,KB,Objects):-
	there_is_class(Class,KB,yes),
	objects_only_in_the_class(Class,KB,ObjectsInClass),
	descendants_of_a_class(Class,KB,Sons),
	objects_of_all_descendants_classes(Sons,KB,DescendantObjects),
	append(ObjectsInClass,DescendantObjects,Objects).

objects_of_a_class(_,_,unknown).

objects_of_all_descendants_classes([],_,[]).

objects_of_all_descendants_classes([Class|T],KB,AllObjects):-
	objects_only_in_the_class(Class,KB,Objects),
	objects_of_all_descendants_classes(T,KB,Rest),
	append(Objects,Rest,AllObjects).



%------------------------------------------------------------
% Main KB Services 
%------------------------------------------------------------

%Class extension

class_extension(Class,KB,Objects):-
	objects_of_a_class(Class,KB,Objects).	


% Property extension

property_extension(Property,KB,Result):-
	objects_of_a_class(top,KB,AllObjects),
	filter_objects_with_property(KB,Property,AllObjects,Objects),
	eliminate_null_property(Objects,Result).

filter_objects_with_property(_,_,[],[]).

filter_objects_with_property(KB,Property,[H|T],[H:Value|NewT]):-
	object_property_value(H,Property,KB,Value),
	filter_objects_with_property(KB,Property,T,NewT).

eliminate_null_property([],[]).

eliminate_null_property([_:unknown|T],NewT):-
	eliminate_null_property(T,NewT).

eliminate_null_property([X:Y|T],[X:Y|NewT]):-
	eliminate_null_property(T,NewT).



% Relation extension

relation_extension(Relation,KB,FinalResult):-
	objects_of_a_class(top,KB,AllObjects),
	filter_objects_with_relation(KB,Relation,AllObjects,Objects),
	eliminate_null_property(Objects,Result),
	expanding_classes_into_objects(Result,FinalResult,KB).

filter_objects_with_relation(_,_,[],[]).

filter_objects_with_relation(KB,Relation,[H|T],[H:Value|NewT]):-
	object_relation_value(H,Relation,KB,Value),
	filter_objects_with_relation(KB,Relation,T,NewT).

expanding_classes_into_objects([],[],_).

expanding_classes_into_objects([X:Y|T],[X:Objects|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expanding_classes_into_objects(T,NewT,KB).

expanding_classes_into_objects([X:Y|T],[X:[Y]|NewT],KB):-
	expanding_classes_into_objects(T,NewT,KB).


%Classes of individual

classes_of_individual(Object,KB,Classes):-
	there_is_object(Object,KB,yes),
	class_of_an_object(Object,KB,X),
	class_ancestors(X,KB,Y),
	append([X],Y,Classes).

classes_of_individual(_,_,unknown).


% Properties of individual

properties_of_individual(Object,KB,Properties):-
	object_properties(Object,KB,Properties).


% Relations of individual

relations_of_individual(Object,KB,ExpandedRelations):-
	there_is_object(Object,KB,yes),
	object_relations(Object,KB,Relations),
	expand_classes_to_objects(Relations,ExpandedRelations,KB).

relations_of_individual(_,_,unknown).

expand_classes_to_objects([],[],_).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>Objects)|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>Objects|NewT],KB):-
	there_is_class(Y,KB,yes),
	objects_of_a_class(Y,KB,Objects),
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([not(X=>Y)|T],[not(X=>[Y])|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).

expand_classes_to_objects([X=>Y|T],[X=>[Y]|NewT],KB):-
	expand_classes_to_objects(T,NewT,KB).

	
