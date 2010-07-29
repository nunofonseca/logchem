%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2009, 2010 Nuno A. Fonseca (nunofonseca at acm.org), Vitor S. Costa (vsc at dcc.fc.up.pt)
%
%    This file is part of LogCHEM.
%
%    LogCHEM is free software; you can redistribute it and/or modify
%    it under the terms of the GNU General Public License as published by
%    the Free Software Foundation; either version 2 of the License, or
%    (at your option) any later version.
%
%    LogCHEM is distributed in the hope that it will be useful,
%    but WITHOUT ANY WARRANTY; without even the implied warranty of
%    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%    GNU General Public License for more details.
%
%    You should have received a copy of the GNU General Public License
%    along with LogCHEM; if not, write to the Free Software
%    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% $Id$
:- source.

:- dynamic active/1.

:- yap_flag(write_strings, on).
:- prolog_flag(unknown, _, error).


logchem_dir('/home/nf/Research/Projects/WIP/logchem/LogCHEM2').
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% lookup for the modules in the LogChem directory
addlogChemDir2path:-	
	logchem_dir(Dir),
	getcwd(C),cd(Dir),cd('lib/pl'),getcwd(PL_Dir),add_to_path(PL_Dir),cd(C).

:-addlogChemDir2path.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Load modules
:- use_module(library(system),
	      [make_directory/1,
	       mktime/2,
	       datime/1,
	       exec/3
	      ]).

:- use_module(library(readutil),
	      [read_line_to_codes/2]).   

:- use_module(library(rbtrees),
	      [rb_new/1,
	       rb_insert_new/4,
	       rb_lookup/3]).   

:- use_module(library(lists),[member/2]).

:- dynamic match/3.

:- use_module('func_groups').
:- use_module('logchem',[optimise_pattern/2, do_not_compile_logchem/0]).
:- do_not_compile_logchem.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TODO: remove the code bellow!!!! 
atom_bond(D,A1,A2,E1,E2,Type) :-
	bond(D,A1,A2,Type),
	atom(D,A1,E1),
	atom(D,A2,E2).
atom_bond(D,A1,A2,E1,E2,Type) :-
	bond(D,A2,A1,Type),
	atom(D,A1,E1),
	atom(D,A2,E2).

atm_bond(D, A1, A2, Type, E2) :-
	bond(D, A1, A2, Type),
	atom(D, A2, E2).

atm_bond(D, A1, A2, Type, E2) :-
	bond(D, A2, A1, Type),
	atom(D, A2, E2).

lte(X,Y) :- number(X), var(Y), !, X = Y.
lte(X,Y) :- number(X), X =< Y.
gte(X,Y) :- number(X), var(Y), !, X = Y.
gte(X,Y) :- number(X), X >= Y.

% TODO: remove the code above!!!! 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%atom(X,Y,Z) :-
%	atom(X,Y,Z,_,_,_).


do_query(ExpName) :-
	gen_prefix(ExpName, Prefix),
	clause(active(Mol),B),
	rule_id(RuleId),
	optimise_pattern((active(Mol):-B),(_:-Goal)),
	% TODO: change this in order to remove func_class
	func_class(Mol,Class),
	once(Goal),
	dump(B, Mol, Prefix, RuleId),
	assert(match(Mol,RuleId,Class)),
	fail.	

do_query(ExpName) :-
	retractall(id(_)),
	gen_prefix(ExpName, Prefix),
	atom_concat(Prefix,'Mols.LogCHEM',Mols),
	open(Mols, write, SM),
	dump_classes_and_theory(SM),
	close(SM),
	format('~w~n',[Prefix]).

rule_id(Id) :-
	retract(id(I)), !,
	Id is I+1,
	assert(id(Id)).
rule_id(1) :-
	assert(id(1)).

dump_classes_and_theory(SM) :-
	setof(X,Mol^func_class(Mol, X),Classes),
	length(Classes,LClasses),
	format(SM, '~d~n', [LClasses]),
	member(Class,Classes),
	format(SM,'~a~n',[Class]),
	assert(class(Class)),
	fail.
dump_classes_and_theory(SM) :-
	setof(B,Mol^clause(active(Mol),B),LClauses),
	length(LClauses, NClauses),
	format(SM, '# number of rules~n~d~n',[NClauses]),
	clause(active(Mol),B),
	rule_id(RuleId),
	dump_clause_info(RuleId, SM, Mol, B),
	fail.
dump_classes_and_theory(_).

dump_clause_info(RuleId, SM,_,_) :-
	format(SM, '# clause~n~d,',[RuleId]),
	fail.
dump_clause_info(RuleId, SM,_,_) :-
	class(Class),
	(
	    setof(Mol,match(Mol,RuleId,Class),Cov),
	    length(Cov,LCov)->
	    true
	;
	    LCov=0
	),
	format(SM, '~w,',[LCov]),
	fail.
dump_clause_info(RuleId, SM, Mol, B) :-
	copy_term(B,B1),
	once(match(Mol,RuleId,_)),
	to_smiles(Mol, B, SMI),
	numbervars(B1,0,_),
	format(SM, '~s,~q~n',[SMI,B1]),
	fail.

dump_clause_info(RuleId, SM, Mol, _) :-
	match(Mol,RuleId,Class),
	format(SM, '~w,~a~n',[Mol,Class]),
	fail.
dump_clause_info(_, _, _, _).

gen_prefix(ExpName, Dir) :-
	atomic_concat([ExpName,'/'],Dir),
	catch(make_directory(Dir),_,true),
	!.

dump(B, Mol, Prefix, Type) :-
	atomic_concat([Prefix,'MOL_',Type,'_',Mol,'.sdf'],NMol),
	atomic_concat([Prefix,'MATCH_',Type,'_',Mol,'.sdf'],NMatch),
	open(NMol, write, SMol),
	open(NMatch, write, SMatch),
	dump_mol(SMol,Mol),
	dump_match(SMatch,Mol,B),
	close(SMol),
	close(SMatch), !.

dump_mol(SMol,Mol) :-
	prefix(SMol, Mol),
	findall(atom(Type,X,Y,Z),atom(Mol,_,Type,X,Y,Z),Ats),
	findall(bond(Id1,Id2,Type),bond(Mol,Id1,Id2,Type),Bonds),
	length(Ats,NAtoms),
	length(Bonds,NBonds),
	dump_hd(SMol,NAtoms,NBonds),
	dump_all_atoms(SMol, Ats),
	dump_all_bonds(SMol, Bonds),
	posfix(SMol, Mol).

dump_match(SMatch,Mol,B0) :-
	prefix(SMatch, Mol),
	clean_match(B0,B),
	atoms_in_match(B, Atoms),
	map_atoms(Atoms, Mol, Map, Ats),
	map_bonds(B, Map, Bonds, []),
	length(Ats,NAtoms),
	length(Bonds,NBonds),
	dump_hd(SMatch,NAtoms,NBonds),
	dump_all_atoms(SMatch, Ats),
	dump_all_bonds(SMatch, Bonds),
	posfix(SMatch, Mol).

clean_match((G1,G2),G3):-
	not atoms_in_g(G1,_,_),
	!,
	clean_match(G2,G3).
clean_match(G,G).    

atoms_in_match(Gs, Sorted) :-
	atoms_in(Gs, Atoms, []),
	sort(Atoms, Sorted).
	
atoms_in((G,Gs), Atoms, Ats) :- !,
	atoms_in_g(G, Atoms, AtomsI),
	atoms_in(Gs, AtomsI, Ats).
atoms_in(G, Atoms, Ats) :-
	atoms_in_g(G, Atoms, Ats).

atoms_in_g(atom(_,_,_), Ats, Ats).
atoms_in_g(atom_bond(_,Id1,Id2,_,_,_), [Id1,Id2|Ats], Ats).

map_atoms(Atoms, Mol, Map, AtomsPos) :-
	rb_new(Map0),
	map_atoms(Atoms, Map0, Map, Mol, 1, AtomsPos).

map_atoms([], Map, Map, _, _, []).
map_atoms([A|Atoms], Map0, Map, Mol, I, [atom(H,X,Y,Z)|AtomsPos]) :-
	atom(Mol, A, H, X, Y, Z),
	rb_insert_new(Map0,A,I,MapI), !,
	I1 is I+1,
	map_atoms(Atoms, MapI, Map, Mol, I1, AtomsPos).

map_bonds((G,Gs), Map, Bonds, Bonds0) :- !,
	map_bonds_g(G, Map, Bonds, BondsI),
	map_bonds(Gs, Map, BondsI, Bonds0).
map_bonds(G, Map, Bonds, Bonds0) :-
	map_bonds_g(G, Map, Bonds, Bonds0).

map_bonds_g(atom(_,_,_), _, Bonds, Bonds).
map_bonds_g(atom_bond(_,Id1,Id2,_,_,Type), Map, [bond(NId1,NId2,Type)|Bonds], Bonds) :-
	rb_lookup(Id1,NId1,Map),
	rb_lookup(Id2,NId2,Map).


dump_hd(SMol,NAtoms,NBonds) :-
	format(SMol,'~t~d~3+~t~d~3+~n',[NAtoms,NBonds]).

dump_all_atoms(_, []).
dump_all_atoms(SMol, [atom(H,X,Y,Z)|Ats]) :-
	format(SMol,'~t~4g~10+~t~4g~10+~t~4g~10+ ~a~t~5+0  0  0  0  0~n',[X,Y,Z,H]),
%	format(user_output,'~t~4g~10+~t~4g~10+~t~4g~10+ ~a~t~5+0  0  0  0  0~n',[X,Y,Z,H]),
	dump_all_atoms(SMol, Ats).

dump_all_bonds(_, []).
dump_all_bonds(SMol, [bond(Id1,Id2,Type)|Ats]) :-
	format(SMol,'~t~d~3+~t~d~3+~t~d~3+  0  0  0~n',[Id1,Id2,Type]),
	dump_all_bonds(SMol, Ats).

% changed ~d --> ~w
prefix(SMol, Mol) :-
	format(SMol,'~w        
crcorina  10299914243D
Corina 01.500030  20.12.1994~n',[Mol]).
% changed ~d --> ~w
posfix(SMol, Mol) :-
	format(SMol,'> <NSC>
~w    

> <CAS_RN>
999-99-9

$$$$
',[Mol]).

to_smiles(Mol, B, SMI) :-
	optimise_pattern((active(Mol):-B),(_:-Goal)),
	once(Goal),
	exec('babel -i sdf -o smi -xn',[pipe(Match),pipe(SMIS),null],_PID),
	dump_match(Match, Mol, B),
	close(Match),
	read_line_to_codes(SMIS, SMI), write(SMI),nl,
	close(SMIS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check arguments
check_args(ExpName,StructFile,RuleFile):-
	yap_flag(argv,[ExpName,StructFile,RuleFile]),!.
check_args(ExpName,struct,rule):-
	yap_flag(argv,[ExpName]),!.
check_args(_,_,_):-
	format('Error~n',[]),
	format('Usage: match expname [struct_file rule_file]~n',[]),
	halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- initialization(main).
main:-
	check_args(ExpName,StructFile,RuleFile),
	consult(StructFile),
	consult(RuleFile),
	consult(func),
	do_query(ExpName),
	halt.
