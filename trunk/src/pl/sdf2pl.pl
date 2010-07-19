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
% $Id: sdf2pl.pl,v 1.3 2010/06/30 14:56:46 nf Exp $
:- source.

:- yap_flag(write_strings, on).

:- style_check(all).

:- prolog_flag(unknown, _, error).

:- use_module(library(lineutils),
	      [split/3,
	       filter/3,
	       glue/3,
	       copy_line/2	      
	      ]).

:- use_module(library(readutil),
	      [read_line_to_codes/2]).   

:- use_module(library(lists),
	      [member/2
	      ]).


:- initialization(main).

main :-
     prolog_flag(argv,[]), !,
     get_mols(user_input, user_output),
     halt.
main :-
     prolog_flag(argv,[Inp]), !,
     open(Inp, read, I),
     get_mols(I, user_output),
     close(I),
     halt.
main :-
     prolog_flag(argv,[Inp,Out]),
     open(Inp, read, I),
     open(Out, write, O),
     get_mols(I, O),
     close(I),
     close(O),
     halt.

get_mols(Inp, Out) :-
	prelims(Out),
	repeat,
	read_line_to_codes(Inp, L),
	(
	 L == end_of_file
	->
	 generate_atm_bond(Out), !
	;
	 process_mol(L, Inp, Out),
	 fail
	).

prelims(Out) :-
	format(Out, '% This file was generated automatically by LogCHEM~n', []),
	format(Out, '% ~n~n% DO NOT EDIT~n~n', []), 
	format(Out, ':- style_check(-discontiguous).~n~n~n~n', []).

process_mol(IdS,Inp,Out) :- !,
	get_header(Inp, _),
	scan_lines(Inp, AllAtoms, AllBonds),
	read_props(Inp, Props),
	dump_props(Out, IdS, Props, AllAtoms, AllBonds).


get_header(Inp, Generator) :-
	read_line_to_codes(Inp, Generator),
	read_line_to_codes(Inp, _Comments).
	
dump_props(O, Id0, Props, AllAtoms, AllBonds) :-
	get_id_from_props(Props, Id0, Id),
	dump(O, Id, Props, AllAtoms, AllBonds).

process_lines(Id, Inp, Out) :-
	scan_lines(Inp, AllAtoms, AllBonds),
	dump(Out, Id, [], AllAtoms, AllBonds).

scan_lines(Inp, AllAtoms, AllBonds) :-	
	read_line_to_codes(Inp, [A1,A2,A3,B1,B2,B3|_]),
	split([A1,A2,A3], " 	", [As]),
	split([B1,B2,B3], " 	", [Bs]),
	number_codes(Atoms, As),
	number_codes(Bonds, Bs),
	scan_atoms(0, Atoms, Inp, AllAtoms),
	scan_bonds(0, Bonds, Inp, AllBonds).

scan_atoms(Atoms, Atoms, _, []) :- !.
scan_atoms(I0, Atoms, Inp, [I-at(At,X,Y,Z)|AllAtoms]) :-
	I is I0+1,
	read_line_to_codes(Inp, L),
	split(L, " 	", [Xs,Ys,Zs,C|_]),
	atom_codes(At,C),
	number_codes(X,Xs),
	number_codes(Y,Ys),
	number_codes(Z,Zs),
	scan_atoms(I, Atoms, Inp, AllAtoms).

scan_bonds(Bonds, Bonds, _, []) :- !.
scan_bonds(I0, Bonds, Inp, [b(F,La,V)|AllBonds]) :-
	I is I0+1,
	read_line_to_codes(Inp, [F1,F2,F3,L1,L2,L3,V1,V2,V3|_]),
	split([F1,F2,F3], " 	", [Fs]),
	split([L1,L2,L3], " 	", [Las]),
	split([V1,V2,V3], " 	", [Vs]),
	number_codes(F, Fs),
	number_codes(La, Las),
	number_codes(V, Vs),
	scan_bonds(I, Bonds, Inp, AllBonds).

skip_lines(Inp) :-
	read_line_to_codes(Inp, "$$$$"), !.
skip_lines(Inp) :-
	skip_lines(Inp).

read_props(Inp, Props) :-
	read_line_to_codes(Inp, L),
	read_props(L, Inp, Props).

read_props([], Inp, Props) :- !,
	read_line_to_codes(Inp, L),
	read_props(L, Inp, Props).
read_props(end_of_file, _, []) :- !.
read_props([0'$,0'$|_], _, []) :- !.
read_props(L, Inp, Props) :-
	split(L," 	", ["M"|_]),
	read_line_to_codes(Inp, MoreL),
	read_props(MoreL, Inp, Props).
read_props([13], Inp, Props) :- !,
	read_line_to_codes(Inp, L),
	read_props(L, Inp, Props).
read_props(L, Inp, AllProps) :-
	split(L,[13|"< >	"],[PropS]),
	name(PropName, PropS),
	read_line_to_codes(Inp, L2),
%	process_atts(L2,Atts),
%	Prop =..  [PropName|Atts],
%	read_line_to_codes(Inp, NL),
	continue_read_props(L2, Inp, PropName, AllProps).

continue_read_props([], Inp, _PropName, AllProps) :-!,
	read_props([], Inp, AllProps).

continue_read_props(L2, Inp, PropName, [Prop|Props]) :-
	process_atts(L2,Atts),
	Prop =..  [PropName|Atts],
	read_line_to_codes(Inp, NL),
	continue_read_props(NL, Inp, PropName, Props).

process_atts(L2,Atts) :-
	split(L2,[13|" 	"],AttL),
	process_attl(AttL, Atts).

process_attl([], []).
process_attl([AttS|AttL], [Att|Atts]) :-
	undercase_first(AttS,NAttS),
	name(Att, NAttS),
	process_attl(AttL, Atts).

undercase_first([C|PropS],[NC|PropS]) :-
	code_type(C, to_upper(NC)).

dump(O, Id, Props, AllAtoms, AllBonds) :-
	dump_atoms(AllAtoms,Id,O),
	dump_bonds(AllBonds,Id,O),
	dump_props(Props,Id,O),
	format(O,'%$$$$$$$$~n~n~n~n~n~n~n~n~n~n',[]).

dump_atoms([],_,_).
dump_atoms([I-at(At,X,Y,Z)|AllAtoms],Id,O) :-
	format(O,'atom(~q,~d,~q,~4g,~4g,~4g).~n',[Id,I,At,X,Y,Z]),
	assert(atom(Id,I,At)),
	format(O,'atom(~q,~d,~q).~n',[Id,I,At]),
	dump_atoms(AllAtoms,Id,O).


dump_bonds([],_,_).
dump_bonds([b(F,L,V)|AllBonds],Id,O) :-
	assert(bond(Id,F,L,V)),
	format(O,'bond(~q,~d,~d,~d).~n',[Id,F,L,V]),
	dump_bonds(AllBonds,Id,O).
	
dump_props([],_,_).
dump_props([P|AllProps],Id,O) :-
	P =.. [Name|Args],
	NP =.. [Name,Id|Args],
	format(O,'~q.~n',[NP]),
	dump_props(AllProps,Id,O).

lower_codes([],[]).
lower_codes([C|Cs],[LC|LCs]) :-
	code_type(C, to_upper(LC)),
	lower_codes(Cs,LCs).

get_id_from_props(Props, Id0, Id) :-
	Id0 = "", !,
	get_id_from_props(Props, Id).
get_id_from_props(_, Id0, Id) :-
	atom_codes(Id, Id0).

get_id_from_props(Props, Id) :-
	member(dSSTox_RID(Id),Props), !.
get_id_from_props(Props, Id) :-
	member(dSSTox_CID(Id),Props), !.


generate_atm_bond(O) :-
	user:atom(D, A1, _),
	(
	 user:bond(D, A1, A2, Type),
	 user:atom(D, A2, E2)
	;
	 user:bond(D, A2, A1, Type),
	 user:atom(D, A2, E2)
	),
	format(O,'~q.~n', [atm_bond(D, A1, A2, Type, E2)]),
	fail.
generate_atm_bond(_).


