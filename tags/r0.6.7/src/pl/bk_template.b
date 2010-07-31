%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    This file is part of LogCHEM.
%
%    $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% -*- Mode: Prolog; -*-

:- use_module(logchem).
%:- load_files('logchem.pl',[silent(true)]).

%% load the user settings, mode and determination declarations, and the database
:- ['sdf.pl'].
:- ['func_groups.pl'].
:- ['user_settings.pl'].

:- modeh(1,active(+drug)).

:- modeb(*,atom(+drug,-atomid,#element)).
:- determination(active/1,atom/3).
:- modeb(*,atom_bond(+drug,+atomid,-atomid,#element,#element,#int)).
:- determination(active/1,atom_bond/6).
:- ['mode_decls.pl'].

%%%%%%%%%%%%%
%% Properties
%:- modeb(*,'CF'(+drug,-logp)).
%:- modeb(*,'PF'(+drug,-logp)).
:- determination(active/1,'RF'/2).
:- modeb(*,'RF'(+drug,-rf)).
:- determination(active/1,'BCUT'/2).
:- modeb(*,'BCUT'(+drug,-bcut)).
:- determination(active/1,'LogP'/2).
:- modeb(*,'LogP'(+drug,-logp)).
:- determination(active/1,'LogD'/2).
:- modeb(*,'LogD'(+drug,-logd)).
:- determination(active/1,'TPSA'/2).
:- modeb(*,'TPSA'(+drug,-tpsa)).
:- determination(active/1,'Mass'/2).
:- modeb(*,'Mass'(+drug,-mass)).
:- determination(active/1,'Heavy'/2).
:- modeb(*,'Heavy'(+drug,-heavy)).

:- determination(active/1,'DONCOUNT'/2).
:- modeb(*,'DONCOUNT'(+drug,-doncount)).
:- determination(active/1,'ACCCOUNT'/2).
:- modeb(*,'ACCCOUNT'(+drug,-acccount)).
:- determination(active/1,'MOL_POLARIZABILITY'/2).
:- modeb(*,'MOL_POLARIZABILITY'(+drug,-acccount)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- determination(active/1,gte/2).
:- modeb(*,gte(+rf,#rf)).
:- modeb(*,gte(+bcut,#bcut)).
:- modeb(*,gte(+logd,#logd)).
:- modeb(*,gte(+logp,#logp)).
:- modeb(*,gte(+tpsa,#tpsa)).
:- modeb(*,gte(+mass,#mass)).
:- modeb(*,gte(+heavy,#heavy)).
:- modeb(*,gte(+doncount,#doncount)).
:- modeb(*,gte(+account,#account)).
:- modeb(*,gte(+mol_polarizability,#mol_polarizability)).

gte(X,Y) :- number(X), var(Y), !, X = Y.
gte(X,Y) :- number(X), X >= Y.

:- determination(active/1,lte/2).
:- modeb(*,lte(+rf,#rf)).
:- modeb(*,lte(+bcut,#bcut)).
:- modeb(*,lte(+logd,#logd)).
:- modeb(*,lte(+logp,#logp)).
:- modeb(*,lte(+tpsa,#tpsa)).
:- modeb(*,lte(+mass,#mass)).
:- modeb(*,lte(+heavy,#heavy)).
:- modeb(*,lte(+doncount,#doncount)).
:- modeb(*,lte(+account,#account)).
:- modeb(*,lte(+mol_polarizability,#mol_polarizability)).

%% :- modeb(*,lte(+secondary_carbon,#secondary_carbon)).
lte(X,Y) :- number(X), var(Y), !, X = Y.
lte(X,Y) :- number(X), X =< Y.

%%%%%%%%%%%%%
% constraints
:- ['user_constraints.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LogCHEM required stuff.
:- set(i,200).
:- set(refine,user).
:- set(optimise_clauses,true).
:- set(sat_prelims_hook,init_canonic).
:- set(match_bottom_hook,canonic).
:- set(clause_rewrite_hook,optimise_pattern).

logchem_dir('/home/nf/Research/Projects/WIP/logchem/LogCHEM2').
%
addlogChemDir2path:-	
	%unix(environ('LOGCHEM_DIR',Dir)),
	logchem_dir(Dir),
	getcwd(C),cd(Dir),cd('lib/pl'),getcwd(PL_Dir),add_to_path(PL_Dir),cd(C).

:-addlogChemDir2path.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hyp2refine(_):-fail.

refine(Cl, NewCl) :-
	logchem_refine(Cl, NewCl).

%:- determination(active/1,log_er_rba/2).

atom_bond(D,A1,A2,E1,E2,Type) :-
	bond(D,A1,A2,Type),
	atom(D,A1,E1),
	atom(D,A2,E2).
atom_bond(D,A1,A2,E1,E2,Type) :-
	bond(D,A2,A1,Type),
	atom(D,A1,E1),
	atom(D,A2,E2).


