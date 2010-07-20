%:- module(func,[func_class/2]).

%:- ensure_loaded(struct).

func_class(X,Y) :-
	current_predicate('ActivityOutcome_NCTRER'/2), !,
	'ActivityOutcome_NCTRER'(X,Y).
func_class(X,Y) :-
	current_predicate('ActivityOutcome_CPDBAS_SingleCellCall'/2), !,
	'ActivityOutcome_CPDBAS_SingleCellCall'(X,Y).
func_class(X,Y) :-
	current_predicate('ActivityOutcome_DBPCAN'/2), !,
	'ActivityOutcome_DBPCAN'(X,Y).
func_class(X,Y) :-
	current_predicate('ActivityOutcome_EPAFHM'/2), !,
	'ActivityOutcome_EPAFHM'(X,Y).
func_class(X,Y) :-
	current_predicate('ActivityCategory_MCASE_mg'/2), !,
	'ActivityCategory_MCASE_mg'(X,Y0),
	(
	 Y0 == high -> Y = active;
	 Y0 == low -> Y = inactive;
	 Y0 = Y
	).
