%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2008, 2009, 2010 Vitor S. Costa (vsc at dcc.fc.up.pt),
%                            Nuno A. Fonseca (nunofonseca at acm.org)
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
%%% -*- Mode: Prolog; -*-
%
:- style_check(all).

:- yap_flag(call_counting,off).

:- module(logchem,[
		   enable_prop/1,
		   enable_props/1,
		   logchem_refine/2,
		   init_canonic/0,
		   canonic/1,
		   optimise_pattern/2,
		   user_goal_fails/6,
		   check_duplicates/1,
		   constraint/1,
		   do_not_compile_logchem/0
		   ]).

:- use_module(library(wundgraphs)).

:- use_module(library(lists)).

:- use_module(library(tries)).

:- use_module(library(rbtrees)).

:- dynamic canonic/1, docompile/1, dooptimise/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
docompile(on).
dooptimise(off). % not really worth the cost
canonize(on).

do_not_compile_logchem :-
	retractall(docompile(_)),
	assert(docompile(off)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%:- use_module(user,[atom/3, atom_bond/6, max/2, example/3]).
:- op(500,fy,#).
:- op(500,fy,*).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% enable predicates
enable_prop(PropName):-
	lc_pred(PropName,Mode),	
	user:modeb(*,Mode),
	functor(Mode, Fun, Arity),
	user:determination(active/1,Fun/Arity).

enable_props([]).
enable_props([P|R]):-
	(enable_prop(P)->true;true),
	enable_props(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functional groups
lc_pred('Methyl','Methyl'(+drug,-ring)).
lc_pred('Amino','Nh2'(+drug,-atomid,-atomid,-atomid)).
lc_pred('Aldehyde','Aldehyde'(+drug,-atomid,-atomid,-atomid)).
lc_pred('Carboxylic acid','Carboxylic_acid'(+drug,-ring)).
lc_pred('Hydroxyl','Hydroxyl'(+drug,-atomid,-atomid)).
lc_pred('Cyano','Cyano'(+drug,-atomid,-atomid)).
lc_pred('Ketone','Ketone'(+drug,-atomid,-atomid)).
lc_pred('Benzene ring','Benzene'(+drug,-ring)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
match_atom((A,_B),A1):-
	arg(1,A,A1),
	!.
match_atom(A,A1):-
	arg(1,A,A1).

%
logchem_refine(false, (active(A):-Rule)) :-
	user:hyp2refine(Rule),!,
	match_atom(Rule,A).
logchem_refine(false, Cl) :-
	logchem_refine_atts(false, Cl).
logchem_refine(false, Cl) :-
	logchem_refine_graph(false, Cl).
logchem_refine((active(M) :- Gs),(active(M) :- NGs)) :-
	split_clause(Gs, GAtt, GGraph),
	(
	 logchem_refine_atts((active(M) :- GAtt), (active(M) :- NGAtt))
	;
	 NGAtt = GAtt
	),
	logchem_refine_graph((active(M) :- GGraph), (active(M) :- NGGraph)),
	compose_clause_from_subclauses(NGAtt, NGGraph, NGs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Only one refinement step for now		    
logchem_refine_atts(false,(active(M) :- Prop)) :-
	get_logchem_att(Prop, M).
logchem_refine_atts((active(M) :- false),Gs)  :-
        Gs = (active(M) :- Prop),
	get_logchem_att(Prop, M).

get_logchem_att((PropG,gte(Inp,_)), M) :-
	user:'$aleph_global'(modeb,modeb(_,gte(+Type,#_))),
	user:'$aleph_global'(modeb,modeb(_,Prop)),
	functor(Prop, Na, 2),
	arg(2,Prop,-Type),
	PropG =.. [Na,M,Inp].
get_logchem_att((PropG,lte(Inp,_)), M) :-
	user:'$aleph_global'(modeb,modeb(_,lte(+Type,#_))),
	user:'$aleph_global'(modeb,modeb(_,Prop)),
	functor(Prop, Na, 2),
	arg(2,Prop,-Type),
	PropG =.. [Na,M,Inp].
get_logchem_att(PropG, M) :-
	user:'$aleph_global'(modeb,modeb(_,Prop)),
	functor(Prop, Na, Ar),
	not check_numerical(Prop,Na,Ar),
	Na \= atom,
	Na \= atom_bond,
	functor(PropG, Na, Ar),
	arg(1, PropG, M).

check_numerical(Prop,_Na,Ar):-
	arg(Ar,Prop,-Type),
	user:'$aleph_global'(modeb,modeb(_,lte(+Type,#_))).
	
split_clause((atom(M,A,B),Gs), false, (atom(M,A,B),Gs) ) :- !.
split_clause((G0, atom(M,A,B),Gs), G0, (atom(M,A,B),Gs) ) :- !.
split_clause((G1,Gs), (G1, Atts), Struc ) :- !,
	split_clause(Gs, Atts, Struc ).
split_clause(atom(M,A,B), false, atom(M,A,B) ) :- !.
split_clause(Att, Att, false ).	    

compose_clause_from_subclauses(false, G, G) :- !.
compose_clause_from_subclauses(G, false, G) :- !.
compose_clause_from_subclauses((G,NGAtts), NGGraph, (G,NGs)) :- !,
	compose_clause_from_subclauses(NGAtts, NGGraph, NGs).
compose_clause_from_subclauses(G, NGGraph, (G,NGGraph)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% user refinement operator
% bulk of effort is here
logchem_refine_graph(false,(active(M) :- atom(M,_,_))).
logchem_refine_graph((active(M) :- false),Gs) :- !,
        Gs = (active(M) :- atom(M,_,_)).
logchem_refine_graph((active(M) :- Gs),
	(active(M) :- NGs)) :-
	count_gs(Gs,1,_Count,LGs),
	refine(LGs,M,[],NGs).

count_gs((G,Gs),I0,Count,[G|LGs]) :- !,
	I is I0+1,
	count_gs(Gs,I,Count,LGs).
count_gs(G,Count,Count,[G]).

refine([],M,L,atom_bond(M,OA,_,OZ,_NZ,_BT)) :-
	sort(L,NL),
	back_link(NL,OA,OZ).

refine([atom(M,A,Z)|LG],M,VL,(atom(M,A,Z),NG)) :-
	refine(LG,M,[A-Z|VL],NG).
refine([atom_bond(M,A1,A,Z1,Z,B)|LG],M,VL,(atom_bond(M,A1,A,Z1,Z,B),NG)) :-
	refine(LG,M,[A-Z|VL],NG).

back_link([BA-BZ|_],BA,BZ).
back_link([_|L],BA,BZ) :-
	back_link(L,BA,BZ).


element('Br').
element('C').
element('Cl').
element('F').
element('H').
element('I').
element('N').
element('O').
element('S').


bind(1).
bind(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Optimization
optimise_pattern((H :- B), (H :- NBF)) :-
	    H = active(Mol),
            B = (_, _),
	    !,
	    (
	     optimise_pat(B, Mol, NB)
	    ->
	     true
	    ;
	     write(error:(H:-B)),nl
	    ),
	    (docompile(on) ->
	     abolish(user:nactive/1),
	     assert_static(user:(nactive(Mol) :- NB)),
	     NBF = nactive(Mol)
	    ;
	     NBF = NB
	    ).		   
optimise_pattern((active(Mol) :- B), (active(Mol) :- B)) :-
    	    (docompile(on) ->
	     abolish(user:nactive/1),
	     assert_static(user:(nactive(Mol) :- B))
	    ;
	     true
	    )    .

optimise_pat(B, _, NB) :-
	dooptimise(off), !,
	rewrite_atom_goals(B,List,[]),
	list2goals(List, NB).
optimise_pat(B, Mol, NB) :-
	wundgraph_new(Graph0),
	make_graph(B, Graph0, Graph),
	pick_best_node(Graph, BestNode),
	build_clause_wmol(BestNode, Mol, Graph, ListGoals),
	list2goals(ListGoals, NB).


pick_best_node(Graph, BestNode) :-
	wundgraph_vertices(Graph, [Node|Nodes]),
	Node = (T._),
	once(score( T,  S)),
	select_best(Nodes, Node, S, BestNode).

select_best([], BestNode, _, BestNode).
select_best([N|Nodes], _, S, BestNode) :-
	better_node(N, S, SN), !,
	select_best(Nodes, N, SN, BestNode).
select_best([_|Nodes], Node, S, BestNode) :-
	select_best(Nodes, Node, S, BestNode).

better_node((TN._), S, SN) :-
	once(score(TN, SN)),
	SN > S.

score('H', 1).
score('C', 2).
score('O', 3).
score('N', 4).
score('S', 5).
score('Cu', 6).
score(_, 7).

build_clause_wmol((T.V), Mol, Graph, [atom(Mol, V,T)|LClause]) :-
	% bootstrap the queue.
        wundgraph_wneighbors((T.V), Graph, Neighs),
	add_neighbs(Neighs, Nodes, [(T.V)], NodesR),
	add_nodes_wmol(Nodes, NodesR, [(T.V)], Mol, Graph, LClause).

add_nodes_wmol([],_, _, _, _, []).
add_nodes_wmol([N|Nodes], MoreNodes, Processed, Mol, Graph, LClauseF) :-
	vmember(N, Processed), !,
	add_nodes_wmol(Nodes, MoreNodes, Processed, Mol, Graph, LClauseF).
add_nodes_wmol([(NT.NV)|Nodes], MoreNodes, Processed, Mol, Graph, LClauseF) :-
        wundgraph_wneighbors((NT.NV), Graph, Neighs),
	(
	 dooptimise(depth)
	->
	 % add to the front of the list
	 add_neighbs(Neighs, MNodes, Processed, Nodes),
	 NodesR = MoreNodes
	;
	 % add to the tail of the list
	 add_neighbs(Neighs, MoreNodes, Processed, NodesR),
	 Nodes = MNodes
	),
	add_edges_wmol(Neighs, Processed, first, NT, NV, Mol, LClauseF, LClauseI),
	add_nodes_wmol(MNodes, NodesR, [(NT.NV)|Processed], Mol, Graph, LClauseI).

add_edges_wmol([], _, _, _, _, _, LClause, LClause).
add_edges_wmol([(T.V)-W|Edges], Processed, first, NT, NV, Mol, LClauseF, LClause0) :-
	vmemberdel((T.V),Processed,IProcessed), !,
	LClauseF = [atm_bond(Mol,V,NV,W,NT)|LClauseI],
	add_diffs_wmol(IProcessed, NT, NV, LClauseI, LClause1),
	add_edges_wmol(Edges, Processed, non_first, NT, NV, Mol, LClause1, LClause0).
add_edges_wmol([(T.V)-W|Edges], Processed, non_first, NT, NV, Mol, LClauseF, LClause0) :-
	vmember((T.V),Processed), !,
	LClauseF = [atm_bond(Mol,V,NV,W,NT)|LClauseI],
	add_edges_wmol(Edges, Processed, non_first, NT, NV, Mol, LClauseI, LClause0).
add_edges_wmol([_|Edges], Processed, First, NT, NV, Mol, LClauseF, LClause0) :-
	add_edges_wmol(Edges, Processed, First, NT, NV, Mol, LClauseF, LClause0).

add_diffs_wmol([], _, _, LClause, LClause).
add_diffs_wmol([(T.V)|Processed], T, V0, [V0\=V|LClauseI], LClause2) :- !,
	add_diffs_wmol(Processed, T, V0, LClauseI, LClause2).
add_diffs_wmol([_|Processed], T, V0, LClauseI, LClause2) :- !,
	add_diffs_wmol(Processed, T, V0, LClauseI, LClause2).

add_neighbs([], MoreNodes, _, MoreNodes).
add_neighbs([N-_|Neighs], [N|MoreNodes], Processed, NodesR) :-
	\+ vmember(N, Processed), !,
	add_neighbs(Neighs, MoreNodes, Processed, NodesR).
add_neighbs([_|Neighs], MoreNodes, Processed, NodesR) :-
	add_neighbs(Neighs, MoreNodes, Processed, NodesR).

rewrite_atom_goals((B,Bs),NBs,L0) :- !,
	rewrite_atom_goal(B,NBs,IBs,L0,NL),
	rewrite_atom_goals(Bs,IBs,NL).
rewrite_atom_goals(B,NB,L) :-
	rewrite_atom_goal(B,NB,[],L,_).

rewrite_atom_goal(atom(Mol,At,Type),[atom(Mol,At,Type)|NL],L,VL,[At-Type|VL]) :-
	\+ vmember(At-Type,VL), !,
	add_constraints(At, Type, _, VL, L, NL).
rewrite_atom_goal(atom_bond(Mol,A1,A2,E1,_,B),[atm_bond(Mol,A2,A1,B,E1)|NL],L,VL,[A1-E1|VL]) :-
	\+ vmember(A1-E1,VL), !,
	add_constraints(A1, E1, A2, VL, L, NL).
rewrite_atom_goal(atom_bond(Mol,A1,A2,_,E2,B),[atm_bond(Mol,A1,A2,B,E2)|NL],L,VL,[A2-E2|VL]) :-
	\+ vmember(A2-E2,VL), !,
	add_constraints(A2, E2, A2, VL, L, NL).
rewrite_atom_goal(G,[G|L],L,VL,VL).


add_constraints(_, _, _, [], L, L).
add_constraints(A, T, A2, [V-T|VL], L, [A\=V|NL]) :- V \== A2, !,
	add_constraints(A, T, V, VL, L, NL).
add_constraints(A, T, V, [_|VL], L, NL) :-
	add_constraints(A, T, V, VL, L, NL).

list2goals([], true).
list2goals([G], G) :- !.
list2goals([G|List], (G,NB)) :-
	    list2goals(List, NB).

% get rid of reverse links (eg A-B and B-A)
check_duplicates((_ :- Gs)) :-
        refinable(Gs, [], _).

refinable((G,Gs),L0,LGs) :- !,
	refinable(Gs,L0,LGs1),
	refinable(G,LGs1,LGs).
refinable(G,L,[G|L]) :-
	acceptable_bond(G,L).

% make sure we have no back links in a clause to expand. Bheak!
acceptable_bond(false,_).
acceptable_bond(atom(_,_,_),_).
acceptable_bond(atom_bond(M,A,B,C,D,T),LGs) :-
	\+ vmember(atom_bond(M,B,A,D,C,T),LGs).



user_goal_fails(Current, Type, Head, _, _, Goals) :-
	(
	 user:example(Current,Type,Head),
	 call(user:Goals)
	->
	 fail
	;
	 true
	).

prove_bds((arg(A,B,C),Gs)) :- !,
	arg(A,B,C),
	prove_bds(Gs).
prove_bds((atom_bond(Mol,G1,G2,T1,T2,B),Gs)) :- !,
	user:atm_bond(Mol,G1,G2,T1,T2,B),
	prove_bds(Gs).
prove_bds(arg(A,B,C)) :-
	arg(A,B,C).
prove_bds(atom_bond(Mol,G1,G2,T1,T2,B)) :-
	user:atm_bond(Mol,G1,G2,T1,T2,B).

:- initialization(nb_setval(red_tries, -1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Canonic
init_canonic:-
	nb_getval(red_tries, V),
	(V == -1 -> true ; trie_close(V)),
	trie_open(NV), nb_setval(red_tries,NV).

%%%%%%%%%%%%%
% canonic(+G)
canonic(_G):-
	canonize(off),!.
canonic(G):-
	canonic1(G).

canonic1((_:-false)) :- !.
canonic1((_:-atom(_,_,C))) :- !,
%        G = [atom(C)],
	normalized_graph((atom(_,_,C)),G,_),
        nb_getval(red_tries,Trie),
	\+ trie_check_entry(Trie,G,_),
	trie_put_entry(Trie,G,_),
	\+ breaks_constraint(G).

canonic1((_:-Gs)):-
	split_clause(Gs, GAtt, G),
	G \= false, !,
	simplify_atts_for_canonic(GAtt, GA),
	normalized_graph(G,Graph,SortedNodes),
	build_clause(SortedNodes, Graph, LClause),
	FullLClause = [GA|LClause],
	nb_getval(red_tries,Trie),
	\+ trie_check_entry(Trie,FullLClause,_),
	trie_put_entry(Trie,FullLClause,_),
	\+ breaks_constraint(Graph).

canonic1((_:-GAtt)) :-
	simplify_atts_for_canonic(GAtt, GA),
	nb_getval(red_tries,Trie),
	\+ trie_check_entry(Trie,GA,_),
	trie_put_entry(Trie,GA,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% normalized_graph(+Gs,-NormGs)
normalized_graph(Gs,NormGs):-
	normalized_graph(Gs,NormGs,_SortedGs).
% normalized_graph(+Gs,-NormGs,-SortedGs)
normalized_graph((atom(A,B,C)),Graph,[atom(C)]):-!,
	wundgraph_new(Graph0),
	make_graph((atom(A,B,C)), Graph0, Graph).
normalized_graph(Gs,Graph,SortedGs):-
	split_clause(Gs, _GAtt, G),
	wundgraph_new(Graph0),
	make_graph(G, Graph0, Graph),
	morgan(Graph, SortedGs),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simplification of the clauses
simplify_atts_for_canonic(false, false) :- !.
% drop the first argument
simplify_atts_for_canonic('Methyl'(_A,B), 'Methyl'(B)) :- !.
simplify_atts_for_canonic('Nh2'(_A,B,C,D), 'Nh2'(B,C,D)) :- !.
simplify_atts_for_canonic('Aldehyde'(_A,B,C,D), 'Aldehyde'(B,C,D)) :- !.
simplify_atts_for_canonic('Hydroxil'(_A,B,C), 'Hydroxyl'(B,C)) :- !.
simplify_atts_for_canonic('Cyano'(_A,B,C), 'Cyano'(B,C)) :- !.
simplify_atts_for_canonic('Ketone'(_A,B,C), 'Ketone'(B,C)) :- !.
simplify_atts_for_canonic('Benzene'(_A,B), 'Benzene'(B)) :- !.
simplify_atts_for_canonic('Carboxylic_acid'(_A1,A,B,C,D),'Carboxylic_acid'(A,B,C,D)) :- !.
% binary predicates
simplify_atts_for_canonic(Prop, Prop) :- 
	functor(Prop,F,_Arity),
	binary_att_member(F),
	!.

simplify_atts_for_canonic((G1,Gs), (NG1,NGs)) :- !,
	simplify_atts_for_canonic(G1, NG1),
	simplify_atts_for_canonic(Gs, NGs).
simplify_atts_for_canonic(GAtt, NGAtt) :-
	GAtt =.. [Att,_|Gs],
	NGAtt =.. [Att|Gs].


binary_att_member('Mass').
binary_att_member('MOL_POLARIZABILITY').
binary_att_member('ACCCOUNT').
binary_att_member('DONCOUNT').
binary_att_member('Heavy').
binary_att_member('Mass').
binary_att_member('TPSA').
binary_att_member('LogD').
binary_att_member('LogP').
binary_att_member('BCUT').
binary_att_member('RF').
binary_att_member('gte').
binary_att_member('lte').

make_graph((Node,G),Graph0,Graph) :- !,
	add_node(Node,Graph0,GraphI),
	make_graph(G,GraphI,Graph).
make_graph(Node,Graph0,Graph) :-
	add_node(Node,Graph0,Graph).

add_node(atom(_,V,T),Graph0,Graph) :-
	wundgraph_add_vertex(Graph0,(T.V),Graph).
add_node(atom_bond(_,V1,V2,T1,T2,W),Graph0,Graph) :-
	wundgraph_add_edge(Graph0,(T1.V1),(T2.V2),W,Graph).

morgan(Graph0, SortedNodes) :-
	wundgraph_vertices(Graph0,Vs),
	init_morgan(Graph0, Vs, Map),
	relaxation(Graph0, Vs, Map, SortedNodes, 0).

init_morgan(Graph0,Vs,Tree) :-
	rb_new(Tree0),
	add_initial_vertices(Vs, Graph0, Tree0, Tree).

add_initial_vertices([], _, RB, RB).
add_initial_vertices([V|Vs], Graph0,RB,Tree) :-
	wundgraph_neighbors(V, Graph0, Kids),
	length(Kids, NKids),
	rb_insert(RB, V, NKids, RB1),
	add_initial_vertices(Vs, Graph0, RB1, Tree).

relaxation(Graph0, Vs, Map, Nodes, Different) :-
	increase(Vs, Graph0, Map, Map, MapI, New),
	count_different(New, NewDifferent),
	(
	 NewDifferent =:= Different
	->
	 sorted_nodes(MapI, Graph0, Nodes)
	;
	 relaxation(Graph0, Vs, MapI, Nodes, NewDifferent)
	).

increase([], _, _, Map, Map, []).
increase([V|Vs], Graph0, Map0, Map, NMap, [Tot|New]) :-
	wundgraph_neighbors(V, Graph0, Kids),
	weight(Kids, Map, 0, Tot),
	rb_update(Map,V,Tot,Map1),
	increase(Vs, Graph0, Map0, Map1, NMap, New).

weight([], _, Tot, Tot).
weight([V|Kids], Map, Tot0, Tot) :-
	rb_lookup(V,Val,Map),
	Tot1 is Tot0+Val,
	weight(Kids, Map, Tot1, Tot).

sorted_nodes(Map, G0, Queue) :-
	rb_visit(Map,Nodes),
	getmax_el(Nodes,G0,El0),
	Queue = [El0|QueueF],
	process_queue(Queue, Map, G0, QueueF).
		      
process_queue([], _, _, _) :- !.
process_queue([K|Queue], Map, G0, QueueF) :-
	rb_delete(Map,K,_,Map1), !,
	wundgraph_neighbors(K, G0, Kids),
	sort_kids_by_weights(Kids, Map1, G0, QueueF, NQueueF),
	process_queue(Queue, Map1, G0, NQueueF).
process_queue([_|Queue], Map, G0, QueueF) :-
	process_queue(Queue, Map, G0, QueueF).

sort_kids_by_weights(Kids, Map, G, QueueF, NQueueF) :-
	weigh_kids(Kids, Map, WKids),
	sort(WKids, SKids),
	reorder_duplicates(SKids, G, RSKids),
	add_wkids_2queue(RSKids, QueueF, NQueueF).

weigh_kids([], _, []).
weigh_kids([K|Kids], Map, [Val-K|WKids]) :-
	rb_lookup(K, Val, Map), !,
	weigh_kids(Kids, Map, WKids).
weigh_kids([_|Kids], Map, WKids) :-
	weigh_kids(Kids, Map, WKids).

reorder_duplicates([], _, []).
reorder_duplicates([Lab-(T.V),Lab-(T.V1)|SKids], G, NewKids) :-
	fetch_extra(SKids, Lab, T, Extra, Left),
	add_info([(T.V),(T.V1)|Extra],G,Info),
	sort(Info, Sort),
	add_info_kids(Sort, NewKids, RSKids),
	reorder_duplicates(Left, G, RSKids).
reorder_duplicates([Kid|SKids], G, [Kid|RSKids]) :-
	reorder_duplicates(SKids, G, RSKids).

add_wkids_2queue([], Queue, Queue).
add_wkids_2queue([_-V|SKids], QueueF, NQueueF) :-
	add_wkids_2queue(SKids, QueueF, [V|NQueueF]).

fetch_extra([], _, _, [], []).
fetch_extra([Lab-(T.V)|SKids], Lab, T, [(T.V)|More], Left) :- !,
	fetch_extra(SKids, Lab, T, More, Left).
fetch_extra(SKids, _, _, [], SKids).

add_info([],_,[]).
add_info([Node|Extra],G,[Info-Node|InfoExtra]) :-
	wundgraph_wneighbors(Node, G, Edges),
	edges2info(Edges, NInfo),
	msort(NInfo, Info),
	add_info(Extra,G,InfoExtra).

edges2info([], []).
edges2info([(T._)-W|Edges], [(T.W)|NInfo]) :-
	edges2info(Edges, NInfo).

add_info_kids([], RSKids, RSKids).
add_info_kids([_-Kid|Sort], [Kid|NewKids], RSKids) :-
	add_info_kids(Sort, NewKids, RSKids).

build_clause([(T.V)|SortedNodes], Graph, [atom(V,T)|LClause]) :-
	add_nodes(SortedNodes, [(T.V)], Graph, LClause).

add_nodes([], _, _, []).
add_nodes([(NT.NV)|SortedNodes], Processed, Graph, LClauseF) :-
        wundgraph_wneighbors((NT.NV), Graph, Edges),
	add_edges(Edges, Processed, NT, NV, LClauseF, LClauseI),
	add_nodes(SortedNodes, [(NT.NV)|Processed], Graph, LClauseI).

add_edges([], _, _, _, LClause, LClause).
add_edges([(T.V)-W|Edges], Processed, NT, NV, LClauseF, LClause0) :-
	once(vmember((T.V),Processed)), !,
	LClauseF = [atom_bond(V,NV,T,NT,W)|LClauseI],
	add_edges(Edges, Processed, NT, NV, LClauseI, LClause0).
add_edges([_|Edges], Processed, NT, NV, LClauseF, LClause0) :-
	add_edges(Edges, Processed, NT, NV, LClauseF, LClause0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
vmember(A1,[A|_]) :-
	A1 == A.
vmember(A1,[_|L]) :-
	vmember(A1,L).

vmemberdel(T,[T1|L],L) :- T == T1, !.
vmemberdel(T,[T1|L],[T1|NL]) :- 
	vmemberdel(T,L,NL).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
count_different(New, NewDifferent) :-
	sort(New, Sorted),
	length(Sorted, NewDifferent).


getmax_el([H-V|L],G,NH) :-
	max_list(L,[H],V,Els),
	add_info(Els, G, Info),
	sort(Info,[_-NH|_]).

max_list([],H,_,H).
max_list([H-Val|L],H0,Max0,NH) :-
	(
	  Val > Max0 
	->
	  max_list(L,[H],Val,NH)
	;
	  Val =:= Max0 
	->
	  max_list(L,[H|H0],Max0,NH)
	;
	  max_list(L,H0,Max0,NH)
	).

%
% method
%
build_tree([], _, _, []).
build_tree([(T.V)|MoreKids], Graph, C, NGoals) :-
	wundgraph_wneighbors((T.V), Graph, Kids), !,
	wundgraph_del_vertex(Graph, (T.V), NGraph),
	compute_kids(Kids, NGraph, SKids, Nodes),
	generate_goals(NGoals, C, T, V, SKids, Goals),
	append(MoreKids, Nodes, List),
	build_tree(List, NGraph, C, Goals).
build_tree([_|Kids], Graph, C, Goals) :-
	build_tree(Kids, Graph, C, Goals).


generate_goals(Goals, _, _, _, [], Goals) :- !.
generate_goals([atom_bond(V,NV,T,NT,W)|NGoals], C, T, V, [W-(NT.NV)|SKids], Goals) :-
	generate_goals(NGoals, C, T, V, SKids, Goals).

compute_kids(Kids0, G, Edges, Nodes) :-
	reverse_edges(Kids0, Kids1),
	sort(Kids1, Kids2),
	larger_first(Kids2, Kids2, G, NKids2),
	nodes(NKids2, Edges, Nodes).

reverse_edges([],[]).
reverse_edges([El-W|Kids],  [W-El|NKids]) :-
	reverse_edges(Kids, NKids).

nodes([],[], []).
nodes([W-K|Kids0],[W-K|Kids2], [K|Kids1]) :-
	nodes(Kids0,Kids2,Kids1).


larger_first([], _, _, []).
larger_first([W-(T.V)|RKids], Kids2, G, Sorted) :-
	similar_kids(RKids, W, T, Similar, RKids2),
	Similar = [_|_], !,
	add_sorted([(T.V)|Similar], W, [(T.V)|Similar], G, Sorted, NKids2),
	larger_first(RKids2, Kids2, G, NKids2).
larger_first([A|RKids], Kids2, G, [A|Sorted]) :-
	larger_first(RKids, Kids2, G, Sorted).

similar_kids([W-(T.V)|RKids], W, T, [(T.V)|Similar], RKids2) :- !,
	similar_kids(RKids, W, T, Similar, RKids2).
similar_kids(Kids2, _, _, [], Kids2).

%
% greedy search failed, what now?
%
add_sorted(Similar, W, Kids2, G, Sorted, NKids2) :-
	compute_new_weights(Similar, G, Kids2, NSimilar),
	sort(NSimilar, SSimilar),
	add_weights(SSimilar, W, Sorted, NKids2).

compute_new_weights([], _, _, []).
compute_new_weights([Node|Similar], G, Kids2, [W-Node|NSimilar]) :-
	delete_other_kids(Kids2, Node, NKids),
	wundgraph_del_vertices(G,NKids,NG),
	wundgraph_reachable(Node,NG,W0),
	sort(W0, W),
	compute_new_weights(Similar, G, Kids2, NSimilar).


delete_other_kids([], _, []).
delete_other_kids([El|Kids2], Node, [El|NKids]) :-
	El \== Node, !,
	delete_other_kids(Kids2, Node, NKids).
delete_other_kids([_|Kids], _, Kids).

add_weights([], _, L, L).
add_weights([_-Node|SSimilar], W, [W-Node|Sorted], More) :-
	add_weights(SSimilar, W, Sorted, More).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constraints
:- style_check(-discontiguous).
:- dynamic_predicate([const/1],logical).

% constraint(+Con)
constraint(Con) :-
	preprocess_constraint(Con,PreCon),
	assert(const(PreCon)),
	(numbervars(Con,0,_),format('Added constraint: ~q~n',[Con]),fail;true),
	!.

preprocess_constraint((atom(A,B,C)),PreConstr):-!,
	normalized_graph((atom(A,B,C)),PreConstr).
preprocess_constraint((atom(A,B,C),Gs),PreConstr):-!,
	normalized_graph((atom(A,B,C),Gs),PreConstr).
preprocess_constraint((_,Gs),PreConstr):-% ignore the properties
	preprocess_constraint(Gs,PreConstr).


clause_to_edge_list((AtomBond,Gs)) -->
	!,
	atom_bond_to_edge(AtomBond),
	clause_to_edge_list(Gs).
clause_to_edge_list(AtomBond) -->
	atom_bond_to_edge(AtomBond).

atom_bond_to_edge(atom_bond(_,A1,A2,T1,T2,W)) -->
	[A1.T1-(A2.T2-W)].

breaks_constraint(W2) :-
	const(W1),
	format('Check Constraint...~n~q~n~q~n???????????~n',[W1,W2]),
	subgraph(W1, W2), 
	format('Constraint violated.~n',[]),
	!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% holds true if W1 is subgraph of W2
% subgraph(+W1, +W2)
subgraph(W1, W2):-
	format('subgraph:~q~n',[subgraph(W1, W2)]),
	wundgraph_vertices(W1,Vert1),
	wundgraph_vertices(W2,Vert2),
	length(Vert1,S1),
	length(Vert2,S2),
	subgraph(S1,S2,Vert1,Vert2,W1,W2).

subgraph(1,1,V,V,_,_):-!.
subgraph(1,_,[N],V2,_,_):-!,
	member(N,V2).
subgraph(_,_,_,_,W1,W2):-
	dgraphs:dgraph_edges(W1,  [W1Edge|W1Edges]),
	dgraphs:dgraph_edges(W2,  W2Edges),
	%format('call ~q~n',[subgraph_match(Edge, W2Edges, W1Edges, W2)]),
	subgraph_match(W1Edge, W2Edges, W1Edges, W2Edges).

% subgraph_match(+Edge_MatchGraph,EdgesMatchedGraph,Edges_MatchGraph,MatchedGraph)
subgraph_match(Edge, [Edge|Edges], W1, W2):-
%	W1Edge = N1-(N2-K),                       % (N1)---K---(N2)
%	wundgraph_del_edge(W2,N1,N2,K,W22),
	subgraph_match_more_edges([Edge|W1], W2).
subgraph_match(Edge, [_|Edges], W1, W2) :-
	subgraph_match(Edge, Edges, W1, W2).

%
subgraph_match_more_edges([], _).
subgraph_match_more_edges([Edge|Edges], W2) :-
%	Edge = N1-(N2-K),
	delete(W2,Edge,W22),
	subgraph_match_more_edges(Edges, W22).

test:-
	spy(logchem:subgraph),
	logchem:subgraph(t(black('',_141250,_141251,''),black(black('',_141250,_141251,''),['Br'|_141264],[],black('',_141250,_141251,''))),t(black('',_140932,_140933,''),black(black('',_140932,_140933,''),['Br'|_140808],[['C'|_140867]-1],red(black('',_140932,_140933,''),['C'|_140867],[['Br'|_140808]-1],black('',_140932,_140933,''))))).
%	logchem:subgraph(t(black('',_141461,_141462,''),black(red(black('',_141461,_141462,''),['C'|_141480],[['C'|_141487]-1],black('',_141461,_141462,'')),['C'|_141487],[['C'|_141480]-1,['Cl'|_141508]-1],red(black('',_141461,_141462,''),['Cl'|_141508],[['C'|_141487]-1],black('',_141461,_141462,'')))),t(black('',_140891,_140892,''),black(red(black('',_140891,_140892,''),['C'|_140696],[['C'|_140758]-1],black('',_140891,_140892,'')),['C'|_140758],[['C'|_140696]-1,['Cl'|_140822]-1],red(black('',_140891,_140892,''),['Cl'|_140822],[['C'|_140758]-1],black('',_140891,_140892,''))))).
%	logchem:subgraph(t(black('',_141250,_141251,''),black(red(black('',_141250,_141251,''),['C'|_141269],[['C'|_141276]-1],black('',_141250,_141251,'')),['C'|_141276],[['C'|_141269]-1,['Cl'|_141297]-1],red(black('',_141250,_141251,''),['Cl'|_141297],[['C'|_141276]-1],black('',_141250,_141251,'')))),t(black('',_140932,_140933,''),black(black('',_140932,_140933,''),['Br'|_140808],[['C'|_140867]-1],red(black('',_140932,_140933,''),['C'|_140867],[['Br'|_140808]-1],black('',_140932,_140933,''))))).


