%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%    This file is part of LogCHEM.
%
%    $Id: func_groups.pl,v 1.3 2010/06/30 14:56:46 nf Exp $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Functional Groups - Background Knowlwdge
%% :- modeb(*,'Methyl'(+drug,-ring)).
%% :- determination(active/1,methyl/2).
%% :- modeb(*,'Nh2'(+drug,+atomid,-atomid,-atomid)).
%% :- determination(active/1,nh2/4).
%% :- modeb(*,'Aldehyde'(+drug,+atomid,-atomid,-atomid)).
%% :- determination(active/1,aldehyde/4).
%% :- modeb(*,'Carboxylic_acid'(+drug,-ring)).
%% :- determination(active/1,carboxylic_acid/2).
%% :- modeb(*,'Hydroxyl'(+drug,+atomid,-atomid)).
%% :- determination(active/1,hydroxyl/3).
%% :- modeb(*,'Cyano'(+drug,+atomid,-atomid)).
%% :- determination(active/1,cyano/3).
%% :- modeb(*,'Ketone'(+drug,+atomid,-atomid)).
%% :- determination(active/1,ketone/3).
%% :- modeb(*,'Benzene'(+drug,-ring)).
%% :- determination(active/1,benzene/2).

% Methyl
'Methyl'(D,[A1,A2,A3,A4,A5]) :-
   atom(D,A2,'C'),
   bond(D,A1,A2,1),
   atom(D,A1,Type), 
   Type \= 'H',
   bond(D,A2,A3,1),
   atom(D,A3,'H'), 
   bond(D,A2,A4,1),
   A4 > A3,
   atom(D,A4,'H'), 
   bond(D,A2,A5,1),
   A5 > A4,
   atom(D,A5,'H').

% Amino
'Nh2'(D,A1,A2,A3) :-
   atom(D,A1,'N'),
   atom(D,A2,'H'),
   atom(D,A3,'H'),
   A2 \= A3,
   bond(D,A1,A2,1),
   bond(D,A1,A3,1).
   
% Aldehyde
'Aldehyde'(D,A1,A2,A3) :-
   atom(D,A1,'C'),
   atom(D,A2,'H'),
   atom(D,A3,'O'),  
   bond(D,A1,A2,1),
   bond(D,A1,A3,2).

% Carboxylic acid
'Carboxylic_acid'(D,[A1,A2,A3,A4]) :-
   atom(D,A1,'C'),
   atom(D,A2,'O'),
   atom(D,A3,'O'),
   atom(D,A4,'H'),
   A2 \= A3,
   bond(D,A1,A2,2),
   bond(D,A1,A3,1),
   bond(D,A3,A4,1). 

% Hydroxyl
'Hydroxyl'(D,A1,A2) :-
   atom(D,A1,'O'),
   atom(D,A2,'H'),
   bond(D,A1,A2,1).

% Cyano
'Cyano'(D,A1,A2) :-
   atom(D,A1,'C'),
   atom(D,A2,'N'),
   bond(D,A1,A2,3).

% Ketone
'Ketone'(D,A1,A2) :-
   atom(D,A1,'C'),
   atom(D,A2,'O'),
   bond(D,A2,A1,2).
'Ketone'(D,A1,A2) :-
   atom(D,A1,'C'),
   atom(D,A2,'O'),
   bond(D,A1,A2,2).


% 'Benzene' ring
'Benzene'(D,[A1,A2,A3,A4,A5,A6]) :-
   atom(D,A1,'C'),
   atom(D,A2,'C'),
   atom(D,A3,'C'),
   atom(D,A4,'C'),
   atom(D,A5,'C'),
   atom(D,A6,'C'),
   A3 \= A2,
   A4 \= A1,
   A5 \= A1,
   A6 \= A2,
   A6 \= A3,
   bond(D,A1,A2,1),
   bond(D,A1,A3,1),
   bond(D,A2,A4,1),
   bond(D,A3,A5,1),
   bond(D,A4,A6,1),
   bond(D,A5,A6,1).

