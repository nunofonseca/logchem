%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Copyright 2010 Nuno A. Fonseca (nunofonseca at acm.org)
%                            
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


go:-
	unix(argv(Args)),
	check_args(Args,[Src2Load,Dataset]),
	consult(Src2Load),
	read_all(Dataset),
	induce,
	write_rules.

check_args([Src,DS],Src,DS):-!.
check_args(_,_):-
	format('Error: Usage: lc_runaleph.pl src  dataset',[]),
	nl,
	halt.
:-prolog_initialization(go).
