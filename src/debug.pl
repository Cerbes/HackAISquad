:- module(debug, [
                        write_list/1,
                        write_program/0,
                        if_debug/2,
                        force/1,
                        indent/1
                 ]).

/** <module> Debugging predicates

Predicates used only for debugging and that will have no purpose in a final
release.

@author Kyle Marple
@version 20151129
@license GPL
*/

/*
* Copyright (C) 2015 Kyle Marple <kbm072000@utdallas.edu>
* 
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 2 of the License, or
* (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License along
* with this program; if not, write to the Free Software Foundation, Inc.,
* 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
*/

:- use_module(library(writef)).
:- use_module(common).
:- use_module(options).
:- use_module(output). % for formatting and metacalls to fill_in_variable_values/5.
:- use_module(program).
:- use_module(variables). % for formatting and new_var_struct/1.

%! debugging(+Level:int)
% Define debugging(Level) to enable if_debug/2 statements with a level less than
% or equal to the given one.
%
% @param Level The debugging level.
% @see See config.pl for default setting.
debugging(X) :-
        user_option(debug, X),
        !.

%! write_list(+List:list) is det
% Write a list with one element per line. This is just for debugging purposes.
%
% @param List The list to write.
write_list([X | T]) :-
        writef('%w\n', [X]),
        !,
        write_list(T).
write_list([]).

%! write_program is det
% Write the rules, NMR check and query. Just for debugging purposes.
write_program :-
        findall(R, (defined_rule(_, H, B), rule(R, H, B)), Rs),
        write('Rules:\n'),
        write_rules(Rs, 0),
        new_var_struct(V),
        (defined_nmr_check(NMR) ->
                format_term_list(NMR, NMR2, _, V),
                writef('\nNMR Check:\n%w\n', [NMR2])
        ;
                true
        ),
        (defined_query(Q, _) ->
                format_term_list(Q, Q2, _, V),
                writef('\nQuery\n%w\n', [Q2])
        ;
                true
        ),
        write('\n'),
        !.

%! write_rules(+Rules:list, +LastHead:compound) is det
% Write the rules in the program, formatted for readability.
%
% @param Rules The list of rules.
% @param LastHead The functor of the head of the last rule. Check to print a
%        blank line between clauses of different predicates. Use 0 for initial
%        value to avoid extra blank line at start.
write_rules([X | T], Lh) :-
        rule(X, H, B),
        H =.. [F | _],
        ((F \= Lh, Lh \= 0) -> % blank line between predicates
                write('\n')
        ;
                true
        ),
        new_var_struct(V),
        format_term(H, H2, _, V),
        writef('%w', [H2]),
        write_body(B),
        writef('.\n'),
        !,
        write_rules(T, F).
write_rules([], _).

%! write_body(+Goals:list) is det
% Write the body of a rule. Cannot be empty.
%
% @param Goals The body of the rule.
write_body([X | T]) :-
        new_var_struct(V),
        format_term(X, X2, _, V),
        writef(' :-\n\t%w', [X2]),
        write_body2(T).
write_body([]).

%! write_body2(+Goals:list) is det
% Write the body of a rule. Cannot be empty.
%
% @param Goals The body of the rule.
write_body2([X | T]) :-
        new_var_struct(V),
        format_term(X, X2, _, V),
        writef(',\n\t%w', [X2]),
        !,
        write_body2(T).
write_body2([]).

%! if_debug(+Level:int, +Call:callable)
% Run the given call if debugging(L) is defined such that Level =< L, otherwise
% just succeed.
%
% @param Call The call to run when debugging is enabled.
if_debug(L, C) :-
        debugging(L2),
        L =< L2,
        !,
        call(C).
if_debug(L, _) :-
        debugging(L2),
        L > L2,
        !.
if_debug(_, _) :-
        \+debugging(_).

%! force(+Call:callable) is nondet
% Call the goal on backtracking, even if it would otherwise be deterministic.
% The primary use is to force write/1 or writef/2 to print when backtracking.
%
% @param Call The goal to call.
force(X) :-
        once(call(X)).
force(X) :-
        write('force! '),
        call(X),
        !,
        fail.

%! indent(+Level:int) is det
% Write Level spaces.
%
% @param Level The level to indent to.
indent(N) :-
        N > 0,
        N1 is N - 1,
        write(' '),
        !,
        indent(N1).
indent(0).
