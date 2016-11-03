:- module(output, [
                        format_term/4,
                        format_term_list/4,
                        print_chs/3,
                        fill_in_variable_values/5,
                        print_vars/2,
                        print_var_constraints/1
                     ]).

/** <module> Output formatting and printing.

Predicates related to formatting and printing output. This includes predicates
that may be used for warning and error output.

@author Kyle Marple
@version 20160510
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

:- use_module(library(lists)).
:- use_module(library(rbtrees)).
:- use_module(library(writef)).
:- use_module(chs).
:- use_module(common).
:- use_module(program).
:- use_module(variables).

%! format_term(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +Vars:compound) is det
% Format a term for printing.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param Vars Variable struct for filling in values.
format_term(X, X, Con, V) :-
        is_unbound(X, V, Con, _, _), % constrained var
        !.
format_term(X, Xo, Con, V) :-
        is_var(X),
        var_value(X, V, val(X2)), % bound var
        !,
        format_term(X2, Xo, Con, V).
format_term(X, Xo, Con, V) :-
        X =.. [_ | _], % non-var
        !,
        format_predicate(X, Xo, Con, [], _, V).
format_term(X, X, [], _) :- % anything else, just pass along
        !.

%! format_term_list(+ListIn:compound, -ListOut:compound, -Constraints:list, +Vars:compound) is det
% Format each term in a list.
%
% @param ListIn The initial list.
% @param ListOut The formatted list.
% @param Constraints The list of constraints. MAY CONTAIN DUPLICATES!
% @param Vars Variable struct for filling in values.
format_term_list([X | T], [X2 | T2], Con, V) :-
        format_term(X, X2, C, V),
        !,
        format_term_list(T, T2, Ct, V),
        append(C, Ct, Con).
format_term_list([], [], [], _).

%! print_chs(+CHS:list, +Vars:compound, +Flag:int) is det
% Print the CHS, removing internally added information and formatting.
% Flag indicates the print mode: 0 for normal (positive and negative printing
% literals), 1 for positive literals only, 2 for all (including non-printing)
% literals.
%
% @param CHS The CHS to print.
% @param Vars A variable struct to get bindings for each variable.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
print_chs(CHS, V, Flag) :-
        rb_visit(CHS, CHS2),
        rb_visit_to_list(CHS2, [], CHS3),
        format_chs(CHS3, CHS4, V, Flag),
        !,
        print_chs2(CHS4),
        !.
print_chs(_, _, _) :-
        write_error('could not print CHS'),
        !,
        fail.        

%! rb_visit_to_list(+RBvisit:list, +ListIn:list, -ListOut:list) is det
% Given the results of rb_visit/2, strip the keys from each member.
%
% @param RBvist A Red-Black tree visit.
% @param ListIn Input list.
% @param List Output list.
rb_visit_to_list([-(_, X) | T], Li, Lo) :-
        append(X, Li, L2),
        !,
        rb_visit_to_list(T, L2, Lo).
rb_visit_to_list([], L, L) :-
        !.

%! print_chs2(+CHS:list) is det
% Print the formatted CHS produced by format_chs/4, accounting for negated
% literals.
%
% @param CHS The formatted CHS.
print_chs2([X | T]) :-
        write('{ '),
        X = -(X2, Con),
        (X2 = not(X3) -> % print negation properly
                writef('not %w', [X3])
        ;
                writef('%w', [X2])
        ),
        (Con \= [] ->
                write(' ( '),
                print_var_constraints(Con),
                write(' )')
        ;
                true
        ),
        !,
        print_chs3(T),
        write(' }').
print_chs2([]) :-
        write('{ }').

%! print_chs3(+CHS:list) is det
% Print the rest of the formatted CHS.
%
% @param CHS The CHS to print.
print_chs3([X | T]) :-
        X = -(X2, Con),
        (X2 = not(X3) -> % print negation properly
                writef(', not %w', [X3])
        ;
                writef(', %w', [X2])
        ),
        (Con \= [] ->
                write(' ( '),
                print_var_constraints(Con),
                write(' )')
        ;
                true
        ),
        !,
        print_chs3(T).
print_chs3([]) :-
        !.

% format_chs(+CHS:list, -PrettyCHS:list, +Vars:compound, +Flag:int) is det
% Format CHS entries for printing. Leave negated literals wrapped in not(), to
% be handled later. Flag indicates the print mode: 0 for normal (positive and
% negative printing literals), 1 for positive literals only, 2 for all
% (including non-printing) literals.
%
% @param CHS The CHS.
% @param PrettyCHS The formatted CHS.
% @param Vars The variable struct used to fill in values.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
format_chs(CHSi, CHSo, V, F) :-
        F \= 2,
        once(get_next_printable(CHSi, CHS2)),
        CHS2 \= [],
        !,
        format_chs2(CHS2, CHS3, [], _, V, F), % Reduce number of unique vars in CHS
        sort_chs(CHS3, CHS4), % sort entries by literal and then constraints
        divide_chs(CHS4, CHSp, CHSn),
        (F = 0 ->
                append(CHSp, CHSn, CHSo) % print pos and neg, puttting negative entries after positive ones.
        ;
                CHSo = CHSp % print positive literals only
        ).
format_chs(CHSi, CHSo, V, F) :-
        CHSi \= [],
        F = 2,
        !,
        format_chs2(CHSi, CHS2, [], _, V, F), % Reduce number of unique vars in CHS
        sort_chs(CHS2, CHS3), % sort entries by literal and then constraints
        divide_chs(CHS3, CHSp, CHSn),
        append(CHSp, CHSn, CHSo). % put negative entries after positive ones.
format_chs(_, [], _, _). % CHS is either empty or contains no printable literals

% format_chs2(+CHS:list, -PrettyCHS:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound, +Flag:int) is det
% Format CHS entries for printing. Leave negated literals wrapped in not(), to
% be handled later. Attach constraints. Flag indicates the print mode: 0 for
% normal (positive and negative printing literals), 1 for positive literals
% only, 2 for all (including non-printing) literals.
%
% @param CHS The CHS.
% @param PrettyCHS The formatted CHS. Members will be of the form
%        -(Literal, Constraints).
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars The variable struct used to fill in values.
% @param Flag An integer 0, 1 or 2 indicating the print mode.
format_chs2([X | T], [Y | T2], Uvi, Uvo, V, F) :-
        F \= 2,
        format_chs_entry(X, X2, Con, Uvi, Uv1, V),
        Y = -(X2, Con),
        get_next_printable(T, T3),
        !,
        format_chs2(T3, T2, Uv1, Uvo, V, F).
format_chs2([X | T], [Y | T2], Uvi, Uvo, V, F) :-
        F = 2,
        !,
        format_chs_entry(X, X2, Uvi, Uv1, Con, V),
        Y = -(X2, Con),
        !,
        format_chs2(T, T2, Uv1, Uvo, V, F).
format_chs2([], [], Uv, Uv, _, _).

%! format_chs_entry(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Format a CHS entry for printing.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_chs_entry(X, Xo, Con, Uvi, Uvo, V) :-
        chs_entry(X, X2, A, _),
        X3 =.. [X2 | A],
        format_predicate(X3, Xo, Con, Uvi, Uvo, V).

%! format_predicate(+EntryIn:compound, -EntryOut:compound, -Constraints:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Given a term, call format_predicate2/5, fill in variable values and
% process constraints.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param Constraints Any constraints on variables in the entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate(X, Xo, Con, Uvi, Uvo, V) :-
        fill_in_variable_values(X, X2, [], _, V), % fill in bound vars; ignore constraints for now.
        format_predicate2(X2, X3, Uvi, Uv2, V),
        fill_in_variable_values(X3, Xo, [], Con2, V), % get any constraints.
        format_predicate3(Con2, Con3, Uv2, Uvo, V), % format any terms in constraints
        sort(Con3, Con).

%! format_predicate2(+EntryIn:compound, -EntryOut:compound, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Given a term, remove the arity, strip any prefixes, and process arguments.
%
% @param EntryIn The initial entry.
% @param EntryOut The formatted entry.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
        Xi = [_ | _], % list
        !,
        format_predicate3(Xi, Xo, Uvi, Uvo, V).
format_predicate2(Xi, Xo, Uv, Uv, _) :-
        predicate(Xi, X2, []),
        atom(X2), % compound term, predicate or atom
        atom_chars(X2, ['\'' | X3]), % quoted string; strip outermost quotes and arity
        reverse(X3, ['0', '_', '\'' | X4]),
        reverse(X4, X5),
        atom_chars(Xo, X5).
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
        predicate(Xi, X2, A),
        atom(X2), % compound term, predicate or atom
        !,
        split_functor(X2, Xc, _), % strip arity
        atom_chars(X3, Xc),
        strip_prefixes(X3, X4),
        format_predicate3(A, A2, Uvi, Uvo, V),
        (X4 = not(Xn) -> % append args
                Xn2 =.. [Xn | A2],
                Xo = not(Xn2)
        ;
                Xo =.. [X4 | A2]
        ).
format_predicate2(Xi, Xo, Uvi, Uvo, V) :-
        Xi =.. [X2 | A], % compound term, but not a predicate or atom head
        !,
        format_predicate3(A, A2, Uvi, Uvo, V),
        Xo =.. [X2 | A2].
format_predicate2(X, X, Uv, Uv, _) :-
        !. % not a predicate or atom

%! format_predicate3(+ArgsIn:list, -ArgsOut:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Process a list of predicate args or variable constraints.
%
% @param ArgsIn Input args.
% @param ArgsOut Output args.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate3([X | T], [Y | T2], Uvi, Uvo, V) :-
        format_predicate4(X, Y, Uvi, Uv1, V),
        !,
        format_predicate3(T, T2, Uv1, Uvo, V).
format_predicate3(X, Y, Uvi, Uvo, V) :-
        X \= [_ | _],
        X \= [], % can occur if we have a list with an unbound tail
        format_predicate4(X, Y, Uvi, Uvo, V),
        !.
format_predicate3([], [], Uv, Uv, _).

%! format_predicate4(+ArgsIn:list, -ArgsOut:list, +UsedVarsIn:list, +UsedVarsOut:list, +Vars:compound) is det
% Process a single predicate arg or variable constraint. Where possible,
% substitute previously used variables for variables with the same value ID.
% This makes the final output more readable.
%
% @param ArgIn Input arg.
% @param ArgOut Output arg.
% @param UsedVarsIn Input used vars. Format for each is =|-(value_id, var)|=.
% @param UsedVarsOut Output used vars.
% @param Vars Variable struct for filling in values.
format_predicate4(Xi, Xo, Uv, Uv, V) :-
        is_var(Xi),
        atom_chars(Xi, ['?' | Gt]), % get ID without flag
        atom_chars(X2, Gt),
        get_value_id(X2, ID, V),
        member(-(ID, Xo), Uv), % Var with same value already selected.
        !.
format_predicate4(Xi, Xo, Uv, Uv, V) :-
        get_value_id(Xi, ID, V),
        member(-(ID, Xo), Uv), % Var with same value already selected.
        !.
format_predicate4(X, X, Uv, [-(ID, X) | Uv], V) :-
        is_var(X),
        atom_chars(X, ['?' | Gt]), % get ID without flag
        atom_chars(X2, Gt),
        get_value_id(X2, ID, V),
        !.
format_predicate4(X, X, Uv, [-(ID, X) | Uv], V) :-
        get_value_id(X, ID, V),
        !.
format_predicate4(X, X, Uv, Uv, _) :- % variable without ID in V
        is_var(X),
        !.
format_predicate4(Xi, Xo, Uvi, Uvo, V) :-
        !, % non variable
        format_predicate2(Xi, Xo, Uvi, Uvo, V).

%! get_next_printable(+CHSin:compound, -CHSout:compound) is det
% Get the next CHS entry to print, skipping any non-printing literals (starting
% with an underscore).
%
% @param CHSin Input CHS.
% @param CHSout Output CHS.
get_next_printable([X | T], CHS) :-
        chs_entry(X, X2, _, _),
        strip_prefixes(X2, X3),
        (
                X3 = not(X4)
        ;
                X3 \= not(_),
                X4 = X3
        ),
        atom_chars(X4, ['_' | _]), % non-printing literal; skip it.
        !,
        get_next_printable(T, CHS).
get_next_printable([X | T], [X | T2]) :-
        !,
        get_next_printable(T, T2).
get_next_printable([], []) :-
        !.

%! divide_chs(+CHSin:compound, -CHSpos:list, -CHSneg:list) is det
% Split the CHS by negative and positive literals. Input should be sorted by
% literal. Each output will remain sorted by literal.
divide_chs([X | T], P, [X | N]) :-
        X = -(not(_), _), % negated literal
        !,
        divide_chs(T, P, N).
divide_chs([X | T], [X | P], N) :-
        X \= -(not(_), _), % positive literal
        !,
        divide_chs(T, P, N).
divide_chs([], [], []).

%! strip_prefixes(+FunctorIn:ground, -FunctorOut:ground) is det
% Strip any reserved prefixes added during processing. If appropriate, modify
% the output functor to restore formatting represented by the prefix. To ensure
% that prefixes added by the user remain intact, this predicate will return
% after removing a single copy of the dummy prefix, if encountered. Otherwise,
% all reserved prefixes will be stripped. To prevent errors, the dual rule
% prefix, if present, should always be first.
%
% @param FunctorIn Input functor
% @param FunctorOut Output functor
strip_prefixes(Fi, Fo) :-
        has_prefix(Fi, 'c'), % classical negation
        atom_chars(Fi, ['c', '_' | Fc]),
        atom_chars(F1, Fc),
        !,
        strip_prefixes(F1, F2),
        atom_chars(F2, Fc2),
        atom_chars(Fo, ['-' | Fc2]). % restore negation
strip_prefixes(Fi, not(Fo)) :-
        has_prefix(Fi, 'n'), % negation
        atom_chars(Fi, ['n', '_' | Fc]),
        atom_chars(F1, Fc),
        !,
        strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
        has_prefix(Fi, C),
        C \= 'd', % non-dummy prefix
        atom_chars(Fi, [C, '_' | Fc]),
        atom_chars(F1, Fc),
        !,
        strip_prefixes(F1, Fo).
strip_prefixes(Fi, Fo) :-
        has_prefix(Fi, 'd'), % dummy prefix, remove and finish
        atom_chars(Fi, ['d', '_' | Fc]),
        atom_chars(Fo, Fc),
        !.
strip_prefixes(F, F) :- % no prefixes
        !.

%! sort_chs(+CHSin:compound, -CHSout:compound) is det
% Sort entries in the formatted CHS by functor. If functors match, use =|@<|= to
% compare the entries. Basically a modified merge sort.
%
% @param CHSin The unsorted CHS. Entries of the form -(Predicate, Constraints),
%              where Predicate may be wrapped in a not().
% @param CHSout The sorted CHS.
sort_chs([], []).
sort_chs([X], [X]).
sort_chs(Ci, Co) :-
        length(Ci, L),
        L2 is L div 2,
        split_chs(Ci, Ca, Cb, L2),
        sort_chs(Ca, Ca2),
        sort_chs(Cb, Cb2),
        merge_chs(Ca2, Cb2, Co).

%! split_chs(+CHSi:list, -CHSl:list, -CHSr:list, +Count:int) is det
% Split the CHS (or any other list) into two lists: one with Count elements and
% one with the remainder of the list.
%
% @param CHSi The input CHS.
% @param CHSl The left CHS. Will contain Count elements.
% @param CHSr The right CHS. Will contain the remainder of the list.
% @param Count The number of elements to go in the left list.
split_chs([X | T], [X | T2], R, N) :-
        N > 0,
        !,
        N1 is N - 1,
        split_chs(T, T2, R, N1).
split_chs(R, [], R, 0) :-
        !.
split_chs([], [], [], _) :-
        !.

%! merge_chs(+CHSa:list, +CHSb:list, -CHSc:list) is det
% Merge sorted CHSes A and B into sorted CHS C.
%
% @param CHSa First sorted list.
% @param CHSb Second sorted list.
% @param CHSc Output sorted list.
merge_chs(X, [], X) :-
        !.
merge_chs([], X, X) :-
        !.
merge_chs([X | T], [Y | T2], [X | T3]) :-
        chs_lte(X, Y), % X =< Y
        !,
        merge_chs(T, [Y | T2], T3).
merge_chs(X, [Y | T2], [Y | T3]) :-
        !,
        merge_chs(X, T2, T3).

%! chs_lte(+A:compound, +B:compound) is det
% Compare two CHS entries. First, check functors, disregarding any not()
% wrappers. If functors match, compare the entries with =|@=<|=.
chs_lte(-(not(A), _), -(not(B), _)) :-
        !,
        chs_lte(-(A, _), -(B, _)).
chs_lte(-(not(A), _), -(B, _)) :-
        !,
        chs_lte(-(A, _), -(B, _)).
chs_lte(-(A, _), -(not(B), _)) :-
        !,
        chs_lte(-(A, _), -(B, _)).
chs_lte(-(A, _), -(B, _)) :-
        A \= not(_),
        B \= not(_),
        A =.. [Fa | _],
        B =.. [Fb | _],
        Fa @< Fb, % clear less than, else check args below
        !.
chs_lte(-(A, _), -(B, _)) :-
        A \= not(_),
        B \= not(_),
        A =.. [F | _],
        B =.. [F | _], % functors match
        A @=< B,
        !.

%! fill_in_variable_values(+GoalIn:compound, -GoalOut:compound, +ConstraintsIn:list, -ConstraintsOut:list, +Vars:compound) is det
% Given a goal, replace any bound variables with their values. If a goal or
% value is a compound term, process each arg. For constrained variables, store
% the variable/constraints pair in Constraints. NOTE: Must never return
% constraint entries with empty constraint lists!
%
% @param GoalIn Input goal.
% @param GoalOut Output goal.
% @param ConstraintsIn Input constraints.
% @param ConstraintsOut Output constraints.
% @param VarsOut Output vars.
fill_in_variable_values(G, G, C, C, V) :-
        is_unbound(G, V, [], _, 0), % unbound variable, no constraints
        !.
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
        is_unbound(Gi, V, [], _, 1), % unbound variable, no constraints, flagged
        !,
        atom_chars(Gi, Gc),
        (Gc = ['?' | Gt] -> % don't duplicate flag
                Go = Gi,
                atom_chars(G, Gt),
                ((is_unbound(G, V, Con, _, 1), Con \= []) -> % flag added in last pass, get correct constraints
                        Co = [-(Go, Con) | Ci]
                ;
                        Co = Ci
                )
        ;
                atom_chars(Go, ['?' | Gc]), % add flag
                Co = Ci
        ).
fill_in_variable_values(G, G, C, [-(G, Con) | C], V) :-
        is_unbound(G, V, Con, _, 0), % unbound variable, constraints
        !.
fill_in_variable_values(Gi, Go, C, [-(Go, Con) | C], V) :-
        is_unbound(Gi, V, Con, _, 1), % unbound variable, constraints, flagged
        !,
        atom_chars(Gi, Gc),
        (Gc = ['?' | _] -> % don't duplicate flag
                Go = Gi
        ;
                atom_chars(Go, ['?' | Gc]) % add flag
        ).
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
        var_value(Gi, V, val(G2)),
        G2 =.. [F | A], % compound term, check args
        !,
        fill_in_variable_values2(A, A2, Ci, Co, V),
        Go =.. [F | A2]. % repack term
fill_in_variable_values(Gi, Go, C, C, V) :-
        var_value(Gi, V, val(Go)),
        atom(Go), % atom
        !.
fill_in_variable_values(Gi, Go, Ci, Co, V) :-
        Gi =.. [F | A], % compound term, check args
        !,
        fill_in_variable_values2(A, A2, Ci, Co, V),
        Go =.. [F | A2]. % repack term
fill_in_variable_values(G, G, C, C, _) :-
        \+is_var(G), % other non-var
        !.

%! fill_in_variable_values2(+ArgsIn:list, -ArgsOut:list, +ConstraintsIn:list, -ConstraintsOut:list, +Vars:compound) is det
% Given a list of args, call fill_in_variable_values/5 for each.
%
% @param ArgsIn Input args.
% @param ArgsOut Output args.
% @param ConstraintsIn Input constraints.
% @param ConstraintsOut Output constraints.
% @param VarsOut Output vars.
fill_in_variable_values2([X | T], [X2 | T2], Ci, Co, V) :-
        fill_in_variable_values(X, X2, Ci, C1, V),
        !,
        fill_in_variable_values2(T, T2, C1, Co, V).
fill_in_variable_values2([], [], C, C, _).

%! print_vars(+PrintVars:list, +Vars:compound) is det
% Given a list of variables, group them by value and print them. Don't print
% completely unbound variables that aren't unified with another variable in the
% list.
%
% @param PrintVars The variables to print.
% @param Vars Var struct to get values from.
print_vars(X, V) :-
        X \= [],
        !,
        get_var_vals(X, V, X2),
        group_by_val(X2, X3),
        remove_nonprinting(X3, X4),
        format_vars(X4, X5, V),
        print_vars2(X5).
print_vars([], _) :-
        !.

%! print_vars2(+VarGroups:list) is det
% Given a list of lists of variables with the same value, print each.
%
% @param VarGroups The groups of variables to print.
print_vars2([-(X, V) | T]) :-
        write('\n'), % First set doesn't get a comma before newline.
        print_vars4(X, V),
        !,
        print_vars3(T).
print_vars2([]) :-
        !.

%! print_vars3(+VarGroups:list) is det
% Given a list of lists of variables with the same value, print each.
%
% @param VarGroups The groups of variables to print.
print_vars3([-(X, V) | T]) :-
        write(',\n'), % Sets after the first get a comma before the newline.
        print_vars4(X, V),
        !,
        print_vars3(T).
print_vars3([]) :-
        !.

%! print_vars4(+VarGroup:list, +Value:compound) is det
% Given a list of variables with the same value, print them. List must be
% non-empty.
%
% @param VarGroup The group of variables to print.
% @param Value The value of the variables.
print_vars4([X, Y | T], V) :- % at least two elements
        writef('%w = %w', [X, Y]), % print first element here
        !,
        print_vars5([Y | T], V). % print remaining elements
print_vars4([X], V) :- % single-element list; constrained variable
        var_con(V, C, _, _),
        !,
        print_var_constraints([-(X, C)]).
print_vars4([X], V) :- % single-element list; bound variable
        V = val(V2),
        !,
        writef('%w = %w', [X, V2]).

%! print_vars5(+VarGroup:list, +Value:compound) is det
% Given a list of variables with the same value, print them. List must be
% non-empty.
%
% @param VarGroup The group of variables to print.
% @param Value The value of the variables.
print_vars5([X, Y | T], V) :- % at least two elements
        writef(', %w = %w', [X, Y]),
        !,
        print_vars5([Y | T], V).
print_vars5([X], V) :- % last element; constrained variable
        var_con(V, C, _, _),
        C \= [],
        !,
        write(', '),
        print_var_constraints([-(X, C)]).
print_vars5([X], V) :- % last element; constrained variable
        var_con(V, [], _, _), % no constraints to print
        !.
print_vars5([X], V) :- % last element; bound variable
        V = val(V2),
        !,
        writef(', %w = %w', [X, V2]).

%! get_var_vals(+PrintVars:list, +Vars:compound, -PrintVarsOut:list) is det
% Get the value for each variable in a list, returning a list of
% -(Value, Var) pairs.
%
% @param PrintVars The variables to print.
% @param Vars Var struct to get values from.
% @param PrintVarsOut Output list of -(Value, Var) pairs.
get_var_vals([X | T], V, [-(Val, X) | T2]) :-
        is_var(X),
        var_value(X, V, Val),
        Val \= id(_), % ensure that we don't get IDs
        !,
        get_var_vals(T, V, T2).
get_var_vals([], _, []) :-
        !.

%! group_by_val(+VarsIn:list, -VarsOut:list) is det
% Given a list of -(Val, Var) pairs, return a list of lists of variables with
% the same value. VarsOut will be sorted and each sub-list will be sorted.
%
% @param VarsIn Input list.
% @param VarsOut Output list of lists.
group_by_val(Xi, Xo) :-
        once(sort(Xi, X2)),
        X2 = [-(V, Y) | X3],
        !,
        group_by_val2(X3, V, [Y], Xo).
group_by_val([], []) :-
        !.

%! group_by_val2(+VarsIn:list, +LastVal:int, +CurrSet:list, -VarsOut:list) is det
% Given a list of -(Val, Var) pairs, return a list of pairs of the form
% -(Vars, Val) where vars is a list of variables with the same value. Should
% only be called from group_by_val/2. VarsIn must be sorted. Each list of
% variables will be sorted, but not the outer list.
%
% @param VarsIn Input list.
% @param LastVal The last value encountered.
% @param CurrSet The current set of variables with LastVal as their value.
% @param VarsOut Output list of lists.
group_by_val2([X | T], V, Vs, Vo) :-
        X = -(V, Y), % same value as previous entry
        !,
        group_by_val2(T, V, [Y | Vs], Vo).
group_by_val2([X | T], V, Vs, [-(Vs2, V) | Vo]) :-
        X = -(V2, Y),
        V2 \= V, % different value.
        !,
        reverse(Vs, Vs2), % restore sort order of vars
        group_by_val2(T, V2, [Y], Vo).
group_by_val2([], V, Vs, [-(Vs2, V)]) :- % return final set
        reverse(Vs, Vs2), % restore sort order of vars
        !.

%! remove_nonprinting(+VarGroupsIn:list, -VarGroupsOut:list) is det
% Given a list of lists of variables with the same value, remove lists
% containing only one completely unbound variable.
%
% @param VarGroupsIn Input var groups.
% @param VarGroupsOut Output var groups.
remove_nonprinting([X | T], T2) :-
        X = -([_], V),
        var_con(V, [], _, 0), % skip single-element lists where variable is completely unbound
        !,
        remove_nonprinting(T, T2).
remove_nonprinting([X | T], [X | T2]) :-
        !,
        remove_nonprinting(T, T2).
remove_nonprinting([], []) :-
        !.

%! format_vars(+VarGroupsIn:list, -VarGroupsOut:list, +Vars:compound) is det
% Call chs:format_term/4 on the value for each variable group.
%
% @param VarGroupsIn Input var groups.
% @param VarGroupsOut Output var groups.
% @param Vars Var struct to get values from.
format_vars([X | T], [X2 | T2], V) :-
        X = -(Vs, val(Val)),
        format_term(Val, Val2, _, V),
        X2 = -(Vs, val(Val2)),
        !,
        format_vars(T, T2, V).
format_vars([X | T], [X2 | T2], V) :-
        X = -(Vs, Val),
        var_con(Val, Con, F, L),
        format_term(Con, Con2, _, V),
        var_con(Val2, Con2, F, L),
        (L =:= 1 ->
                atom_chars(Vs, Vc),
                atom_chars(Vs2, ['?' | Vc])%, % add flag
                % writef('loop var con list = %w, processed = %w\n', [Con, Con2])
        ;
                Vs2 = Vs
        ),
        X2 = -(Vs2, Val2),
        !,
        format_vars(T, T2, V).
format_vars([], [], _) :-
        !.

%! print_var_constraints(+Constraints:list) is det
% Given a list of pairs of variables and constraints, format and print them.
% This should only be called with constraints returned by
% variables:fill_in_variable_values/5.
%
% @param Constraints The list of variable/constraint pairs.
print_var_constraints([]) :-
        !.
print_var_constraints([X | T]) :-
        X = -(V, Cs),
        sort(Cs, [C | Cs2]), % order and remove duplicate constraints
        writef('%w \\= %w', [V, C]), % write first entry here for proper comma placement
        print_var_constraints3(V, Cs2),
        print_var_constraints2(T),
        !.

%! print_var_constraints2(+Constraints:list) is det
% Given a list of constraints on variables in a CHS entry, format and print
% them. Each will be a list of values the constraint can't take, with an empty
% list indicating a completely unbound variable.
%
% @param Constraints The list of variable/constraint pairs.
print_var_constraints2([X | T]) :-
        X = -(V, Cs),
        print_var_constraints3(V, Cs),
        !,
        print_var_constraints2(T).
print_var_constraints2([]) :-
        !.

%! print_var_constraints3(+Var:ground, +Constraints:list) is det
% Given variable and a list of constraints on it, pretty print each constraint.
%
% @param Var The variable name.
% @param Constraints The list of constraints.
print_var_constraints3(V, [C | T]) :-
        writef(', %w \\= %w', [V, C]),
        !,
        print_var_constraints3(V, T).
print_var_constraints3(_, []) :-
        !.
