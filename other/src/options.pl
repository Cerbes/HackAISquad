:- module(options, [
                        user_option/1,
                        user_option/2,
                        set_default_options/0,
                        set_user_option/1,
                        set_user_option/2,
                        option_cleanup/0,
                        set_stack_sizes/0
                     ]).

/** <module> Assertion, retraction and testing of options.

Predicates related to managing options that alter runtime behavior.


@author Kyle Marple
@version 20150803
@license GPL
@see Default settings are specified in config.pl, not here.
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

:- use_module(config). % for default options

%! user_option(+Mode:atom) is det
% Asserted for options that are specified by the user via command-line
% arguments. Where applicable, these can override settings hard-coded into a
% program, such as the number of answer sets to compute. If not specified by the
% user, the hard-coded value would normally override the default.
%
% @param Mode The option specified by the user.

%! user_option(+Mode:atom, +Value:compound) is det
% Asserted for applicable options by set_user_option/2. Associates each option with
% a binary flag indicating whether or not the option is enabled. Only one fact
% should be asserted per Mode (ensured by set_user_option/2).
%
% @param Mode An atom identifying the option.
% @param Value The value for the option.
:- dynamic
        user_option/1,
        user_option/2.

%! set_default_options
% Get a list of all config:default_option/2 facts and set the options
% accordingly.
set_default_options :-
        findall(-(X, Y), default_option(X, Y), Defaults),
        set_default_options2(Defaults).

%! set_default_options2(+Options:list)
% For each option / flag pair, call set_user_option/2 after checking that no
% other entry for the same option exists.
%
% @param Options List of options to set.
set_default_options2([X | T]) :-
        X = -(A, B),
        \+user_option(A, _), % not overridden by user
        set_user_option(A, B),
        !,
        set_default_options2(T).
set_default_options2([X | T]) :-
        X = -(A, _),
        user_option(A, _), % overridden by user
        !,
        set_default_options2(T).
set_default_options2([]).

%! set_user_option(+Mode:atom) is det
% Assert user_option(Mode).
%
% @param Mode The option specified by the user.
set_user_option(Mode) :-
        assertz(user_option(Mode)).

%! set_user_option(+Mode:atom, +Value:compound)
% Assert an option with a given value.
%
% @param Mode The option to assert.
% @param Value The value for the option.
set_user_option(M, X) :-
        retractall(user_option(M, _)),
        assertz(user_option(M, X)),
        !.

%! option_cleanup
% Cleanup (retract) all user_option/1 and user_option/2 assertions.
option_cleanup :-
        retractall(user_option(_, _)),
        retractall(user_option(_)),
        !.

%! set_stack_sizes
% Increase the default stack sizes to the value indicated by
% config:stack_size/1. For 32-bit systems SWI should ignore this command. For
% 64-bit systems, a limit larger than the system allows will be automatically
% reduced to the maximum allowed size.
set_stack_sizes :-
        stack_size(X),
        set_prolog_stack(global, limit(X)),
        set_prolog_stack(local, limit(X)),
        set_prolog_stack(trail, limit(X)),
        !.
