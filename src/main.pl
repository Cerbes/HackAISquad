:- module(main, [
                        main/0,
                        main/1,
                        debugmain/1
                     ]).

/** <module> s(ASP) Ungrounded Stable Models Solver

Read in a normal logic program. Compute dual rules and the NMR check. Execute
the modified program according to the stable model semantics and output the
results.

@author Kyle Marple
@version 20150803
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
:- use_module(common).
:- use_module(comp_duals).
%:- use_module(debug).
:- use_module(interactive). % for help output
:- use_module(io).
:- use_module(nmr_check).
:- use_module(options).
:- use_module(program). % for destroy_program/0
:- use_module(solve).

%! main
% Used by compiled executable. Not for interactive runs.
main :-
        %time(submain),
        submain,
        halt.
main :-
        halt(1).

%! submain
% Original contents of the first clause of main/0. Split to allow the call to
% submain to be wrapped in main/0, ex. with time/1 or profile/1.
submain :-
        once(parse_args(Sources)),
        main2(Sources).

%! main(+Source:filepath)
% Wrapper for interactive runs.
%
% @param Source Path of input file, or list of paths for multiple files.
main(Sources) :-
        Sources = [_ | _],
        !, % list of input files
        main2(Sources).
main(Source) :-
        !, % single input file
        main2([Source]).

%! debugmain(+Args:list)
% Simulate calling from command line by passing the command line args as a list
% of strings. For debugging using the Prolog console.
% Ex. debugmain(['-vv','-nf','file.asp']).
%
% @param Args The list of commandline arguments, including input files.
debugmain(Args) :-
        once(parse_args2(Args, Sources)),
        main2(Sources).

%! main2(+Sources:list)
% Output of each call should be deterministic, so backtracking is not necessary
% at this level.
%
% @param Sources A list of paths of input files.
main2(_) :-
        user_option(help, 1),
        !,
        help.
main2([]) :- % require an input file
        write(user_error, 'ERROR: No input file specified!\n\n'),
        help,
        !.
main2(Sources) :-
        once(set_stack_sizes),
        once(set_default_options),
        once(load_source_files(Sources)),
        once(comp_duals),
        once(generate_nmr_check),
        write_verbose(0, 'Preparation of input program complete.\n'),
        user_option(mode, Mode),
        once(solve(Mode)),
        destroy_program,
        option_cleanup.

%! parse_args(-Sources:list)
% Handle command-line arguments. Strip first entry.
%
% @param Sources Paths of input files.
parse_args(Sources) :-
        current_prolog_flag(argv, Args),
        Args = [_ | Args2],
        parse_args2(Args2, Sources).
parse_args(_) :-
        write_error('invalid command-line arguments'),
        help,
        !,
        fail.

%! parse_args2(+Args:list, -Sources:list)
% Checks individual arguments given from command-line. Call parse_args/1
% instead of this.
%
% @param Args List of command-line arguments.
% @param Sources Paths of input files.
parse_args2([X | T], S) :-
        member(X, ['-v', '--verbose']),
        !,
        set_user_option(verbose, 1),
        parse_args2(T, S).
parse_args2([X | T], S) :-
        member(X, ['-vv', '--veryverbose']),
        !,
        set_user_option(veryverbose, 1),
        parse_args2(T, S).
parse_args2([X | T], S) :-
        member(X, ['-a', '--auto']),
        !,
        set_user_option(mode, auto),
        parse_args2(T, S).
parse_args2([X | T], S) :-
        member(X, ['-i', '--interactive']),
        !,
        set_user_option(mode, user),
        parse_args2(T, S).
parse_args2([X | T], S) :-
        atom_chars(X, ['-', 's' | Nc]),
        Nc \= [],
        number_chars(N, Nc),
        N >= 0,
        !,
        set_user_option(ascount, N),
        set_user_option(ascount), % allow user-specified value to override hard-coded ones
        parse_args2(T, S).
parse_args2([X | T], S) :-
        X = '-s',
        !,
        set_user_option(ascount, 0), % find all
        set_user_option(ascount), % allow user-specified value to override hard-coded ones
        parse_args2(T, S).
parse_args2([X | T], S) :- % intentionally undocumented option to enable debugging
        atom_chars(X, ['-', 'd' | Nc]),
        Nc \= [],
        number_chars(N, Nc),
        N >= 0,
        !,
        set_user_option(debug, N),
        parse_args2(T, S).
parse_args2([X | T], S) :-
        X = '-d',
        !,
        set_user_option(debug, 0), % lowest level that actually adds debugging output
        parse_args2(T, S).
parse_args2([X | T], S) :-
        member(X, ['-?', '-h', '--help']),
        !,
        set_user_option(help, 1),
        parse_args2(T, S).
parse_args2([X | T], [X | S]) :-
        \+ atom_concat('-', _, X), % if it isn't a flag, assume source file.
        !,
        parse_args2(T, S).
parse_args2([], []).
