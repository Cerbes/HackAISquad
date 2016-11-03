:- module(interactive, [
                        help/0,
                        get_user_query/1,
                        get_user_response/0
                       ]).

/** <module> User interaction

Predicates related to interaction with the user, particularly when running in
interactive mode.

@author Kyle Marple
@version 20150805
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
:- use_module(library(writef)).
:- use_module(io).
:- use_module(text_dcg).
:- use_module(tokenizer).

%! help
% Print usage information.
help :-
        current_prolog_flag(argv, [Prog | _]),
        swritef(M1, 'Usage: %w [options] InputFile(s)\n\n', [Prog]),
        write(user_error, M1),
        swritef(M2, 's(ASP) computes stable models of ungrounded normal logic programs.\n', []),
        write(user_error, M2),
        % for reference on line length (keep under 80 chars/line incl. newline):
        %                 '1                                                                          80->x'
        write(user_error, 'Command-line switches are case-sensitive!\n\n'),
        write(user_error, ' General Options:\n\n'),
        write(user_error, '  -h, -?, --help     Print this help message and terminate.\n'),
        write(user_error, '  -i, --interactive  Run in user / interactive mode.\n'),
        write(user_error, '  -a, --auto         Run in automatic mode (no user interaction).\n'),
        write(user_error, '  -sN                Compute N answer sets, where N >= 0. 0 for all.\n'),
        write(user_error, '                     Ignored unless running in automatic mode.\n'),
        write(user_error, '  -v, --verbose      Enable verbose progress messages.\n'),
        write(user_error, '  -vv, --veryverbose Enable very verbose progress messages.\n').

%! get_user_query(-Query:list) is det
% Write a prompt, then get a query from the user, tokenize it, parse it and
% return the list of goals. If an invalid query is entered, prompt the user for
% another one.
%
% @param Query The list of goals entered by the user.
get_user_query(Q) :-
        write('?- '), % write initial prompt
        read_query(user_input, CharPairs),
        (tokenize(CharPairs, Toks) ->
                (parse_query(Toks, Q) ->
                        true % we're done
                ;
                        get_user_query(Q) % prompt user for another try
                )
        ;
                get_user_query(Q) % prompt user for another try
        ),
        !.

%! get_user_response is det
% A query has succeeded, so get input from the user to accept ('.') or reject
% (';') it.
get_user_response :-
        get_single_char(C),
        char_code(C2, C),
        writef('%w\n', [C2]),
        !,
        get_user_response2(C2).

%! get_user_response2(+Char:char) is det
% Process the user's response. For invalid responses, print an error and give
% them another chance.
%
% @param Char The one character response.
get_user_response2(C) :- % accept
        member(C, ['.', 'c', 'a']),
        !.
get_user_response2(C) :- % accept
        char_type(C, end_of_line),
        !.
get_user_response2(C) :- % redo
        member(C, [';', 'n', 'r', ' ', '\t']),
        !,
        fail.
get_user_response2(C) :- % help
        member(C, ['?', 'h']),
        !,
        write(';, n, r, space, tab:\treject\n'),
        write('., c, a, enter:\t\taccept\n'),
        write('?, h:\t\t\thelp\n'),
        write('Action? '),
        !,
        get_user_response.
get_user_response2(C) :-
        writef('Unknown action: %w (h for help)\n', [C]),
        write('Action? '),
        !,
        get_user_response.

