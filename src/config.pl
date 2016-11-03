:- module(config,
        [
                default_option/2,
                stack_size/1
        ]).

/** <module> Default options for compiler.

Facts for default options. These can be overridden by command-line switches.
To change the default behavior of the compiler, change the default_option/2
facts below. Additionally, the stack size is defined here, and cannot be changed
via command-line switches.

@author Kyle Marple
@version 20150827
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

%! default_option(+Option:atom, +Value:compound) is det
% Declarations for default options. Used by main:set_default_options/0. Only one
% fact should be declared for an Option value.
%
% @param Option An identifier associated with the option.
% @param Value The default value for the option.

% Execution mode: auto (automatic) or user (interactive).
%default_option(mode, user).
default_option(mode, auto).

% Number of answer sets to compute in automatic mode. 0 for all.
default_option(ascount, 1).

% Debugging level. Levels >= 0 are not intended for use by end-users. Change at
% your own risk.
default_option(debug, -1).

%! stack_size(+Size:atom)
% Override for default SWI stack sizes. Increasing this value allows for
% compilation of larger programs, which may otherwise yield stack errors. All
% three stacks are set to this value: global, local and trail.
%
% This will only work with 64-bit SWI. This option will be ignored on 32-bit
% systems, as the limit is already set to the maximum, 128MB. Additionally, if
% the limit is set higher than the system supports, it will be silently reduced
% to the maximum allowed size.
%
% @param Size The maximum stack size to use. Values is an integer number of
%        kilobytes. For large values, use powers of 10, ex. X*10**9 for X
%        gigabytes.
stack_size(2*10**9).
