%% Copyright (c) 2015, Grzegorz Junka
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%% * Redistributions of source code must retain the above copyright notice,
%%   this list of conditions and the following disclaimer.
%% * Redistributions in binary form must reproduce the above copyright notice,
%%   this list of conditions and the following disclaimer in the documentation
%%   and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
%% EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-module(bld_lib).

-include_lib("builderl/include/builderl.hrl").

-export([
         get_reltool_config/0,
         check_file_op/1,
         mk_link/2,
         consult_app_file/1,
         error_reading_file/2
        ]).

%%------------------------------------------------------------------------------

get_reltool_config() ->
    case file:consult(?RELTOOL_CONFIG) of
        {ok, File} ->
            File;
        {error, Error} ->
            Msg = "Can't read file '~s':~nError: ~p~n",
            io:format(standard_error, Msg, [?RELTOOL_CONFIG, Error]),
            halt(1)
    end.

check_file_op(ok) ->
    io:format(standard_io, "Done.~n", []);
check_file_op({error, Err}) ->
    io:format(standard_io, "Error:~n~1000p~n", [Err]),
    halt(1).

%%------------------------------------------------------------------------------

mk_link(To, From) ->
    io:format(standard_io, "Create link to '~s' from '~s': ", [To, From]),
    check_file_op(file:make_symlink(To, From)).

%%------------------------------------------------------------------------------

consult_app_file(Src) ->
    case file:consult(Src) of
        {ok, AppSrc} -> AppSrc;
        Err -> error_reading_file(Src, Err)
    end.

%%------------------------------------------------------------------------------

error_reading_file(Src, Err) ->
    io:format("Error when reading file '~p': '~p'. Aborting.~n", [Src, Err]),
    halt(1).
