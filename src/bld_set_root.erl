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

-module(bld_set_root).

-include_lib("builderl/include/builderl.hrl").

-export([do/1]).

usage() ->
    [
     "************************************************************************",
     "Updates the ROOTDIR variable in a release created with reltool.",
     "Please only run in the release folder as it updates files in-place!",
     "",
     "Usage:",
     "  update_root_dir.esh [ -v | -h | --help ]",
     "",
     "  -h, --help",
     "    This help.",
     "************************************************************************"
    ].

-define(RELCHECK,
        [{dir, <<"deps">>},
         {dir, <<"deps-versions">>},
         {file, <<"GNUmakefile">>}
        ]).

do(["-h"]) ->     bld_lib:print(usage());
do(["--help"]) -> bld_lib:print(usage());
do([]) ->         do1().

do1() ->
    check_release(),

    {ok, Root} = file:get_cwd(),
    io:format(standard_io, "Using '~s' as node root.~n", [Root]),

    %% Should be just one of each type, correct if it has changed
    [ErlFile] = filelib:wildcard("erts-*/bin/erl"),
    [StartFile] = filelib:wildcard("erts-*/bin/start"),
    ToUpdate = ["bin/start", ErlFile, StartFile],

    {ok, RegExp} = re:compile("\s*ROOTDIR=.+$", [multiline]),
    ToLine = "ROOTDIR=\"" ++ Root ++ "\"",
    Msg = "~nUpdating in-place!~n~p~nIn files: ~p~n~n",
    io:format(standard_io, Msg, [ToLine, ToUpdate]),
    lists:foreach(fun(F) -> update_in_place(F, RegExp, ToLine) end, ToUpdate).

check_release() ->
    Fun = fun({dir, Dir}) -> filelib:is_dir(Dir);
             ({file, File}) -> filelib:is_regular(File)
          end,

    case lists:all(Fun, ?RELCHECK) of
        true -> bld_lib:print(err1()), halt(1);
        false -> ok
    end.

err1() ->
    [
     "This script updates files in-place and can only be executed in the release folder!.",
     "Use -h or --help for more options."
    ].

update_in_place(File, RegExp, Value) ->
    Data = re:replace(bld_lib:read_data(File), RegExp, Value),
    io:format(standard_io, "Write file '~s': ", [File]),
    case file:write_file(File, Data) of
        ok ->
            io:format(standard_io, "Done.~n", []);
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end.
