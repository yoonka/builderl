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
         consult_app_file/1,
         print/1,
         h_line/1,
         chmod_exe/1,
         chmod/2,
         mk_link/2,
         mk_dir/1,
         intersection/2,
         process_file/3,
         process_file/4,
         read_data/1,
         cp_file/4,
         cp_dir/4
        ]).

%% Attributes to set on an executable script
-define(EXEMODE, 8#00744).

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

consult_app_file(Src) ->
    case file:consult(Src) of
        {ok, AppSrc} -> AppSrc;
        Err -> error_reading_file(Src, Err)
    end.

error_reading_file(Src, Err) ->
    io:format("Error when reading file '~p': '~p'. Aborting.~n", [Src, Err]),
    halt(1).

%%------------------------------------------------------------------------------

print(Lines) -> [io:format(standard_error, X ++ "~n", []) || X <- Lines].

h_line(Msg) ->
    h_line(Msg, Msg, 80).

h_line([$~, $n, T], Org, Line) ->
    h_line(T, Org, Line + 2);
h_line(Msg, Org, Line) ->
    HLine = Msg ++ lists:duplicate(Line - length(Org), $-) ++ "~n",
    io:format(standard_io, HLine, []).

chmod_exe(File) ->
    io:format(standard_io, "Change mode of '~s' to ~.8B: ", [File, ?EXEMODE]),
    bld_lib:check_file_op(file:change_mode(File, ?EXEMODE)).

chmod(File, Mode) ->
    io:format(standard_io, "Change mode of '~s' to ~.8B: ", [File, Mode]),
    bld_lib:check_file_op(file:change_mode(File, Mode)).

mk_link(To, From) ->
    io:format(standard_io, "Create link to '~s' from '~s': ", [To, From]),
    check_file_op(file:make_symlink(To, From)).

mk_dir(Name) ->
    io:format(standard_io, "Create folder '~s': ", [Name]),
    check_file_op(file:make_dir(Name)).

%%------------------------------------------------------------------------------

intersection(L1, L2) ->
    lists:filter(fun(X) -> lists:member(X, L1) end, L2).

%%------------------------------------------------------------------------------

process_file([H|_T] = Src, Dest, Args) when is_list(H) ->
    process_file(filename:join(Src), Dest, Args);
process_file(Src, [H|_T] = Dest, Args) when is_list(H) ->
    process_file(Src, filename:join(Dest), Args);
process_file(Src, Dest, Args) ->
    case lists:keytake(options, 1, Args) of
        {value, {options, Opts}, Args1} -> process_file(Src, Dest, Args1, Opts);
        false -> process_file(Src, Dest, Args, [])
    end.

process_file(Src, Dest, Args, Options) ->
    Data = read_data(Src),
    io:format(standard_io, "Write file '~s': ", [Dest]),
    case not lists:member(force, Options) andalso filelib:is_file(Dest) of
        false ->
            process_file1(Data, Dest, Args);
        true ->
            io:format(standard_error, "Error, file already exists!~n", []),
            halt(1)
    end.

read_data(Src) ->
    case file:read_file(Src) of
        {ok, Data} ->
            Data;
        {error, Err} ->
            Msg = "Can not read file: '~s', Error: ~p~n",
            io:format(standard_error, Msg, [Src, Err]),
            halt(1)
    end.

process_file1(Data, Dest, []) ->
    case file:write_file(Dest, Data) of
        ok ->
            io:format(standard_io, "Done.~n", []);
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end;
process_file1(Data, Dest, Args) ->
    NewData = lists:foldl(fun(X, D) -> replace(X, D) end, Data, Args),
    process_file1(NewData, Dest, []).

replace({Name, Value}, Src) -> re:replace(Src, Name, Value);
replace({Name, Value, Opts}, Src) -> re:replace(Src, Name, Value, Opts).

%%------------------------------------------------------------------------------

cp_file(Src, Dest, File, Args) ->
    SrcFile = filename:join(Src, File),
    DestFile = filename:join(Dest, File),
    process_file(SrcFile, DestFile, Args).

cp_dir(Src, Dest, Templates, CfgArgs) ->
    Msg = "Processing files from '~s'~n  to '~s'...~n",
    io:format(standard_io, Msg, [Src, Dest]),

    {Dirs0, Files, Tmpls} = relative_paths(Src, Templates),
    Dirs = lists:sort(Dirs0),
    lists:foreach(fun(X) -> mk_dir(filename:join(Dest, X)) end, Dirs),
    lists:foreach(fun(X) -> cp_file(Src, Dest, X, []) end, Files),
    lists:foreach(fun(X) -> cp_file(Src, Dest, X, CfgArgs) end, Tmpls),

    io:format(standard_io, "Done processing files from '~s'.~n", [Src]).

relative_paths(Src, Templates) ->
    Paths = bld_compat:wildcard(filename:join(Src, "**")),
    {Files, Dirs} = lists:partition(fun(X) -> filelib:is_regular(X) end, Paths),
    TmplFun = fun(X) -> lists:member(filename:basename(X), Templates) end,
    {Tmpls, Regular} = lists:partition(TmplFun, Files),

    Root = filename:split(Src),
    {subtract_root(Root, Dirs),
     subtract_root(Root, Regular),
     subtract_root(Root, Tmpls)}.

subtract_root(SrcElems, Paths) ->
    [filename:join(filename:split(X) -- SrcElems) || X <- Paths].

%%------------------------------------------------------------------------------
