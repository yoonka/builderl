%% Copyright (c) 2015-2016, Grzegorz Junka
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

-module(bld_run).

-include_lib("builderl/include/builderl.hrl").

-export([start/1, stop/1]).

start_usage(Allowed) ->
    [
     "************************************************************************",
     "Starts the specified nodes.",
     "",
     "Usage:",
     "  start.esh [ -v | -h | --help ]",
     "  start.esh [ <type> | <type>-<suffix> | --RELEASES ] ... ]",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  --RELEASES",
     "    Starts the node only to create the 'releases/RELEASES' file",
     "    and then the node is stopped.",
     "",
     "  <type>, <type>-<suffix>",
     "    If only the type is specified then it starts all nodes of the given",
     "    type. If the suffix is specified as well then it starts only the",
     "    specified node type with the supplied suffix. If no type",
     "    is specified then it starts all the known node types.",
     "    Accepted node types: " ++ string:join(Allowed, ", ") ++ ".",
     "************************************************************************"
    ].

stop_usage(Allowed) ->
    [
     "************************************************************************",
     "Stops the specified nodes.",
     "",
     "Usage:",
     "  stop.esh [ -v | -h | --help ]",
     "  stop.esh [ <type> | <type>-<suffix> ] ...",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  <type>, <type>-<suffix>",
     "    If only the type is specified then it stops all nodes of the given",
     "    type. If the suffix is specified as well then it stops only the",
     "    specified node type with the supplied suffix. If no type",
     "    is specified then it stops the known node types that have been",
     "    found to be running.",
     "    Accepted node types: " ++ string:join(Allowed, ", ") ++ ".",
     "************************************************************************"
    ].

ensure_nodes({_, _, BldCfg} = Params, Options, Mode) ->
    case lists:keymember(node, 1, Options) of
        true ->
            Options;
        false ->
            Fun = fun(N, Acc) -> bld_lib:add_node(N, Mode, Params, Acc) end,
            lists:foldl(Fun, Options, bld_lib:get_default_nodes(BldCfg))
    end.

print_usage(LinesFun, BldCfg) ->
    bld_lib:print(LinesFun(bld_lib:get_allowed(BldCfg))).

%%------------------------------------------------------------------------------

start(Args) ->
    BldConf = bld_lib:read_builderl_config(),
    start1(Args, BldConf).

start1(["-h"], BldCfg) ->     print_usage(fun start_usage/1, BldCfg);
start1(["--help"], BldCfg) -> print_usage(fun start_usage/1, BldCfg);
start1(Other, BldCfg) ->      start2(Other, bld_lib:get_params(BldCfg), []).

start2(["--RELEASES"|T], P, Acc) ->
    start2(T, P, bld_lib:ensure_member(releases, Acc));
start2([Node|T], P, Acc) ->
    start2(T, P, bld_lib:add_node(Node, start, P, Acc));
start2([], P, Acc) ->
    do_start(P, ensure_nodes(P, lists:reverse(Acc), start));
start2(Other, _P, _Acc) ->
    bld_lib:halt_badarg(Other).

do_start({_, _, BldCfg}, OrgOpts) ->
    Options = [{bld_lib:node_name(T, S, BldCfg), Dir}
               || {node, start, T, S, Dir} <- OrgOpts],
    io:format("Using options: ~p~n", [Options]),
    io:format(standard_io, "~n => Starting nodes...~n~n", []),
    Ext = get_extension(OrgOpts),
    lists:foreach(fun(Node) -> bld_lib:start_node(Node, Ext) end, Options),
    io:format("Finished.~n").

get_extension(OrgOpts) ->
    case lists:member(releases, OrgOpts) of
        true -> ?RELEASES_EXT;
        false -> ".sh"
    end.

%%------------------------------------------------------------------------------

stop(Args) ->
    BldConf = bld_lib:read_builderl_config(),
    stop1(Args, BldConf).

stop1(["-h"], BldCfg) ->     print_usage(fun stop_usage/1, BldCfg);
stop1(["--help"], BldCfg) -> print_usage(fun stop_usage/1, BldCfg);
stop1(Other, BldCfg) ->      stop2(Other, bld_lib:get_params(BldCfg), []).

stop2([Node | T], P, Acc) -> stop2(T, P, bld_lib:add_node(Node, stop, P, Acc));
stop2([], P, Acc) -> do_stop(P, ensure_nodes(P, lists:reverse(Acc), stop));
stop2(Other, _P, _Acc) -> bld_lib:halt_badarg(Other).

do_stop({_, _, BldCfg}, OldOpts) ->
    Options = [bld_lib:node_name(T, S, BldCfg)
               || {node, stop, T, S, _} <- OldOpts],
    io:format("Using options: ~p~n", [Options]),
    Running = bld_lib:running_nodes(),
    io:format("Found running nodes: ~p~n", [Running]),
    lists:foreach(fun(N) -> stop_one(N, Options, BldCfg) end, Running),
    io:format("Finished.~n").

stop_one(Name, Opts, BldCfg) ->
    {Node, Suffix} = extract_node_relaxed(Name),
    case {lists:member(Name, Opts), lists:member(Node, Opts)} of
        {false, false} -> ok;
        _ -> stop_node(Name, bld_lib:node_dir(Node, Suffix, BldCfg))
    end.

extract_node_relaxed(Text) ->
    case string:chr(Text, $-) of
        0 ->
            {Text, []};
        Idx ->
            Node = string:sub_string(Text, 1, Idx - 1),
            {Node, string:sub_string(Text, Idx + 1)}
    end.

stop_node(Name, Dir) ->
    io:format("~nStopping '~s' in '~s'...~n", [Name, Dir]),
    case bld_lib:connect_to_node(Dir) of
        {false, Remote} ->
            Msg = "Error: Couldn't connect to node: ~s, skipping...~n",
            io:format(standard_error, Msg, [Remote]);
        {true, Remote} ->
            Res = rpc:call(Remote, init, stop, [], 5000),
            io:format("Result: ~p~n", [Res])
    end.
