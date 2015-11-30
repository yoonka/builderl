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

-module(bld_cfg).

-include_lib("builderl/include/builderl.hrl").

-export([set_root/1, configure/1, config/1]).

set_root_usage() ->
    [
     "************************************************************************",
     "Updates the ROOTDIR variable in a release created with reltool.",
     "Please only run in the release folder as it updates files in-place!",
     "",
     "Usage:",
     "  update_root_dir.esh [ -h | --help ]",
     "",
     "  -h, --help",
     "    This help.",
     "************************************************************************"
    ].

configure_usage(Allowed) ->
    [
     "************************************************************************",
     "Reconfigures scripts from the node_package repository that are used",
     "to start and manage nodes installed with \"./bin/init.esh\".",
     "See: https://github.com/basho/node_package",
     "",
     "Usage:",
     "  configure.esh [ -h | --help | [ <type> | <type>-<suffix> ]",
     "  configure.esh [ -n (<type> | <type>-<suffix>) <name> ]",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  <type>, <type>-<suffix>",
     "    If only the type is specified then it configures all nodes of the",
     "    given type. If the suffix is specified as well then it configures",
     "    only the specified node type with the supplied suffix. If no type",
     "    is specified then it configures all known node types.",
     "    Known node types are: " ++ string:join(Allowed, ", ") ++ ".",
     "",
     "  -n (<type> | <type>-<suffix>) <name>",
     "    Similar to just specifying <type> or <type>-<suffix> but",
     "    instead of using the <type> or <type>-<suffix> it uses the supplied",
     "    <name> as the name of the service and the command to start and",
     "    control the node.",
     "************************************************************************"
    ].

config_usage() ->
    [
     "************************************************************************",
     "Updates configuration files of an already installed node.",
     "",
     "Usage:",
     "  config.esh [ -h | --help ]",
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

set_root(["-h"]) ->     bld_lib:print(set_root_usage());
set_root(["--help"]) -> bld_lib:print(set_root_usage());
set_root([]) ->         set_root1().

set_root1() ->
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
    not lists:all(Fun, ?RELCHECK) orelse bld_lib:print(err1()), halt(1).

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

%%------------------------------------------------------------------------------

configure(Args) ->
    BldConf = bld_lib:read_builderl_config(),
    configure1(Args, BldConf).

configure1(["-h"], BldCfg) ->
    do_configure_usage(BldCfg);
configure1(["--help"], BldCfg) ->
    do_configure_usage(BldCfg);
configure1(Other, BldCfg) ->
    configure2(Other, bld_lib:get_params(BldCfg), []).

do_configure_usage(BldCfg) ->
    bld_lib:print(configure_usage(bld_lib:get_allowed(BldCfg))).

configure2(["-n", Node, Name | T], P, Acc) ->
    configure2(T, P, bld_lib:add_node(Node, Name, P, Acc));
configure2([Node | T], P, Acc) ->
    configure2(T, P, bld_lib:add_node(Node, Node, P, Acc));
configure2([], P, Acc) ->
    configure3(P, ensure_nodes(P, lists:reverse(Acc)));
configure2(Other, _P, _Acc) ->
    bld_lib:halt_badarg(Other).

ensure_nodes({_, _, BldCfg} = Params, Options) ->
    case lists:keymember(node, 1, Options) of
        true ->
            Options;
        false ->
            Fun = fun(N, Acc) -> bld_lib:add_node(N, N, Params, Acc) end,
            lists:foldl(Fun, Options, bld_lib:get_default_nodes(BldCfg))
    end.

configure3({_, _, BldCfg}, OldOpts) ->
    Options = [{bld_lib:node_name(T, S, BldCfg), N, D}
               || {node, N, T, S, D} <- OldOpts],
    io:format("Using options: ~p~n", [Options]),

    {ok, Cwd} = file:get_cwd(),
    io:format(standard_io, "Using '~s' as code root.~n", [Cwd]),
    Args = [{<<"{{abs_code_root}}">>, Cwd, [global]}],

    io:format(standard_io, "~n => Configuring nodes...~n~n", []),
    lists:foreach(fun(Node) -> configure(Cwd, Node, Args) end, Options).

configure(Cwd, {Node, DestName, Dir}, OldArgs) ->
    io:format(standard_io, "Configure '~s' in '~s'.~n", [Node, Dir]),

    ok = file:set_cwd(Dir),
    {ok, Root} = file:get_cwd(),
    io:format(standard_io, "Using '~s' as node root.~n", [Root]),

    Args = [{<<"{{pipe_dir}}">>, "/var/run/" ++ DestName ++ "/"},
            {<<"{{abs_node_root}}">>, Root, [global]},
            {<<"{{node_cmd_name}}">>, DestName} | OldArgs],

    ok = file:set_cwd("bin"),
    bld_lib:process_file("env.sh.src", "env.sh", Args, [force]),
    bld_lib:process_file("runner.src", "runner", Args, [force]),
    bld_lib:chmod_exe("env.sh"),
    bld_lib:chmod_exe("runner"),

    ok = file:set_cwd(Root),
    SrvSrc = filename:join(["etc", "init.d", "daemon.src"]),
    SrvDst = filename:join(["etc", "init.d", DestName]),
    bld_lib:process_file(SrvSrc, SrvDst, Args, [force]),
    bld_lib:chmod(SrvDst, 8#00755),

    ok = file:set_cwd(Cwd),
    io:format(standard_io, "Finished.~n~n", []),
    [io:format(standard_io, X ++ "~n", []) || X <- link_info(Root, DestName)].

link_info(Folder, Name) ->
    [
     "Now please create this link:",
     "ln -s " ++ Folder ++ "/bin/runner /usr/sbin/" ++ Name,
     "",
     "And copy this file '" ++ Folder ++ "/etc/init.d/"
     ++ Name ++ "' to '/etc/init.d/'",
     ""
    ].

%%------------------------------------------------------------------------------

config(["-h"]) ->     bld_lib:print(config_usage());
config(["--help"]) -> bld_lib:print(config_usage());
config([]) ->         do_config().

do_config() ->
    ok.
