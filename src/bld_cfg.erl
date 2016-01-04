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

-export([load_config/1, read_config/1, create_args/6]).

-define(DEFAULT_CONF, "local_setup.conf").

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
     "Updates configuration files on an already installed node.",
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
    not lists:all(Fun, ?RELCHECK) orelse
        begin bld_lib:print(err1()), halt(1) end.

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

%%------------------------------------------------------------------------------

load_config(SetupCfg) ->
    Name = proplists:get_value(default_config, SetupCfg, ?DEFAULT_CONF),
    SetupApp = proplists:get_value(setup_app, SetupCfg),
    File = filename:join(code:priv_dir(SetupApp), Name),

    io:format("Using configuration file: ~p~n", [File]),
    {ok, Config} = file:consult(File),
    Replace = proplists:get_value(install_key_replace, SetupCfg, []),
    io:format("But replacing the following keys:~n~p~n", [Replace]),
    merge_config(Config, Replace).

%%------------------------------------------------------------------------------

read_config(Config) ->
    File = filename:join("etc", "init.conf"),
    io:format(standard_io, "~nTrying to read '~s': ", [File]),
    read_config(Config, file:consult(File)).

read_config(undefined, {error, Err}) ->
    io:format(standard_io, "Unsuccessful: ~p, skipping...~n", [Err]),
    [];
read_config(undefined, {ok, InitConf}) ->
    io:format(standard_io, "Successful.~n", []),
    try_host_config(InitConf);
read_config(Config, {error, Err}) ->
    io:format(standard_io, "Error: ~p~n", [Err]),
    Msg = "Can not read the specified config section '~s' due to the error.~n"
        ++ "Aborting.~n",
    io:format(standard_error, Msg, [Config]),
    halt(1);
read_config(Config, {ok, InitConf}) ->
    io:format(standard_io, "Successful.~n", []),
    merge_config(default_config(InitConf), custom_config(Config, InitConf)).

try_host_config(InitConf) ->
    Default = default_config(InitConf),
    case os:getenv("HOSTNAME") of
        false ->
            Default;
        Found ->
            Msg = "Trying to read config section for HOSTNAME '~s': ",
            io:format(standard_io, Msg, [Found]),
            merge_config(Default, host_config(Found, InitConf))
    end.

default_config(InitConf) ->
    proplists:get_value(default, InitConf, []).

custom_config(Config, InitConf) ->
    case catch list_to_existing_atom(Config) of
        {'EXIT', _} -> custom_config1(Config, undefined);
        Atom -> custom_config1(Config, proplists:get_value(Atom, InitConf))
    end.

custom_config1(Config, undefined) ->
    io:format(standard_io, "Error!~n", []),
    Msg = "Can not read the configuration section '~s', aborting.~n",
    io:format(standard_error, Msg, [Config]),
    halt(1);
custom_config1(_Config, CfgList) ->
    CfgList.

host_config(Config, InitConf) ->
    case catch list_to_existing_atom(Config) of
        {'EXIT', _} -> host_config1(undefined);
        Atom -> host_config1(proplists:get_value(Atom, InitConf))
    end.

host_config1(undefined) ->
    io:format(standard_io, "Not found, skipping...~n", []),
    [];
host_config1(CfgList) ->
    io:format(standard_io, "Successful.~n", []),
    CfgList.


merge_config(BaseCfg, [Elem | T])
  when is_tuple(Elem), is_list(BaseCfg) ->
    Key = element(1, Elem),
    case proplists:lookup(Key, BaseCfg) of
        none ->
            merge_config([Elem | BaseCfg], T);
        Tuple ->
            NewElem = merge_tuple(Tuple, Elem),
            NewCfg = lists:keyreplace(Key, 1, BaseCfg, NewElem),
            merge_config(NewCfg, T)
    end;
merge_config(BaseCfg, []) ->
    BaseCfg;
merge_config(_OldElem, NewElem) ->
    NewElem.

merge_tuple({Key, Val}, {Key, NVal}) ->
    {Key, merge_config(Val, NVal)};
merge_tuple({Key, Val}, {Key, NVal, NOpts}) ->
    {Key, merge_config(Val, NVal), NOpts};
merge_tuple({Key, Val, Opts}, {Key, NVal}) ->
    {Key, merge_config(Val, NVal), Opts};
merge_tuple({Key, Val, Opts}, {Key, NVal, NOpts}) ->
    {Key, merge_config(Val, NVal), merge_options(Opts, NOpts)}.

merge_options(Opts, [Opt | T]) ->
    merge_options(bld_lib:ensure_member(Opt, Opts), T);
merge_options(Opts, []) ->
    Opts.

%%------------------------------------------------------------------------------

create_args(Type, Suffix, Seq, Base, InitConf, RunVars) ->
    Name = bld_rel:get_node_name(Type, Suffix, RunVars),
    Offset = bld_rel:get_port_offset(Type, RunVars) + Seq,
    Cookie = bld_rel:get_cookie(Type, RunVars),
    SetupMod = proplists:get_value(setup_module, RunVars, undefined),
    Hostname = proplists:get_value(hostname, RunVars),
    KeyReplace = get_key_replace(SetupMod, Base, Name, Offset, RunVars),

    Default =
        [
         {<<"=NODE=">>, Name, [global]},
         {<<"=NAMEATHOST=">>, Name ++ "@" ++ get_host(Hostname)},
         {<<"=COOKIE=">>, Cookie},
         {<<"=NAMEPARAM=">>, name_param(Name, Hostname, InitConf)},
         %% Used in node_package scripts
         {<<"{{node}}">>, Name, [global]},
         {<<"{{runner_user}}">>, bld_lib:trim(os:cmd("whoami"))},
         {<<"{{runner_wait_process}}">>, <<"mnesia_sup">>},
         {<<"{{runner_ulimit_warn}}">>, <<"">>},
         {<<"{{runner_patch_dir}}">>, ?PATCHES},
         {<<"{{runner_release}}">>, Name},
         {<<"{{runner_script_dir}}">>,
          <<"\"$( cd \"$(dirname \"$0\")\" ; pwd -L )\"">>},
         {<<"{{runner_base_dir}}">>, <<"{{abs_code_root}}">>},
         {<<"{{runner_etc_dir}}">>, <<"{{abs_node_root}}/etc">>},
         {<<"{{runner_log_dir}}">>, <<"{{abs_node_root}}/log">>},
         {<<"{{runner_lib_dir}}">>, <<"{{abs_code_root}}/lib">>}
         %% Only used in node_package version 2.0.0 or later
         %% {<<"{{numactl_arg}}">>, <<"">>},
         %% {<<"{{cuttlefish}}">>, <<"">>},
         %% {<<"{{cuttlefish_conf}}">>, <<"void_cuttlefish.conf">>, [global]},
         %% {<<"{{platform_data_dir}}">>, <<"void_cuttlefish_data_dir">>}
        ] ++ KeyReplace,

    CfgReplace = proplists:get_value(setup_key_replace, InitConf, []),
    merge_config(Default, CfgReplace).

get_key_replace(undefined, _Base, _Name, _Offset, _RunVars) ->
    [];
get_key_replace(Mod, Base, Name, Offset, RunVars) ->
    {ok, Vars} = Mod:key_replace(Base, Name, Offset, RunVars),
    Vars.

get_host({local, Hostname}) -> Hostname;
get_host({fqdn, Hostname}) -> string:sub_word(Hostname, 1, $.).

name_param(Name, Hostname, InitConf) ->
    Opts = proplists:get_value(options, InitConf, []),
    name_param1(Name, Hostname, lists:member(force_sname, Opts)).

name_param1(Name, {fqdn, _} = Hostname, true) ->
    "-sname " ++ Name ++ "@" ++ get_host(Hostname);
name_param1(Name, {fqdn, Hostname}, false) ->
    "-name " ++ Name ++ "@" ++ Hostname;
name_param1(Name, {local, Hostname}, _) ->
    "-sname " ++ Name ++ "@" ++ Hostname.
