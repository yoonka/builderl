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

-module(bld_cfg).

-include_lib("builderl/include/builderl.hrl").

-export([set_root/1, configure/1, migresia/1, config/1]).

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

migresia_usage() ->
    [
     "************************************************************************",
     "Copies the specified files to the 'migrations' folder replacing",
     "timestamp '11112233445566' (if found, unless '-f' is specified) with the",
     "current timestamp.",
     "",
     "Options '-i' and '-a' are mutually exclusive with '-d'.",
     "",
     "Usage:",
     "    migresia -h",
     "    migresia [ -i (<type> | <type>-<suffix>) | -a application ]",
     "             [ -d directory ] [ -f ] [ --verbose ]",
     "             --import path1/file1 [ pathX/fileX ]...",
     "",
     "  -h",
     "    This help.",
     "",
     "  -i (<type> | <type>-<suffix>)",
     "    Connects to the specified node to obtain the location of the",
     "    'migrations' folder. Mutually exclusive with '-d'.",
     "    This option can be omitted if only one node is configured in the",
     "    'etc/reltool.config' file, in which case that node will be used.",
     "",
     "  -a application",
     "    Obtains the 'migrations' folder for the specified application.",
     "    If not provided then the folder specified by the 'rel_relative_dir'",
     "    migresia application configuration option will be returned.",
     "",
     "  -d directory",
     "    Location of the 'migrations' folder relative to the folder",
     "    containing the release. Mutually exclusive with '-i' and '-a'.",
     "",
     "  --import path1/file1 [ pathX/fileX ]",
     "    Copies the specified list of files to the 'migrations' folder",
     "    replacing timestamp '11112233445566' (or any timestamp if '-f' is",
     "    specified) with the current timestamp.",
     "",
     "    The timestamp will be updated if the file name matches one of the",
     "    following formats (where _any_description is any text consisting of",
     "    characters valid in Erlang module names):",
     "      11112233445566.erl",
     "      11112233445566_any_description.erl",
     "",
     "    Any other file or a file with a different timestamp will be copied",
     "    without any modifictions (unless '-f' is provided). The location of",
     "    the 'migrations' folder is obtained from the running node, or from",
     "    the '-d' option if provided.",
     "",
     "  -f",
     "    Replaces the timestamp even if it doesn't match the specific number",
     "    11112233445566. In this case a sequence of 14 digits followed by",
     "    '.erl' or '_any_description.erl' will be treated as the timestamp to",
     "    replace (see option '--import' for more details).",
     "",
     "  --verbose",
     "    Prints out options used when executing the command.",
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

migresia(["-h"]) ->
    bld_lib:print(migresia_usage());
migresia(Other) ->
    {Opts, F} = parse_migresia(Other, none, {[], []}),

    proplists:get_value(verbose, Opts) =:= undefined orelse
        io:format("~nUsing options:~n~p~n~n", [[{files, F} | Opts]]),

    Node = proplists:get_value(node, Opts),
    App  = proplists:get_value(app, Opts),
    Dir  = proplists:get_value(dir, Opts),

    F =/= [] orelse begin bld_lib:print(err_nofiles()), halt(1) end,
    Names = lists:map(fun(X) -> process_name(X) end, F),

    D = get_migresia_folder(Dir, Node, App),
    Ts = current_ts(),
    Force = proplists:get_value(force, Opts, false),
    lists:foreach(fun(X) -> copy_file(X, Force, Ts, D) end, Names),
    io:format("Done.~n", []).

err_nofiles() ->
    [
     "Error, no files have been specified. Please use '--import' to provide",
     "the list of files to copy to the 'migrations' folder.",
     "Use -h or --help for more information about options."
    ].

parse_migresia(["-i", Node | Rest], _, {Opts, F}) ->
    ensure_one("-i", node, Opts),
    parse_migresia(Rest, none, {[{node, Node} | Opts], F});
parse_migresia(["-a", App | Rest], _, {Opts, F}) ->
    ensure_one("-a", app, Opts),
    parse_migresia(Rest, none, {[{app, App} | Opts], F});
parse_migresia(["-d", Dir | Rest], _, {Opts, F}) ->
    ensure_one("-d", dir, Opts),
    parse_migresia(Rest, none, {[{dir, list_to_binary(Dir)} | Opts], F});
parse_migresia(["-f" | Rest], _, {Opts, F}) ->
    parse_migresia(Rest, none, {bld_lib:ensure_member(force, Opts), F});
parse_migresia(["--verbose" | Rest], _, {Opts, F}) ->
    parse_migresia(Rest, none, {bld_lib:ensure_member(verbose, Opts), F});
parse_migresia(["--import" | Rest], none, {Opts, []}) ->
    parse_migresia(Rest, is_import, {Opts, []});
parse_migresia(["--import" | _], _, _) ->
    bld_lib:halt_toomany("--import");
parse_migresia([File | Rest], is_import, {Opts, Acc}) ->
    parse_migresia(Rest, is_import, {Opts, [File | Acc]});
parse_migresia([], _, {Opts, Acc}) ->
    {Opts, lists:reverse(Acc)};
parse_migresia(Other, _, _) ->
    bld_lib:halt_badarg(Other).

ensure_one(Option, Name, Opts) ->
    proplists:get_value(Name, Opts) =:= undefined
        orelse bld_lib:halt_toomany(Option).

current_ts() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    Args = [Year, Month, Day, Hour, Minute, Second],
    Name = "~w~2.2.0w~2.2.0w~2.2.0w~2.2.0w~2.2.0w",
    list_to_binary(lists:flatten(io_lib:format(Name, Args))).

process_name(X) ->
    filelib:is_regular(X) orelse
        begin bld_lib:print(err_nofile(X)), halt(1) end,
    Y = list_to_binary(X),
    {Y, filename:basename(Y)}.

err_nofile(X) ->
    ["Error, file '" ++ X ++ "' doesn't exist or is not a regular file.",
     "Aborting."].

get_migresia_folder(undefined, Node, App) ->
    BldCfg = bld_lib:read_builderl_config(),
    get_migresia_folder1(Node, App, BldCfg);
get_migresia_folder(Dir, undefined, undefined) ->
    filelib:is_dir(Dir) orelse begin bld_lib:print(err_nodir(Dir)), halt(1) end,
    Dir;
get_migresia_folder(_, _, _) ->
    bld_lib:print(err_exclusive()), halt(1).

err_nodir(Dir) ->
    ["Error, the specified directory '" ++
         binary_to_list(Dir) ++ "' doesn't exist."].

err_exclusive() ->
    ["Error, mutually exclusive options provided.",
     "Please use either '-i' (with or without '-a') or '-d'."].

get_migresia_folder1(undefined, App, BldCfg) ->
    case bld_lib:get_default_nodes(BldCfg) of
        [Node] -> get_migresia_folder1(Node, App, BldCfg);
        List when length(List) > 1 -> bld_lib:print(err_manynodes()), halt(1);
        _ -> bld_lib:print(err_nonodes()), halt(1)
    end;
get_migresia_folder1(Node, App, BldCfg) ->
    Params = bld_lib:get_params(BldCfg),
    {Type, Suffix} = bld_lib:extract_node(Node, Params),
    NodeDir = bld_lib:node_dir(Type, Suffix, BldCfg),
    {Res, Remote} = bld_lib:connect_to_node(NodeDir),
    Res orelse begin bld_lib:print(err_dirnotknown(Node)), halt(1) end,
    try
        case get_migresia_folder(Remote, App) of
            {error, Err1} -> bld_lib:print(err_migrations_dir(Err1)), halt(1);
            Dir -> check_dir(Dir)
        end
    catch
        throw:{error, Err2} -> bld_lib:print(err_migrations_dir(Err2)), halt(1)
    end.

get_migresia_folder(Remote, undefined) ->
    rpc:call(Remote, migresia_migrations, get_default_dir, []);
get_migresia_folder(Remote, App) ->
    rpc:call(Remote, migresia_migrations, get_priv_dir, [list_to_atom(App)]).

check_dir(Dir) when is_binary(Dir) ->
    check_dir(binary_to_list(Dir));
check_dir(Dir) ->
    filelib:is_dir(Dir) orelse
        begin bld_lib:print(err_baddir(Dir)), halt(1) end,
    io:format("The 'migrations' folder received from the node: ~p.~n~n", [Dir]),
    list_to_binary(Dir).

err_manynodes() ->
    [
     "Error, the release runs more than one default node. Please use option",
     "'-i' to specify the node to query for the directory location or '-d' to",
     "specify the directory.",
     "Use -h or --help for more information about options."
    ].

err_nonodes() ->
    ["Error, could not determine default nodes run by this release."].

err_dirnotknown(Node) ->
    ["Error, node '" ++ Node ++ "' is not running.",
     "Please start the node or specify the directory using '-d'."].

err_migrations_dir(Err) ->
    ["Error, could not determine location of the 'migrations' folder: "
     ++ io_lib:format("'~p'.~n", [Err])].

err_baddir(Dir) ->
    [
     "Error, received a 'migrations' directory from the node but the directory",
     "is not valid: '" ++ Dir ++ "'."
    ].

%%------------------------------------------------------------------------------

copy_file({Path, <<Short:14/bytes, ".erl">>}, true, Ts, Dir) ->
    copy_and_replace(Path, <<Ts/binary, ".erl">>, Short, Ts, Dir);
copy_file({Path, <<Short:14/bytes, $_, R/binary>>}, true, Ts, Dir) ->
    copy_and_replace(Path, <<Ts/binary, $_, R/binary>>, Short, Ts, Dir);
copy_file({Path, <<"11112233445566.erl">>}, false, Ts, Dir) ->
    copy_and_replace(Path, <<Ts/binary, ".erl">>, Ts, Dir);
copy_file({Path, <<"11112233445566", $_, R/binary>>}, false, Ts, Dir) ->
    copy_and_replace(Path, <<Ts/binary, $_, R/binary>>, Ts, Dir);
copy_file({Path, Name}, _, _Ts, Dir) ->
    cp_file(Path, Name, [], Dir).

copy_and_replace(Path, Name, From, Ts, Dir) ->
    cp_file(Path, Name, [{From, Ts}], Dir).

copy_and_replace(Path, Name, Ts, Dir) ->
    cp_file(Path, Name, [{<<"11112233445566">>, Ts}], Dir).

cp_file(Path, Name, Args, Dir) ->
    Dest = filename:join(Dir, Name),
    bld_lib:process_file(Path, Dest, Args).

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
