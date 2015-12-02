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

-module(bld_init).

-include_lib("builderl/include/builderl.hrl").

-export([do/1]).

%% Time to wait for the release cmd to start before starting installation
-define(TIMEOUT, 15000).

%% Time to wait for the node to stop before preparing for OTP upgrades
-define(STOP_TIMEOUT, 10000).

%% Time to wait for the node to stop after creating the RELEASES file
-define(REL_STOP_TIMEOUT, 120000).

-define(RELCHECK,
        [{dir, <<"lib">>},
         {file, <<"releases/start_erl.data">>}
        ]).

-define(DIRS,
        [<<"bin">>,
         <<"etc">>,
         <<"etc/init.d">>,
         <<"config">>,
         <<"log">>,
         <<"sasl_logfiles">>,
         <<"shell">>]).

usage(DefNodes, DefJoins, Allowed) ->
    [
     "************************************************************************",
     "This script installs and configures an OTP-compliant release.",
     "It creates new target folders, one folder per each node, alongside",
     "the folder containing the release (as siblings of that folder).",
     "All nodes share the same code contained in the release folder but use",
     "the created target folders to store data specific to each node",
     "(e.g. logs, the mnesia database, configuration files, etc.).",
     "This script should be run from the root of the folder containing the",
     "release.",
     "",
     "Usage:",
     "",
     "  init.esh -h | --help",
     "  init.esh [-f] [--config <section>]",
     "           [-i (<type> | <type>-<suffix>)]...",
     "           [-s (<type> | <type>-<suffix>)]...",
     "           [-o (<type> | <type>-<suffix>)]",
     "           [-j (<type> | <type>-<suffix> | <node@host>)[, ...]]...",
     "",
     "  init.esh [-f] [--config <section>]",
     "    Calling init.esh without neither '-i' nor '-s' automatically adds",
     "    the following default options:",
     "      " ++ DefNodes,
     "      " ++ DefJoins,
     "",
     "During installation the following steps are executed:",
     " 1. Create the destination folder for each node.",
     " 2. Set up the node, copy and update the configuration files.",
     " 3. Start all nodes supplied with '-i' or '-s'.",
     " 4. Execute the installation module on nodes supplied with '-i'.",
     " 5. Stop all nodes supplied with '-i'",
     "    (nodes supplied with '-s' remain running).",
     "",
     "Options:",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  -f",
     "    Force install, delete the target folders if they already exist.",
     "",
     "  --config section",
     "    Name of the config section from the 'etc/init.conf' file. This file",
     "    contains additional strings that will be replaced in application",
     "    configuration files when installing them to the 'config' folder",
     "    on the target system.",
     "    If the section is not specified explicitly then the script tries",
     "    to read the name of the section from the environment variable",
     "    'HOSTNAME'.",
     "",
     "  -i <type>, -i <type>-<suffix>",
     "    Installs the specified node into a new folder, installs the",
     "    configuration files, starts the node, executes the installation",
     "    module and stops the node.",
     "    Can be specified multiple times, e.g.: -i node1 -i node2.",
     "    Accepted types are: " ++ string:join(Allowed, ", ") ++ ".",
     "    Multiple nodes of the same type may be installed by listing them",
     "    with an optional suffix, e.g.: -i node-1 -i node-2.",
     "    Suffix must match the following regular expression: '^[\\w-.#+]+$'.",
     "",
     "  -s <type>, -s <type>-<suffix>",
     "    Similar to '-i' above but stops after the nodes have been set up,",
     "    without executing the installation module on the installed nodes.",
     "    To be used when some nodes need to be installed and configured",
     "    together. It will install the specified node in a new folder,",
     "    install the configuration files, then it will start the specified",
     "    node and will leave it running. That node can then be supplied with",
     "    the '-j' argument when installing another node.",
     "    (See '-j' for more information).",
     "",
     "  -o <type>, -o <type>-<suffix>",
     "    Can be specified only once. Selects the node for which the script",
     "    should prepare files needed to do OTP upgrades:",
     "    sys.config, start.boot and RELEASES",
     "    This option can be omitted if only one node is being installed,",
     "    in which case that node will be selected automatically.",
     "    If more than one nodes is being installed and this option is not",
     "    specified then none of the nodes will be prepared for OTP upgrades."
     "",
     "  -j (<type> | <type>-<suffix> | <node@host>)...",
     "    Used by the node installation module (e.g. to create a shared mnesia",
     "    scheme). Causes the list of nodes specified after a single '-j' to",
     "    be configured together. Can be specified multiple times and each",
     "    list is space-separated (the list is collected until another option",
     "    is found or until the end of the line), e.g.:",
     "    -j node-1 node-2 node-3 -j service 'node@host.org -j database",
     "",
     "    Nodes specified with <type> or <type>-<suffix> also need to be",
     "    specified with '-i' so that they can be installed and configured,",
     "    and the proper node names used when executing the installation",
     "    module.",
     "",
     "    Nodes specified with <node@host> are assumed to be already installed",
     "    and started (see '-s' above). Value of <node@host> should be an atom",
     "    as returned from erlang:node/0 on the actual node. Host 'host' may",
     "    be either a short name or a fully qualified name. All <node@host>",
     "    values will be passed to the installation script as is, however the",
     "    script will try to connect to the specified nodes before progressing",
     "    with the configuration. Therefore nodes specified with <node@host>",
     "    should use the same cookie as one of the nodes installed with '-i'",
     "    during the same script execution, otherwise the script won't be able",
     "    to connect to such node.",
     "",
     "    This option may be specified multiple times with the same nodes.",
     "    Each occurence will result in executing the installation module with",
     "    all nodes specified for a particular occurence of '-j' as one of the",
     "    arguments of that execution.",
     "************************************************************************"
    ].

%%------------------------------------------------------------------------------

do(Args) ->
    check_release(),
    {_, Vsn} = StartErl = bld_lib:start_erl_data(),
    RelDir = filename:join("releases", Vsn),
    BldConf = bld_lib:read_builderl_config(RelDir),
    do1(Args, [{start_erl, StartErl}, {rel_dir, RelDir} | BldConf]).

do1(["-h"], BldConf) ->     print_usage(BldConf);
do1(["--help"], BldConf) -> print_usage(BldConf);
do1(Other, BldConf) ->      do2(Other, BldConf).

print_usage(BldConf) ->
    DefNodes = bld_lib:get_default_nodes(BldConf),
    DefJoins = bld_lib:get_default_joins(BldConf),
    Nodes = ["-i " | string:join(DefNodes, " -i ")],
    Joins = string:join(joins_to_str(DefJoins, ""), " "),
    Allowed = bld_lib:get_allowed(BldConf),
    bld_lib:print(usage(Nodes, Joins, Allowed)).

joins_to_str([{join, List} | T], Acc) ->
    Nodes = string:join(joins_to_str(List, ""), " "),
    joins_to_str(T, [Nodes, "-j" | Acc]);
joins_to_str([{Node, []} | T], Acc) ->
    joins_to_str(T, [atom_to_list(Node) | Acc]);
joins_to_str([{Node, Suffix} | T], Acc) ->
    joins_to_str(T, [atom_to_list(Node) ++ "-" ++ Suffix | Acc]);
joins_to_str([], Acc) ->
    lists:reverse(Acc).

do2(Other, BldConf) ->
    Params = bld_lib:get_params(BldConf),
    Options = add_otp(parse(Other, Params)),
    kick_off(bld_rel:runtime_variables(BldConf), Options).

check_release() ->
    Fun = fun({dir, Dir}) -> filelib:is_dir(Dir);
             ({file, File}) -> filelib:is_regular(File)
          end,

    case lists:all(Fun, ?RELCHECK) of
        true -> ok;
        false -> bld_lib:print(err1()), halt(1)
    end.

err1() ->
    [
     "Incorrect folder structure!",
     "Please make sure that the release has been created correctly and run",
     "this script from the root of the folder containing the release.",
     "Use -h or --help for more options."
    ].

%%------------------------------------------------------------------------------

parse(Args, Params) ->
    parse(Args, Params, false, []).

parse(["--config", Config | T], P, J, Acc) ->
    parse(T, P, false, add_config(Config,                 add_joins(J, Acc)));
parse(["-f" | T],               P, J, Acc) ->
    parse(T, P, false, bld_lib:ensure_member(do_delete,   add_joins(J, Acc)));
parse(["-i", Node | T],         P, J, Acc) ->
    parse(T, P, false, bld_lib:add_node(Node, install, P, add_joins(J, Acc)));
parse(["-s", Node | T],         P, J, Acc) ->
    parse(T, P, false, bld_lib:add_node(Node, setup,   P, add_joins(J, Acc)));
parse(["-o", Node | T],         P, J, Acc) ->
    parse(T, P, false, add_mk_otp(Node, P,                add_joins(J, Acc)));
parse(["-j", Type | T],         P, J, Acc) ->
    parse(T, P, [add_join_type(Type, P)],                 add_joins(J, Acc));
parse([],                       P, J, Acc) ->
    add_seq(P, ensure_nodes(P, lists:reverse(add_joins(J, Acc))));
parse(Other,                    _P, false, _Acc) ->
    bld_lib:halt_badarg(Other);
parse([Type | T],               P, J, Acc) ->
    parse(T, P, [add_join_type(Type, P) | J], Acc).

add_joins(false, Acc) -> Acc;
add_joins(Join, Acc) -> [{join, lists:reverse(Join)} | Acc].

add_config(Config, Acc) ->
    not lists:keymember(config, 1, Acc)
        orelse begin bld_lib:print([err5()]), halt(1) end,
    [{config, Config} | Acc].

add_mk_otp(Node, P, Acc) ->
    not lists:keymember(mk_otp, 1, Acc)
        orelse begin bld_lib:print([err6()]), halt(1) end,
    {Type, Suffix} = bld_lib:extract_node(Node, P),
    [{mk_otp, Type, Suffix} | Acc].

add_join_type(Text, Params) ->
    case string:chr(Text, $@) of
        0 -> bld_lib:extract_node(Text, Params);
        _ -> list_to_atom(Text)
    end.

err5() -> "Error, the '--config' option is specified multiple times.".

err6() -> "Error, option '-o' is specified multiple times.".

ensure_nodes(Params, Options) ->
    case lists:keymember(node, 1, Options) of
        true -> Options;
        false -> add_defaults(Params, Options)
    end.

add_defaults({_, _, BldCfg} = Params, Options) ->
    case lists:keymember(join, 1, Options) of
        true -> bld_lib:print(err7()), halt(1);
        false -> ok
    end,
    Fun = fun(N, Acc) -> bld_lib:add_node(N, install, Params, Acc) end,
    DefNodes = bld_lib:get_default_nodes(BldCfg),
    DefJoins = bld_lib:get_default_joins(BldCfg),
    lists:reverse(lists:foldl(Fun, Options, DefNodes)) ++ DefJoins.

err7() ->
    [
     "Error, nodes provided with '-j' must be also provied with '-i' so that",
     "they can be installed before being configured as joined nodes.",
     "Use -h or --help for more information about options."
    ].

add_seq(P, Options) ->
    add_seq(P, Options, [], []).

add_seq(P, [{node, Action, Type, S, D} | T], NewOptions, Counters) ->
    Seq = proplists:get_value(Type, Counters, 0),
    NewCounters = lists:keystore(Type, 1, Counters, {Type, Seq + 1}),
    add_seq(P, T, [{node, Action, Type, S, Seq, D} | NewOptions], NewCounters);
add_seq(P, [Other | T], NewOptions, Counters) ->
    add_seq(P, T, [Other | NewOptions], Counters);
add_seq(_P, [], NewOptions, _Counters) ->
    lists:reverse(NewOptions).

%%------------------------------------------------------------------------------

add_otp(Options) ->
    case is_otp(Options) of
        true ->
            Options;
        false ->
            Nodes = [{T, S} || {node, _, T, S, _, _} <- Options],
            select_otp(Nodes, Options)
    end.

is_otp(Options) ->
    case lists:keyfind(mk_otp, 1, Options) of
        false ->
            false;
        {mk_otp, Type, Suffix} ->
            check_install(fun err8/1, Type, Suffix, Options)
    end.

check_install(ErrFun, Type, Suffix, Options) ->
    is_install(Type, Suffix, Options)
        orelse halt_no_install(ErrFun, Type, Suffix).

is_install(Type, Suffix, Options) ->
    lists:any(fun(X) -> is_install1(X, Type, Suffix) end, Options).

is_install1({node, install, T, S, _, _}, Type, Suffix)
  when T =:= Type, S =:= Suffix -> true;
is_install1(_X, _Type, _Suffix) -> false.

halt_no_install(ErrFun, Type, Suffix) ->
    bld_lib:print(ErrFun(type_suffix(Type, Suffix))), halt(1).

type_suffix(Type, Suffix) ->
    atom_to_list(Type) ++ if Suffix =:= [] -> ""; true -> "-" ++ Suffix end.

err8(Node) ->
    [
     "Error, node '" ++ Node ++ "' is unknown to the script. Only nodes that",
     "are being installed can be prepared for OTP upgrades.",
     "Please ensure that the node specified with '-o' is also specified",
     "with '-i'.",
     "Use -h or --help for more information about options."
    ].

select_otp([{Type, Suffix}], Options) ->
    Msg = "Single node '" ++ type_suffix(Type, Suffix) ++ "' is being "
        "installed.~nSelecting it to be prepared for OTP upgrades.~n~n",
    io:format(standard_io, Msg, []),
    [{mk_otp, Type, Suffix}|Options];
select_otp(_, Options) ->
    Msg = "Installing multiple nodes and option '-o' is not specified.~n"
        "Not selecting a node to be prepared for OTP upgrades.~n~n",
    io:format(standard_io, Msg, []),
    Options.

%%------------------------------------------------------------------------------

kick_off(RunVars, Options) ->
    Nodes = [{A, T, S, Seq, D} || {node, A, T, S, Seq, D} <- Options],
    Names = [{bld_rel:get_node_name(T, S, RunVars), Dir}
             || {_A, T, S, _Seq, Dir} <- Nodes],
    Clusters = [J || {join, J} <- Options],
    check_joins(Clusters, Options),

    io:format("~nUsing options:~n~p~n", [Options]),
    InitConf = bld_cfg:read_config(proplists:get_value(config, Options)),
    io:format("~nInitConf merged:~n~p~n", [InitConf]),
    check_not_running([Node || {Node, _Dir} <- Names]),

    io:format(standard_io, "~n => Ensure '~s' exists: ", [?PATCHES]),

    io:format(standard_io, "~n => Checking for existing folders...~n~n", []),
    Delete = lists:member(do_delete, Options),
    FolderFun = fun({_N, Dir}) -> check_folder(Dir, Delete) end,
    lists:foreach(FolderFun, Names),

    SetupMod = proplists:get_value(setup_module, RunVars, undefined),
    io:format(standard_io, "~n => Setting up nodes...~n", []),
    Fun = fun(Opt) -> do_install(Opt, SetupMod, InitConf, RunVars) end,
    lists:foreach(Fun, Nodes),

    start_nodes(Names, RunVars),
    Connected = wait_for_nodes(Nodes),

    Joins = lists:usort(lists:flatten(Clusters)),
    Known = lists:usort([X || {_T, _S} = X <- Joins]),
    Added = lists:usort([X || X <- Joins, is_atom(X)]),

    ToConnect = [R || {_A, R, _T, _S} <- Connected] ++ Added,
    Installs = [{R, T, S} || {install, R, T, S} <- Connected],

    connect_to_started(Added),
    install_known(
      Installs, ToConnect, Clusters, Known, InitConf, RunVars),
    install_otp(Installs, RunVars, Options),
    io:format(standard_io, "~nFinished.~n", []).

%%------------------------------------------------------------------------------

check_joins(Clusters, Options) ->
    All = lists:usort(lists:flatten(Clusters)),
    [check_install(fun err4/1, Type, Suffix, Options) || {Type, Suffix} <- All].

err4(Node) ->
    [
     "Error, node '" ++ Node ++ "' is unknown to the script. Only nodes that",
     "are being installed can be joined.",
     "Please ensure that the node type specified with the '-j' option is also",
     "specified with a '-i' options.",
     "Alternatively install the node using the option '-s' and specify it by",
     "it's name.",
     "Use -h or --help for more information about options."
    ].

%%------------------------------------------------------------------------------

check_not_running(Nodes) ->
    case bld_lib:intersection(bld_lib:running_nodes(), Nodes) of
        [] -> ok;
        ToStop -> halt_badnodes(ToStop)
    end.

halt_badnodes(Nodes) ->
    Msg = "~nSome of the nodes you asked to install are already running.~n" ++
        "Please stop the following nodes before re-installing them:~n~p~n",
    io:format(standard_error, Msg, [Nodes]),
    halt(1).


check_folder(Name, Delete) ->
    io:format(standard_io, "Check folder '~s': ", [Name]),
    case {filelib:is_file(Name), Delete} of
        {true, false} ->
            io:format(standard_io, "Error!~n", []),
            bld_lib:print(err2(Name)),
            halt(1);
        {true, true} ->
            io:format(standard_io, "Present -> Deleting..~n", []),
            Cmd = "rm -rf " ++ Name,
            io:format(standard_io, Cmd ++ "~n", []),
            Res = os:cmd(Cmd),
            if Res /= "" ->
                    Msg = "Error when deleting '" ++ Name ++ "': ~n",
                    io:format(standard_error, Msg ++ Res, []),
                    halt(1);
               true ->
                    io:format(standard_io, "Done.~n", [])
            end;
        {false, _} ->
            io:format(standard_io, "None, will create.~n", [])
    end.

err2(Name) ->
    [
     "'" ++ Name ++ "' already exists, aborting.~n",
     "Use -f to force deletion of the existing folders.",
     "Use -h or --help for more options."
    ].

%%------------------------------------------------------------------------------

do_install({_Action, Type, Suffix, Seq, Base}, SetupMod, InitConf, RunVars) ->
    bld_lib:h_line("~n---" ++ type_suffix(Type, Suffix)),
    io:format(standard_io, "Installing in '" ++ Base ++ "'.~n", []),
    bld_lib:mk_dir(Base),
    do_subfolders(SetupMod, Base),
    install_cookie(Type, Base, RunVars),
    CfgArgs = bld_cfg:create_args(Type, Suffix, Seq, Base, InitConf, RunVars),
    Name = bld_rel:get_node_name(Type, Suffix, RunVars),
    RelDir = init_rel_files(Name, InitConf, RunVars, CfgArgs),
    Privs = process_data_file(RelDir, Base, Type, Name, RunVars, CfgArgs),
    process_app_configs(SetupMod, Privs, Base, CfgArgs),

    {ErtsVsn, _Vsn} = proplists:get_value(start_erl, RunVars),
    node_package_setup(ErtsVsn, Base, CfgArgs).

do_subfolders(SetupMod, Base) ->
    Msg = "Creating subfolders in '" ++ Base ++ "'...~n",
    io:format(standard_io, Msg, []),
    SubFun = fun(Dir) -> bld_lib:mk_dir(filename:join(Base, Dir)) end,
    lists:foreach(SubFun, ?DIRS ++ get_subfolders(SetupMod)).

get_subfolders(undefined) ->
    [];
get_subfolders(Mod) ->
    {ok, Folders} = Mod:subfolders(),
    Folders.

install_cookie(Type, Base, RunVars) ->
    File = filename:join(Base, ".erlang.cookie"),
    Cookie = bld_rel:get_cookie(Type, RunVars),
    io:format(standard_io, "Write cookie '~s': ", [File]),
    bld_lib:check_file_op(file:write_file(File, Cookie ++ "\n")).

%%------------------------------------------------------------------------------

init_rel_files(Name, InitConf, RunVars, CfgArgs) ->
    RelDir = proplists:get_value(rel_dir, RunVars),
    CfgName = proplists:get_value(sys_config, InitConf, <<"sys.config.src">>),
    ConfigSrc = filename:join(RelDir, CfgName),
    ConfigDest = filename:join(RelDir, config_name(Name)),
    bld_lib:process_file(ConfigSrc, ConfigDest, CfgArgs, [force]),
    RelDir.

config_name(Name) -> Name ++ ".config".

%%------------------------------------------------------------------------------

process_data_file(RelDir, Base, Type, Name, RunVars, CfgArgs) ->
    RelName = bld_rel:get_release_name(Type, RunVars),
    Path = erl_path(proplists:get_value(is_dev, RunVars)),
    StartFile = Name ++ start_file_ext(),
    create_start_script(Path, Base, RelName, Name, "vm.args", StartFile),

    Bytes = rel_sh(Path, Base, RelName, Name),
    write_start_script(Base, Name ++ ?RELEASES_EXT, Bytes),

    Data = get_rel_data(RelDir, RelName ++ ".data"),
    mk_vm_args(Base, Data, "vm.args", CfgArgs),

    case proplists:get_value(setup_release, RunVars) of
        undefined -> ok;
        CmdRel -> install_cmd_scripts(RelDir, Path, Base, CmdRel, Name, CfgArgs)
    end,
    LibDirs = proplists:get_value(lib_dirs, Data, ["lib"]),
    get_priv_dirs(RelDir, RelName, LibDirs).

%% ?BUILD_INFO is created by mk_dev.esh and only present in the dev environment
erl_path(true) -> "";
erl_path(false) -> "./bin/".

create_start_script(Path, Base, Rel, CfgRel, ArgsFile, StartFile) ->
    Bytes = cmd_sh(Path, Base, Rel, CfgRel, ArgsFile),
    write_start_script(Base, StartFile, Bytes).

write_start_script(Base, StartFile, Bytes) ->
    File = filename:join([Base, "bin", StartFile]),
    io:format(standard_io, "Write start script '~s': ", [File]),
    bld_lib:check_file_op(file:write_file(File, Bytes)),
    bld_lib:chmod_exe(File).

cmd_sh(Path, Base, Rel, CfgRel, ArgsFile) ->
    [
     "#!/bin/sh\n",
     "START_ERL=`cat releases/start_erl.data`\n",
     "APP_VSN=${START_ERL#* }\n",
     Path ++ "run_erl -daemon " ++ Base ++ "/shell/ " ++ Base ++ "/log \"exec ",
     Path ++ "erl " ++ Base ++ " releases releases/start_erl.data -config ",
     "releases/$APP_VSN/" ++ CfgRel ++ ".config -args_file " ++ Base ++ "/etc/",
     ArgsFile ++ " -boot releases/$APP_VSN/" ++ Rel ++ "\"\n"
    ].

rel_sh(Path, Base, Rel, CfgRel) ->
    [
     "#!/bin/sh\n",
     "START_ERL=`cat releases/start_erl.data`\n",
     "APP_VSN=${START_ERL#* }\n",
     Path ++ "run_erl -daemon " ++ Base ++ "/shell/ " ++ Base ++ "/log \"exec ",
     Path ++ "erl " ++ Base ++ " releases releases/start_erl.data -config ",
     "releases/$APP_VSN/" ++ CfgRel ++ ".config -args_file " ++ Base,
     "/etc/vm.args -boot releases/$APP_VSN/" ++ Rel ++ " -noshell -noinput ",
     "-eval \\\"{ok, Cwd} = file:get_cwd(), release_handler:create_RELEASES(",
     "Cwd, \\\\\\\"releases\\\\\\\", \\\\\\\"releases/$APP_VSN/"
     ++ Rel ++ ".rel\\\\\\\", []), init:stop()\\\"\"\n"
    ].

%% The *.data file is only present in the development environment where
%% it contains data used when creating configuration files needed
%% to boot releases without actually creating them. When the script
%% is run in a proper OTP release, where this file is not present, it simply
%% assumes OTP-compliant defaults.
get_rel_data(RelDir, DataFile) ->
    case file:consult(filename:join(RelDir, DataFile)) of
        {ok, Vars} -> Vars;
        {error, _} -> []
    end.

mk_vm_args(Base, Data, VMFile, Args) ->
    Args2 = [{<<"=PATHS=">>, get_rel_paths(Data)} | Args],
    config_from_template("vm.args.src", Base, VMFile, Args2).

get_rel_paths(Vars) ->
    case proplists:get_value(paths, Vars) of
        undefined -> "#Example: -pa dirA/app1/ebin dirB/app2/ebin";
        Paths -> "-pa " ++ string:join(Paths, " ")
    end.

config_from_template(Src, Base, Dest, Args) ->
    SrcFile = filename:join("etc", Src),
    DestFile = filename:join([Base, "etc", Dest]),
    bld_lib:process_file(SrcFile, DestFile, Args).

install_cmd_scripts(RelDir, Path, Base, CmdRel, Name, CfgArgs) ->
    VmArgs = "vm." ++ CmdRel ++ ".args",
    StartFile = Name ++ start_file_ext(CmdRel),
    create_start_script(Path, Base, CmdRel, Name, VmArgs, StartFile),
    CmdData = get_rel_data(RelDir, CmdRel ++ ".data"),
    mk_vm_args(Base, CmdData, VmArgs, CfgArgs).

start_file_ext() -> ".sh".

start_file_ext(undefined) -> start_file_ext();
start_file_ext(Middle) -> "." ++ Middle ++ ".sh".

%%------------------------------------------------------------------------------

get_priv_dirs(RelDir, RelName, LibDirs) ->
    [{release, _, _, OldApps}] = get_rel_spec(RelDir, RelName),
    Apps = [app_folder(X) || X <- OldApps],
    bld_compat:filtermap(fun(X) -> get_priv_dir(LibDirs, X) end, Apps).

get_rel_spec(RelDir, RelName) ->
    File = filename:join(RelDir, RelName ++ ".rel"),
    io:format(standard_io, "Read the release file '~s': ", [File]),
    case file:consult(File) of
        {ok, Vars} ->
            io:format(standard_io, "Done.~n", []),
            Vars;
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end.

app_folder({App, Vsn}) -> {App, atom_to_list(App), Vsn};
app_folder({App, Vsn, _}) -> {App, atom_to_list(App), Vsn}.

%% For now in the development environment the application folders don't end with
%% the -vsn suffix. But look for priv in the version-specific folder first.
get_priv_dir(LibDirs, {App, Name, Vsn}) ->
    DirVsn = filename:join(Name ++ "-" ++ Vsn, "priv"),
    Dir = filename:join(Name, "priv"),
    case get_priv_dir(false, LibDirs, App, Name, DirVsn, undefined) of
        false -> get_priv_dir(false, LibDirs, App, Name, Dir, undefined);
        Exists -> Exists
    end.

get_priv_dir(false, [LibDir | T], App, Name, Dir, _) ->
    Path = filename:join(LibDir, Dir),
    IsDir = filelib:is_dir(Path),
    get_priv_dir(IsDir, T, App, Name, Dir, Path);
get_priv_dir(true, _, App, Name, _Dir, Path) ->
    {true, {App, Name, Path}};
get_priv_dir(false, [], _App, _Name, _Dir, _Path) ->
    false.

%%------------------------------------------------------------------------------

process_app_configs(SetupMod, Privs, Base, CfgArgs) ->
    Dest = filename:join(Base, "config"),
    Msg = "~nProcessing configuration files, will copy to '~s'~n"
        ++ " and in each file replace keys with the following values:~n~p~n",
    io:format(standard_io, Msg, [Dest, CfgArgs]),
    Fun = fun(App) -> do_app_cfg(SetupMod, App, Dest, CfgArgs, Privs) end,
    lists:foreach(Fun, Privs).

do_app_cfg(undefined, App, Dest, CfgArgs, _Privs) ->
    process_config(App, Dest, CfgArgs);
do_app_cfg(Mod, App, Dest, CfgArgs, Privs) ->
    case Mod:process_config(App, Dest, CfgArgs, Privs) of
        true -> ok;
        false -> process_config(App, Dest, CfgArgs)
    end.

process_config({_App, Name, PrivDir}, CfgDir, CfgArgs) ->
    process_priv_file(PrivDir, Name ++ ".conf", CfgDir, CfgArgs),
    process_priv_file(PrivDir, Name ++ ".install.conf", CfgDir, CfgArgs).

process_priv_file(PrivDir, File, CfgDir, CfgArgs) ->
    Config = filename:join(PrivDir, File),
    case filelib:is_regular(Config) of
        false -> ok;
        true -> bld_lib:process_file(Config, [CfgDir, File], CfgArgs)
    end.


node_package_setup(ErtsVsn, Base, Args) ->
    SrcDir = filename:join("erts-" ++ ErtsVsn, "bin"),

    RunnerDst = filename:join([Base, "bin", "runner.src"]),
    bld_lib:process_file(filename:join(SrcDir, "runner"), RunnerDst, []),

    EnvDst = filename:join([Base, "bin", "env.sh.src"]),
    bld_lib:process_file(filename:join(SrcDir, "env.sh"), EnvDst, Args),

    SrvDst = filename:join([Base, "etc", "init.d", "daemon.src"]),
    SrvSrc = filename:join(["etc", "init.d", "daemon.src"]),
    bld_lib:process_file(SrvSrc, SrvDst, Args).

%%------------------------------------------------------------------------------

start_nodes(Names, RunVars) ->
    StartExt = start_file_ext(proplists:get_value(setup_release, RunVars)),
    io:format(standard_io, "~n => Starting nodes...~n~n", []),
    lists:foreach(fun(Node) -> bld_lib:start_node(Node, StartExt) end, Names).


stop_nodes(Names) ->
    io:format(standard_io, "~n => Stopping installed nodes...~n", []),
    lists:foreach(fun(R) -> stop_node(R) end, [R || {R, _T, _S} <- Names]).

stop_node(Remote) ->
    io:format(standard_io, "Stopping node '~s'...~n", [Remote]),
    Res2 = rpc:call(Remote, init, stop, [], 5000),
    io:format("Result: ~p~n", [Res2]).

%%------------------------------------------------------------------------------

wait_for_nodes(Nodes) ->
    io:format(standard_io, " => Waiting for nodes...~n~n", []),
    wait_for_nodes(Nodes, ?TIMEOUT, os:timestamp(), []).

wait_for_nodes([{_, _, _, _, Base} = Opt | T], Timeout, Start, Acc) ->
    {Res, Remote} = bld_lib:connect_to_node(Base),
    TimeDiff = bld_lib:get_time_diff(Start),
    wait_for_nodes(Res, Remote, Opt, T, Timeout, Start, TimeDiff, Acc);
wait_for_nodes([Remote | T], Timeout, Start, Acc) when is_atom(Remote) ->
    Res = connect_with_name(Remote),
    TimeDiff = bld_lib:get_time_diff(Start),
    wait_for_nodes(Res, Remote, Remote, T, Timeout, Start, TimeDiff, Acc);
wait_for_nodes([], _Timeout, _Start, Acc) ->
    lists:reverse(Acc).

connect_with_name(Remote) ->
    ping_to_bool(net_adm:ping(Remote)).

ping_to_bool(pang) -> false;
ping_to_bool(pong) -> true.

wait_for_nodes(true, Remote, Opt, T, Timeout, Start, _TimeDiff, Acc) ->
    io:format(standard_io, "Connected to '~s'.~n", [Remote]),
    wait_for_nodes(T, Timeout, Start, [new_opt(Opt, Remote) | Acc]);
wait_for_nodes(false, Remote, Opt, T, Timeout, Start, TimeDiff, Acc)
  when TimeDiff < Timeout ->
    timer:sleep(500),
    Res = connect_with_name(Remote),
    NewTimeDiff = bld_lib:get_time_diff(Start),
    wait_for_nodes(Res, Remote, Opt, T, Timeout, Start, NewTimeDiff, Acc);
wait_for_nodes(false, Remote, _Opt, _T, _Timeout, _Start, _TimeDiff, _Acc) ->
    Msg = "Time out error when waiting for node '~s', aborting...~n",
    io:format(standard_error, Msg, [Remote]),
    halt(1).

new_opt({Action, Type, Suffix, _Seq, _Base}, Remote) ->
    {Action, Remote, Type, Suffix};
new_opt(Remote, Remote) ->
    Remote.


connect_to_started([]) ->
    ok;
connect_to_started(Added) ->
    StartedMsg = "~n => Connecting to already started nodes...~n~n",
    io:format(standard_io, StartedMsg, []),
    Nodes = wait_for_nodes(Added, ?TIMEOUT, os:timestamp(), []),
    Added = lists:usort(Nodes).

%%------------------------------------------------------------------------------

install_known([], _Names, _Clusters, _Known, _InitConf, _RunVars) ->
    ok;
install_known(Installs, Names, OldClusters, Known, InitConf, RunVars) ->
    io:format(standard_io, "~n => Installing nodes...~n", []),

    AddMod = fun({R, T, S}) ->
                     {R, T, S, bld_rel:get_config_module(T, RunVars)}
             end,
    AllToSetup = [AddMod(X) || X <- Installs],
    FilterFun = fun({_R, T, S, _M}) -> not lists:member({T, S}, Known) end,
    NotInJoins = lists:filter(FilterFun, AllToSetup),
    Converted = [convert_join(X) || X <- NotInJoins],
    Clusters = [ [convert_join(N, AllToSetup) || N <- C] || C <- OldClusters],
    ToSetup = Converted ++ Clusters,

    io:format(standard_io, "~nClusters:~n~p~n", [ToSetup]),
    Running = lists:zip(Names, read_cookies(Names)),
    SetupApp = proplists:get_value(setup_app, RunVars, undefined),
    SetupCfg = [{running_nodes, Running} | InitConf],
    lists:foreach(fun(X) -> configure_node(X, SetupApp, SetupCfg) end, ToSetup),

    stop_nodes(Installs).

read_cookies(Names) ->
    case rpc:multicall(Names, erlang, get_cookie, []) of
        {Cookies, []} ->
            Cookies;
        {_, BadNames} ->
            Msg = "Error, couldn't read cookies from nodes: ~p~n.",
            io:format(standard_error, Msg, [BadNames]),
            halt(1)
    end.

convert_join({Type, Suffix}, [{_Remote, Type, Suffix, _Module} = H | _T]) ->
    convert_join(H);
convert_join({_Type, _Suffix} = N, [_ | T]) ->
    convert_join(N, T);
convert_join(Remote, _AllToSetup) when is_atom(Remote) ->
    {Remote, Remote, undefined}.

convert_join({Remote, Type, Suffix, Module}) ->
    {list_to_atom(type_suffix(Type, Suffix)), Type, Remote, Module}.

configure_node(ToSetup, SetupApp, SetupCfg) ->
    configure_info(ToSetup),
    {_Remote, Module} = InstInfo = install_info(ToSetup),
    if SetupApp == undefined orelse Module == undefined ->
            bld_lib:print(skip_msg(SetupApp, Module));
       true ->
            NewSetupCfg = [{setup_app, SetupApp} | SetupCfg],
            configure_node1(ToSetup, InstInfo, SetupApp, NewSetupCfg)
    end.

configure_info({Id, _Type, _Remote, _Module}) ->
    bld_lib:h_line("~n---" ++ atom_to_list(Id));
configure_info(Cluster) ->
    bld_lib:h_line("~n---cluster"),
    [io:format(standard_io, cluster_node(C), []) || C <- Cluster],
    bld_lib:h_line("").

cluster_node({Remote, _Type, _, _, undefined}) ->
    "   " ++ atom_to_list(Remote) ++ "~n";
cluster_node({Id, _Type, Remote, _}) ->
    "   " ++ atom_to_list(Id) ++ " (" ++ atom_to_list(Remote) ++ ")~n".

install_info({_Id, _Type, Remote, Module}) -> {Remote, Module};
install_info(Cluster) -> first_known(Cluster).

first_known([{_Id, _Type, _Remote, undefined} | T]) -> first_known(T);
first_known([{_Id, _Type, Remote, Module} | _T]) -> {Remote, Module}.

skip_msg(undefined, _Module) ->
    ["Skipping, the setup application is not defined."];
skip_msg(_SetupApp, undefined) ->
    ["Skipping, the setup module is not defined."];
skip_msg(undefined, undefined) ->
    ["Skipping, setup application and module are not defined."].

configure_node1({Id, _Type, Remote, Module}, _, SetupApp, SetupCfg) ->
    Msg0 = "Configuring ~p with options:~n~p~n~n",
    io:format(standard_io, Msg0, [Remote, SetupCfg]),

    Res1 = rpc:call(Remote, bld_compat, ensure_all_started, [SetupApp]),
    io:format(standard_io, "Finished starting applications:~n~p~n", [Res1]),

    ok = rpc:call(Remote, Module, install, [{Id, Remote}, SetupCfg]),
    io:format(standard_io, "Done.~n", []);

configure_node1(Cluster, {Remote, Module}, SetupApp, SetupCfg) ->
    io:format(standard_io, "Using options:~n~p~n~n", [SetupCfg]),

    Nodes = [R || {_Id, _Type, R, _M} <- Cluster],
    Res0 = rpc:multicall(Nodes, bld_compat, ensure_all_started, [SetupApp]),
    io:format(standard_io, "Finished starting applications:~n~p~n", [Res0]),

    Args = [{Id, R} || {Id, _Type, R, _Module} <- Cluster],
    ok = rpc:call(Remote, Module, install, [Args, SetupCfg]),
    io:format(standard_io, "Done.~n", []).

%%------------------------------------------------------------------------------

install_otp(Installs, RunVars, Options) ->
    install_otp(Installs, RunVars, Options, lists:keyfind(mk_otp, 1, Options)).

install_otp(_Installs, _RunVars, _Options, false) ->
    ok;
install_otp(Installs, RunVars, Options, {_, Type, Suffix}) ->
    Name = bld_rel:get_node_name(Type, Suffix, RunVars),
    bld_lib:h_line("~n---" ++ Name),
    io:format(standard_io, "Preparing for OTP upgrades...~n~n", []),

    {_, _, _, _, Base} = Node = find_node(Type, Suffix, Options),
    Remote = find_remote(Type, Suffix, Installs),
    mk_otp_links(Type, Name, RunVars),
    wait_node_stopped(Remote, ?STOP_TIMEOUT),

    Msg = "~n => Starting node to create the RELEASES file...~n~n",
    io:format(standard_io, Msg, []),
    bld_lib:start_node({Name, Base}, ?RELEASES_EXT),
    wait_for_nodes([Node]),

    wait_node_stopped(Remote, ?REL_STOP_TIMEOUT).

find_node(T, S, [{node, install, T, S, Seq, D}|_]) -> {install, T, S, Seq, D};
find_node(T, S, [_|Tail]) -> find_node(T, S, Tail).

find_remote(T, S, [{R, T, S}|_]) -> R;
find_remote(T, S, [_|Tail]) -> find_remote(T, S, Tail).

mk_otp_links(Type, Name, RunVars) ->
    RelDir = proplists:get_value(rel_dir, RunVars),
    ConfigLnk = filename:join(RelDir, "sys.config"),
    BootLnk = filename:join(RelDir, "start.boot"),
    bld_lib:rm_link(ConfigLnk),
    bld_lib:rm_link(BootLnk),

    RelName = bld_rel:get_release_name(Type, RunVars),
    bld_lib:mk_link(config_name(Name), ConfigLnk),
    bld_lib:mk_link(RelName ++ ".boot", BootLnk).

wait_node_stopped(Remote, Timeout) ->
    Msg = "~n => Waiting for node '~p' to stop...~n",
    io:format(standard_io, Msg, [Remote]),
    wait_node_stopped(Remote, Timeout, os:timestamp()).

wait_node_stopped(Remote, Timeout, Start) ->
    case net_adm:ping(Remote) of
        pang -> ok;
        pong -> wait_node_stopped1(Remote, Timeout, Start)
    end.

wait_node_stopped1(Remote, Timeout, Start) ->
    case bld_lib:get_time_diff(Start) >= Timeout of
        true -> halt_cannot_stop(Remote);
        false -> ok
    end,
    timer:sleep(500),
    wait_node_stopped(Remote, Timeout, Start).

halt_cannot_stop(Remote) ->
    Msg = "Error, node '~p' didn't stop, can't continue, aborting.~n",
    io:format(standard_error, Msg, [Remote]),
    halt(1).
