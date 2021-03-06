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

-module(bld_rel).

-include_lib("builderl/include/builderl.hrl").

-export([mk_dev/0, mk_rel/1]).

%% Internal exports
-export([
         get_reltool_config/0,
         runtime_variables/1,
         get_release_name/2,
         get_node_name/3,
         get_config_module/2,
         get_port_offset/2,
         get_cookie/2
        ]).

-define(RELTOOL_CONFIG, "etc/reltool.config").
-define(SYS_CONFIG_TMPL, "sys.config.src").

%% Location of the vm_local.args file used when starting local VMs
-define(LOCAL_VM_ARGS, "tmp/vm_local.args").

%% Where the reltool release will be created
-define(REL_PATH, "tmp/rel").

%% File with name of the current release (used when creating the .tar.gz)
-define(RELEASE_DATA, "tmp/release.data").

%%------------------------------------------------------------------------------
%% Generates files (.rel, .script, .boot) that can be used to boot the release
%% targets (as found in reltool.config) in the local development environment.
%% (i.e. without creating the actual release).

mk_dev() ->
    File = get_reltool_config(),
    BldCfg = get_builderl_config(File),
    LibDirs = get_lib_dirs(File),
    CodePaths = code_paths(LibDirs, []),

    [process_dev(Config, CodePaths) || {config, _} = Config <- File],

    write_vm_local_args(CodePaths),

    ErtsVsn = erlang:system_info(version),
    Vsn = bld_lib:keyget(boot_version, BldCfg),
    write_start_erl_data(ErtsVsn, Vsn),
    write_build_info(Vsn),
    write_builderl_config(Vsn, BldCfg, LibDirs),
    link_configs(Vsn, config_files(BldCfg)),

    io:format("Finished.~n", []).


mk_rel(BldLink) ->
    File = get_reltool_config(),
    BldCfg = get_builderl_config(File),
    {ok, CWD} = file:get_cwd(),
    io:format("Ensure dir exists: ~p~n", [CWD ++ "/" ++ ?REL_PATH ++ "/"]),
    ok = filelib:ensure_dir(?REL_PATH ++ "/"),

    RelNames = [process_rel(Config) || {config, _} = Config <- File],

    link_builderl(BldLink, ?REL_PATH),

    Vsn = bld_lib:keyget(boot_version, BldCfg),
    RelDir = filename:join("releases", Vsn),
    RelPath = filename:join(?REL_PATH, RelDir),
    bld_lib:cp_file(RelDir, RelPath, ?BUILDERL_CONFIG),
    cp_configs(RelDir, RelPath, config_files(BldCfg)),

    RelFiles = [X ++ ".rel" || X <- RelNames -- get_cmd_rel(BldCfg)],
    Releases = filename:join(?REL_PATH, "releases"),
    Fun = fun(X) -> bld_lib:cp_file(RelPath, Releases, X) end,
    lists:foreach(Fun, RelFiles),

    io:format("Create version information: '~s': ", [?RELEASE_DATA]),
    bld_lib:check_file_op(file:write_file(?RELEASE_DATA, Vsn)).

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


get_builderl_config(File) ->
    Config = proplists:get_value(builderl, File),
    DeepRels = [ [R || {rel, R, _, _} <- S] || {config, {sys, S}} <- File],
    Rels = lists:foldl(fun(List, Acc) -> List ++ Acc end, [], DeepRels),
    RelTypes = proplists:get_value(release_types, Config, []),
    CmdRel = get_cmd_rel(Config),
    verify_rel_names(Rels, [X || {_, X, _, _, _} <- RelTypes] ++ CmdRel),
    Config.

get_cmd_rel(Cfg) ->
    case proplists:get_value(setup_config, Cfg) of
        undefined -> [];
        {undefined, _, _} -> [];
        {CmdRel, _, _} -> [CmdRel]
    end.

verify_rel_names(Rels, [N | T]) ->
    case lists:member(N, Rels) of
        false -> halt_bad_rel_name(N);
        true -> verify_rel_names(Rels, T)
    end;
verify_rel_names(_Rels, []) ->
    ok.

halt_bad_rel_name(Name) ->
    Msg = "Error: release '~s' is not defined in '~s', aborting.~n",
    io:format(Msg, [Name, ?RELTOOL_CONFIG]),
    halt(1).


get_lib_dirs(File) ->
    Lists = [proplists:get_value(lib_dirs, Sys, [])
             || {config, {sys, Sys}} <- File],
    lists:usort(lists:merge(Lists)).

code_paths([Dir|T], Acc) ->
    code_paths(T, filelib:wildcard(Dir ++ "/*/ebin") ++ Acc);
code_paths([], Acc) ->
    lists:reverse(Acc).

%%------------------------------------------------------------------------------

process_dev({config, {sys, Sys}} = Config, CodePaths) ->
    io:format(" => Retrieving release configuration...", []),
    {ok, Server} = reltool:start_server([Config]),
    RelList = [Rel || {rel, Rel, _, _} <- Sys],
    RelFun = fun(Rel, {ok, Release}) -> {Rel, Release} end,
    Releases = [RelFun(R, reltool:get_rel(Server, R)) || R <- RelList],
    ok = reltool:stop(Server),
    io:format("OK~n", []),

    LibDirs = proplists:get_value(lib_dirs, Sys, []),
    BootRel = inc_get_boot_rel(Sys),
    process_boot_rel(CodePaths, LibDirs, BootRel, Releases).

process_boot_rel(CodePaths, LibDirs, Rel, Releases) ->
    {release, {Rel, BootVsn}, {erts, _ErtsVsn}, _Apps}
        = proplists:get_value(Rel, Releases),

    DirBase = get_release_dir(BootVsn),

    Apps = get_apps(Releases),
    ValidPaths = filter_paths(CodePaths, Apps, []),

    VmVars = filename:join(DirBase, Rel ++ ".data"),
    io:format(" => Create file: ~s~n", [VmVars]),
    Args = [{paths, ValidPaths}, {lib_dirs, LibDirs}, {rel_version, BootVsn}],
    Term = lists:flatten(io_lib:format("~p.~n~p.~n~p.~n", Args)),
    ok = file:write_file(VmVars, Term),

    code:add_paths(ValidPaths),
    lists:foreach(fun(Elem) -> make_rel(DirBase, Elem) end, Releases),
    [code:del_path(Path) || Path <- ValidPaths].

get_release_dir(BootVsn) ->
    DirBase = filename:join(["releases", BootVsn]),
    io:format(" => Ensure dir exists: ~p~n", [DirBase ++ "/"]),
    ok = filelib:ensure_dir(DirBase ++ "/"),
    DirBase.

get_apps(Releases) ->
    Fun = fun({_, {release, _, {erts, _}, Apps}}, Acc) -> Apps ++ Acc end,
    All = lists:foldl(Fun, [], Releases),
    lists:usort([element(1, App) || App <- All]).

filter_paths([Path|T], Apps, Acc) ->
    [_, Name, _] = filename:split(Path),
    case catch lists:member(list_to_existing_atom(Name), Apps) of
        true -> filter_paths(T, Apps, [Path|Acc]);
        false -> filter_paths(T, Apps, Acc);
        {'EXIT', _} -> filter_paths(T, Apps, Acc)
    end;
filter_paths([], _Apps, Acc) ->
    Acc.

make_rel(Dir, {Rel, Release}) ->
    PathBase = filename:join(Dir, Rel),
    RelFile = PathBase ++ ".rel",
    io:format(" => Create file: ~s~n", [RelFile]),
    ok = file:write_file(RelFile, io_lib:format("~p.~n", [Release])),
    io:format(" => Create release: ~s~n", [Rel]),
    ok = systools:make_script(PathBase),
    io:format("Release '~s' created.~n~n", [Rel]).

%%------------------------------------------------------------------------------

process_rel({config, {sys, Sys}} = Config) ->
    BootRel = inc_get_boot_rel(Sys),
    io:format("Create release '~s' in '~s'... ~n", [BootRel, ?REL_PATH]),
    {ok, Server} = reltool:start_server([Config]),
    check_result(reltool:create_target(Server, ?REL_PATH ++ "/")),
    ok = reltool:stop(Server),
    BootRel.

check_result(ok) ->
    io:format("Done.~n");
check_result({error, Reason}) ->
    io:format("Error:~n~p~n", [Reason]).

%%------------------------------------------------------------------------------

inc_get_boot_rel(Sys) ->
    case proplists:get_value(boot_rel, Sys) of
        undefined ->
            io:format("Error, can't find 'boot_rel' tuple, aborting!~n"),
            halt(1);
        BootRel ->
            BootRel
    end.


write_vm_local_args(CodePaths) ->
    PathsArg = "-pa " ++ string:join(CodePaths, " "),
    Vars = [{options, [force]}, {<<"=PATHS=">>, PathsArg}],
    bld_lib:process_file(["etc", "vm_local.args.src"], ?LOCAL_VM_ARGS, Vars).


write_start_erl_data(ErtsVsn, Vsn) ->
    DataFile = filename:join("releases", "start_erl.data"),
    io:format(" => Create file: ~s~n", [DataFile]),
    ok = file:write_file(DataFile, io_lib:format("~s ~s~n", [ErtsVsn, Vsn])).


write_build_info(Vsn) ->
    {{Y,M,D},{H,N,_}} = calendar:universal_time(),
    io:format(" => Create file: ~s~n", [?BUILD_INFO]),
    Info = [<<"Build v">>, Vsn, " ",
            io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B.~n",
                          [Y, M, D, H, N])],
    ok = file:write_file(?BUILD_INFO, Info).


write_builderl_config(RelVsn, Cfg, LibDirs) ->
    File = filename:join(["releases", RelVsn, ?BUILDERL_CONFIG]),
    io:format(" => Create file: ~s~n", [File]),
    Terms = proplists:get_value(release_types, Cfg, []),
    Recs = [{node_type, A, B, C, D, E} || {A, B, C, D, E} <- Terms],
    SetupCfg = setup_cfg(Cfg, LibDirs) ++ get_location(Cfg),
    ToWrite = Recs ++ SetupCfg ++ def_nodes(Cfg) ++ def_joins(Cfg),
    bld_lib:write_terms(File, ToWrite).

setup_cfg(Cfg, LibDirs) ->
    case proplists:get_value(setup_config, Cfg) of
        undefined ->
            [];
        {CmdRel, SetupApp, SetupMod} ->
            check_setup_mod(SetupApp, SetupMod),
            {Dir, SetupVsn} = setup_app(LibDirs, SetupApp),
            [{setup_config, CmdRel, Dir, SetupApp, SetupVsn, SetupMod}]
    end.

check_setup_mod(undefined, SetupMod) when SetupMod =/= undefined ->
    Msg = "Error: setup module requires the setup application to be defined,"
        ++ "aborting.~n",
    io:format(Msg),
    halt(1);
check_setup_mod(_, _) ->
    ok.

get_location(Cfg) ->
    case lists:keyfind(install_location, 1, Cfg) of
        false -> [];
        {install_location, _} = Tuple -> [Tuple]
    end.


setup_app(_LibDirs, undefined) ->
    {undefined, undefined};
setup_app([Dir|T], SetupApp) ->
    Path = filename:join([Dir, SetupApp, "ebin"]),
    case filelib:is_dir(Path) of
        true -> setup_app_found(Dir, Path, SetupApp);
        false -> setup_app(T, SetupApp)
    end;
setup_app([], SetupApp) ->
    Msg = "Error: setup application '~s' not found, aborting.~n",
    io:format(Msg, [SetupApp]),
    halt(1).

setup_app_found(Dir, Path, SetupApp) ->
    AppFile = filename:join(Path, SetupApp) ++ ".app",
    [{application, SetupApp, List}] = bld_lib:consult_app_file(AppFile),
    {Dir, proplists:get_value(vsn, List)}.


def_nodes(Cfg) ->
    get_tuple_list(default_nodes, Cfg).

def_joins(Cfg) ->
    get_tuple_list(default_joins, Cfg).

get_tuple_list(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false -> [];
        Val -> [Val]
    end.

%%------------------------------------------------------------------------------

config_files(BldCfg) ->
    proplists:get_value(config_files, BldCfg, [?SYS_CONFIG_TMPL]).


link_configs(RelVsn, CfgFiles) ->
    lists:foreach(fun(X) -> link_configs1(RelVsn, X) end, CfgFiles).

link_configs1(RelVsn, Name) ->
    From = filename:join(["releases", RelVsn, Name]),
    bld_lib:rm_link(From),
    bld_lib:mk_link(filename:join(["..", "..", "etc", Name]), From).


cp_configs(RelDir, RelPath, CfgFiles) ->
    lists:foreach(fun(X) -> cp_configs1(RelDir, RelPath, X) end, CfgFiles).

cp_configs1(RelDir, RelPath, Name) -> bld_lib:cp_file(RelDir, RelPath, Name).

%%------------------------------------------------------------------------------

link_builderl(LinkPath, RelPath) ->
    Ebin = filename:join(LinkPath, "ebin"),
    To = filename:join(["..", "lib", bld_load:current_app_vsn(Ebin)]),
    bld_lib:mk_link(To, filename:join(RelPath, LinkPath)).

%%------------------------------------------------------------------------------

runtime_variables(BldCfg) ->
    StartErl = {start_erl, _} = lists:keyfind(start_erl, 1, BldCfg),
    RelDir = {rel_dir, _} = lists:keyfind(rel_dir, 1, BldCfg),
    HostName = {hostname, hostname()},
    IsDevT = {is_dev, IsDev = filelib:is_regular(?BUILD_INFO)},
    SetupCfg = setup_config(IsDev, lists:keyfind(setup_config, 1, BldCfg)),
    Nodes = [{{node, T}, {R, N, M, O}} || {node_type, T, R, N, M, O} <- BldCfg],
    Vars = [StartErl, RelDir, HostName, IsDevT] ++ SetupCfg ++ Nodes,
    io:format("Using runtime variables:~n~p~n", [Vars]),
    Vars.

get_release_name(Type, RunVars) ->
    element(1, proplists:get_value({node, Type}, RunVars)).

get_node_name(Type, RunVars) ->
    element(2, proplists:get_value({node, Type}, RunVars)).

get_node_name(Type, Suffix, RunVars) ->
    bld_lib:node_name(get_node_name(Type, RunVars), Suffix).

get_config_module(Type, RunVars) ->
    element(3, proplists:get_value({node, Type}, RunVars)).

%% Constant offset to add to the port number to ensure ports are unique.
%% When installing more nodes of the same type the port number
%% is the offset plus the sequential number starting from 0 for the given type
%% and in the order in which nodes were specified in the command line.
%% Increase the gap if installing more than 10 nodes of the same type.
get_port_offset(Type, RunVars) ->
    element(4, proplists:get_value({node, Type}, RunVars)).

get_cookie(Type, RunVars) ->
    get_node_name(Type, RunVars) ++ "_cookie".


setup_config(_IsDev, false) ->
    [];
setup_config(IsDev, {setup_config, Rel, Dir, App, Vsn, Mod}) ->
    Cfg = add_setup_app(add_setup_rel(Rel), App),
    add_setup_module(Cfg, IsDev, Dir, App, Vsn, Mod).

add_setup_rel(undefined) -> [];
add_setup_rel(Rel) -> [{setup_release, Rel}].

add_setup_app(L, undefined) -> L;
add_setup_app(L, App) -> [{setup_app, App} | L].

add_setup_module(L, _IsDev, _Dir, App, _Vsn, Mod)
  when App == undefined orelse Mod == undefined -> L;
add_setup_module(L, IsDev, Dir, App, Vsn, Mod) ->
    Path = setup_path(IsDev, Dir, App, Vsn),
    code:add_path(Path),
    [{setup_module, Mod} | L].

setup_path(true, Dir, App, _Vsn) ->
    filename:join([Dir, App, "ebin"]);
setup_path(false, _Dir, App, Vsn) ->
    filename:join(["lib", atom_to_list(App) ++ "-" ++ Vsn, "ebin"]).

hostname() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {hostent, Fqdn, _, _, _, _}} = inet:gethostbyname(Hostname),
    case is_fqdn(Hostname, Fqdn, string:chr(Fqdn, $.)) of
        false -> {local, Hostname};
        true -> {fqdn, Fqdn}
    end.

is_fqdn(_Host, _Fqdn, 0) -> false;
is_fqdn(_, undefined, _) -> false;
is_fqdn(Host, Fqdn, Idx) -> is_fqdn(Host, string:sub_string(Fqdn, 1, Idx - 1)).

is_fqdn(Hostname, Hostname) -> true;
is_fqdn(_, _) -> false.
