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

-export([
         read_config/1,
         merge_config/2,
         runtime_variables/0,
         check_release/0,
         check_not_running/1,
         check_folder/2,
         create_start_scripts/3,
         install_cookie/3,
         read_cookies/1,
         start_nodes/1,
         stop_nodes/1,
         wait_for_nodes/1,
         connect_to_started/1,
         node_package_setup/3
        ]).

-define(CMDSH, fun(NodeName) -> NodeName ++ ".cmd.sh" end).
-define(TIMEOUT, 15000).

-define(RELCHECK,
        [{dir, <<"lib">>},
         {file, <<"releases/start_erl.data">>}
        ]).

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


merge_config(BaseCfg, [{Key, Val} = Elem | T]) when is_list(BaseCfg) ->
    case proplists:lookup(Key, BaseCfg) of
        none ->
            merge_config([Elem | BaseCfg], T);
        {Key, OrgVal} ->
            NewElem = {Key, merge_config(OrgVal, Val)},
            NewCfg = lists:keyreplace(Key, 1, BaseCfg, NewElem),
            merge_config(NewCfg, T)
    end;
merge_config(BaseCfg, []) ->
    BaseCfg;
merge_config(_OldElem, NewElem) ->
    NewElem.

%%------------------------------------------------------------------------------

runtime_variables() ->
    {_, Vsn} = StartErl = get_rel_vsn(),
    RelDir = filename:join("releases", Vsn),
    Hostname = hostname(),
    NodeTypes = bld_lib:read_builderl_config(RelDir),
    Vars = [
            {start_erl, StartErl},
            {rel_dir, RelDir},
            {hostname, Hostname}
           ] ++ NodeTypes,
    io:format("~nUsing runtime variables:~n~p~n", [Vars]),
    Vars.

get_rel_vsn() ->
    {ok, Data} = file:read_file("releases/start_erl.data"),
    [ErtsVsn, VsnBin] = binary:split(Data, <<" ">>),
    {bld_lib:trim(ErtsVsn), bld_lib:trim(VsnBin)}.

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

%%------------------------------------------------------------------------------

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

create_start_scripts(Rel, Name, Base) ->
    Path = erl_path(filelib:is_regular(?BUILD_INFO)),
    create_start_script(Path, Base, "cmd", Name, "vm.cmd.args", ?CMDSH(Name)),
    create_start_script(Path, Base, Rel, Name, "vm.args", Name ++ ".sh").

create_start_script(Path, Base, Rel, CfgRel, ArgsFile, StartFile) ->
    Bytes = cmd_sh(Path, Base, Rel, CfgRel, ArgsFile),
    File = filename:join([Base, "bin", StartFile]),

    io:format(standard_io, "Write start script '~s': ", [File]),
    bld_lib:check_file_op(file:write_file(File, Bytes)),
    bld_lib:chmod_exe(File).

%% ?BUILD_INFO is created by mk_dev.esh and only present in the dev environment
erl_path(true) -> "";
erl_path(false) -> "./bin/".

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

%%------------------------------------------------------------------------------

install_cookie(Type, Base, RunVars) ->
    File = filename:join(Base, ".erlang.cookie"),
    Cookie = bld_lib:get_node_name(Type, RunVars) ++ "_cookie",
    io:format(standard_io, "Write cookie '~s': ", [File]),
    bld_lib:check_file_op(file:write_file(File, Cookie ++ "\n")),
    Cookie.


read_cookies(Names) ->
    case rpc:multicall(Names, erlang, get_cookie, []) of
        {Cookies, []} ->
            Cookies;
        {_, BadNames} ->
            Msg = "Error, couldn't read cookies from nodes: ~p~n.",
            io:format(standard_error, Msg, [BadNames]),
            halt(1)
    end.


start_nodes(Names) ->
    io:format(standard_io, "~n => Starting nodes...~n~n", []),
    lists:foreach(fun(Node) -> bld_lib:start_node(Node, ?CMDSH) end, Names).


stop_nodes(Names) ->
    io:format(standard_io, "~n => Stopping installed nodes...~n", []),
    lists:foreach(fun(R) -> stop_node(R) end, [R || {R, _T, _S} <- Names]).

stop_node(Remote) ->
    io:format(standard_io, "Stopping node '~s'...~n", [Remote]),
    Res2 = rpc:call(Remote, init, stop, [], 5000),
    io:format("Result: ~p~n", [Res2]).


connect_with_name(Remote) ->
    ping_to_bool(net_adm:ping(Remote)).

ping_to_bool(pang) -> false;
ping_to_bool(pong) -> true.

%%------------------------------------------------------------------------------

wait_for_nodes(Nodes) ->
    io:format(standard_io, " => Waiting for nodes...~n~n", []),
    wait_for_nodes(Nodes, ?TIMEOUT, os:timestamp(), []).


connect_to_started([]) ->
    ok;
connect_to_started(Added) ->
    StartedMsg = "~n => Connecting to already started nodes...~n~n",
    io:format(standard_io, StartedMsg, []),
    Nodes = wait_for_nodes(Added, ?TIMEOUT, os:timestamp(), []),
    Added = lists:usort(Nodes).

wait_for_nodes([{_, _, _, _, Base} = Opt | T], Timeout, Start, Acc) ->
    {Res, Remote} = bld_lib:connect_to_node(Base),
    TimeDiff = get_time_diff(Start),
    wait_for_nodes(Res, Remote, Opt, T, Timeout, Start, TimeDiff, Acc);
wait_for_nodes([Remote | T], Timeout, Start, Acc) when is_atom(Remote) ->
    Res = connect_with_name(Remote),
    TimeDiff = get_time_diff(Start),
    wait_for_nodes(Res, Remote, Remote, T, Timeout, Start, TimeDiff, Acc);
wait_for_nodes([], _Timeout, _Start, Acc) ->
    lists:reverse(Acc).

wait_for_nodes(true, Remote, Opt, T, Timeout, Start, _TimeDiff, Acc) ->
    io:format(standard_io, "Connected to '~s'.~n", [Remote]),
    wait_for_nodes(T, Timeout, Start, [new_opt(Opt, Remote) | Acc]);
wait_for_nodes(false, Remote, Opt, T, Timeout, Start, TimeDiff, Acc)
  when TimeDiff < Timeout ->
    timer:sleep(500),
    Res = connect_with_name(Remote),
    NewTimeDiff = get_time_diff(Start),
    wait_for_nodes(Res, Remote, Opt, T, Timeout, Start, NewTimeDiff, Acc);
wait_for_nodes(false, Remote, _Opt, _T, _Timeout, _Start, _TimeDiff, _Acc) ->
    Msg = "Time out error when waiting for node '~s', aborting...~n",
    io:format(standard_error, Msg, [Remote]),
    halt(1).

get_time_diff(Start) ->
    timer:now_diff(os:timestamp(), Start) div 1000.

new_opt({Action, Type, Suffix, _Seq, _Base}, Remote) ->
    {Action, Remote, Type, Suffix};
new_opt(Remote, Remote) ->
    Remote.

%%------------------------------------------------------------------------------

node_package_setup(ErtsVsn, Base, Args) ->
    SrcDir = filename:join("erts-" ++ ErtsVsn, "bin"),

    RunnerDst = filename:join([Base, "bin", "runner.src"]),
    bld_lib:process_file(filename:join(SrcDir, "runner"), RunnerDst, []),

    EnvDst = filename:join([Base, "bin", "env.sh.src"]),
    bld_lib:process_file(filename:join(SrcDir, "env.sh"), EnvDst, Args),

    SrvDst = filename:join([Base, "etc", "init.d", "daemon.src"]),
    SrvSrc = filename:join(["etc", "init.d", "daemon.src"]),
    bld_lib:process_file(SrvSrc, SrvDst, Args).
