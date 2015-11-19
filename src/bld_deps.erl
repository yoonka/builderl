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

-module(bld_deps).

-include_lib("builderl/include/builderl.hrl").

-export([start/1]).

-define(DEPSDIR, <<"deps-versions">>).
-define(CMDS, [get, rm]).
-define(DEFAULTDEPSDIR, <<"lib">>).

usage() ->
    [
     "************************************************************************",
     "Routines to manipulate application dependencies.",
     "",
     "Usage:",
     "  deps.esh [ -v | -h | --help ]",
     "  deps.esh [ <cmd> | -d <branch> | -b <branch> | -u <url> ]",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  <cmd>",
     "    Command to execute. Available commands:",
     "    " ++ all_cmds(),
     "",
     "  -d <branch>",
     "    Default branch to use if the current directory is not a git",
     "    repository or the repository is in detached head state.",
     "",
     "  -b <branch>",
     "    Use the specified branch when reading the dependency file.",
     "",
     "  -u <url>",
     "    Prefix to use when constructing urls of repositories. The value",
     "    provided in this argument will replace variable =REPOBASE= in",
     "    entries in the dependency file.",
     "    If this option is not provided then it's read from an environment",
     "    variable. The name of that environment variable is read from another",
     "    environment variable: ENV_REPO_BASE. This allows to specify",
     "    different prefix urls for different projects on the same host.",
     "************************************************************************"
    ].

all_cmds() -> string:join([atom_to_list(X) || X <- ?CMDS], ", ").

start(["-h"]) ->     bld_lib:print(usage());
start(["--help"]) -> bld_lib:print(usage());
start(Other) ->      io:format("~n"), start1(Other, []).

start1(["-d" = Arg, Branch|T], Acc) ->
    start1(T, ensure_one(Arg, {default_branch, to_binary(Branch)}, Acc));
start1(["-b" = Arg, Branch|T], Acc) ->
    start1(T, ensure_one(Arg, {branch, to_binary(Branch)}, Acc));
start1(["-u" = Arg, Url|T], Acc) ->
    start1(T, ensure_one(Arg, {url, to_binary(Url)}, Acc));
start1([Cmd|T], Acc) when Cmd =:= "get"; Cmd =:= "rm" ->
    start1(T, [{cmd, list_to_atom(Cmd)}|Acc]);
start1([], Acc) ->
    do_start(ensure_url(lists:reverse(Acc)));
start1(Other, _Acc) ->
    bld_lib:halt_badarg(Other).

to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) -> list_to_binary(List).

ensure_one(Arg, Tuple, Acc) ->
    case proplists:get_value(element(1, Tuple), Acc) of
        undefined -> [Tuple|Acc];
        _ -> halt_multiple_args(Arg)
    end.

halt_multiple_args(Arg) ->
    Msg = "Error, argument '~s' specified multiple times. Aborting",
    io:format(standard_error, Msg, [Arg]),
    halt(1).

ensure_url(Options) ->
    case lists:keymember(url, 1, Options) of
        true -> Options;
        false -> add_url(Options)
    end.

add_url(Options) ->
    File = bld_rel:get_reltool_config(),
    Config = proplists:get_value(builderl, File),
    EnvRepoBase = proplists:get_value(env_repo_base, Config),
    RepoBase = proplists:get_value(default_repo_base, Config),
    Dummy = "default_repo_base not set in builderl config section!!!",
    case {RepoBase, EnvRepoBase =/= undefined andalso os:getenv(EnvRepoBase)} of
        {undefined, false} -> add_url(Dummy, Options);
        {_, false} -> add_url(RepoBase, Options);
        {_, Val} -> add_url(Val, Options)
    end.

add_url(RepoBase, Options) -> [{url, to_binary(RepoBase)}|Options].

%%------------------------------------------------------------------------------

do_start(Options) ->
    bld_cmd:is_cmd(<<"git">>) orelse halt_no_git(),
    length([X || {cmd, X} <- Options]) > 0 orelse halt_no_cmd(),
    io:format("Using options: ~p~n~n", [Options]),

    Default = proplists:get_value(default_branch, Options),
    DepsOrg = read_deps(Default, proplists:get_value(branch, Options)),
    Url = proplists:get_value(url, Options),
    Cmds = [X || {cmd, X} <- Options],
    {ok, MP} = re:compile(<<"=REPOBASE=">>),
    Args = [global, {return, binary}],
    Deps = [{X, re:replace(Y, MP, Url, Args)} || {X, Y} <- DepsOrg],

    Fun = fun(X) -> execute(Cmds, X) end,
    Res = bld_lib:call(Fun, Deps),
    io:format("All finished, result: ~p~n", [lists:usort(Res)]).

halt_no_git() -> bld_lib:print(err_nogit()), halt(1).

err_nogit() ->
    [
     "Error, git command couldn't be found.",
     "Please ensure that git is installed and its executable available in one",
     "of the paths specified in the PATH environment variable.",
     "Use -h or --help for more information about options."
    ].

halt_no_cmd() -> bld_lib:print(err_nocmd()), halt(1).

err_nocmd() ->
    [
     "Error, command to execute has not been specified.",
     "Please use one of the following commands:",
     all_cmds(),
     "Use -h or --help for more information about options."
    ].

read_deps(Default, undefined) ->
    case {bld_cmd:git_branch(<<".">>), Default} of
        {{0, Branch}, _} -> read_deps_file1(Branch);
        {_, undefined} -> halt_no_branch();
        {_, Branch} -> read_deps_file2(Branch)
    end;
read_deps(_, Force) ->
    read_deps_file3(Force).

read_deps_file1(Branch) ->
    bld_lib:h_line("===" ++ binary_to_list(Branch), $=),
    read_deps_file(Branch).

read_deps_file2(Branch) ->
    bld_lib:h_line("=== invalid branch, using: "
                   ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

read_deps_file3(Branch) ->
    bld_lib:h_line("=== force-using: " ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

read_deps_file(Branch) ->
    DepsFile = filename:join(?DEPSDIR, Branch),
    Msg = "Trying to read deps file ~p: ",
    io:format(standard_io, Msg, [binary_to_list(DepsFile)]),
    case file:consult(DepsFile) of
        {ok, UserCfg} ->
            io:format(standard_io, "OK~n", []),
            bld_lib:h_line("=", $=),
            {ok, MP} = re:compile(<<"(\\s+)">>),
            [normalize_deps(MP, X) || X <- UserCfg];
        {error, Err} ->
            io:format(standard_io, "Error: ~p, aborting.~n", [Err]),
            halt(1)
    end.

normalize_deps(MP, {Cmd, AppDir}) ->
    normalize_deps(MP, ?DEFAULTDEPSDIR, Cmd, AppDir);
normalize_deps(MP, {Dir, Cmd, AppDir}) ->
    normalize_deps(MP, Dir, Cmd, AppDir).

normalize_deps(MP, Dir, Cmd, AppDir) ->
    {to_binary(filename:join(Dir, AppDir)), compact(MP, Cmd)}.

compact(MP, What) -> re:replace(What, MP, " ", [global, {return, list}]).

halt_no_branch() -> bld_lib:print(err_nobranch()), halt(1).

err_nobranch() ->
    [
     "Error, couldn't determine git branch in the current directory",
     "and the default branch hasn't been specified. Aborting..."
    ].

%%------------------------------------------------------------------------------

execute(Cmds, Dep) ->
    io:format("Execute ~p for ~p~n", [Cmds, Dep]).
