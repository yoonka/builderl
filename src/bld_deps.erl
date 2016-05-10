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

-module(bld_deps).

-include_lib("builderl/include/builderl.hrl").

-export([start/1]).

-define(DEPSDIR, <<"deps-versions">>).
-define(DEFAULTDEPSDIR, <<"lib">>).
-define(MASTER_BRANCH, "master").
-define(CMDS, [st, get, rm, mk, eunit]).

-define(ERL_EXT, erl).

usage() ->
    [
     "************************************************************************",
     "Routines to manipulate application dependencies defined in dependency",
     "files in the 'deps-versions' folder.",
     "",
     "Usage:",
     "  deps.esh [ -h | --help ]",
     "  deps.esh [ <cmd> | [ <cmd> ] ]",
     "           [ -d <branch> | -b <branch> | -u <url> | --verbose ]",
     "           [ -f ] [ -p <profile> ] [ -- ] [ <dep> | [ <dep> ] ]",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  <cmd>",
     "    Command or commands to execute. If multiple commands are specified",
     "    they are executed in the specified order on each dependency",
     "    separately (in parallel). Available commands:",
     "    " ++ all_cmds(),
     "   where:",
     "    st:    git status in a dependency",
     "    get:   git clone (or fetch if exists) a dependency",
     "    rm:    delete a dependency with rm -rf",
     "    mk:    compile a dependency",
     "    eunit: run eunit unit tests for a dependency",
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
     "",
     "  -f",
     "    Used with command 'rm'.",
     "    Force delete the dependency if the folder is dirty.",
     "",
     "  -p <profile>",
     "    Used with command 'mk'.",
     "    Adds compilation options from the specified profile to the end",
     "    of the list of options that will be passed to OTP compilers when",
     "    compiling files of the particular type. Initially that list contains",
     "    options from the 'default' profile, if it's defined. However, if the",
     "    'default' profile is defined explicitly with '-p' then the list of",
     "    options is initially empty and options are added to that list from",
     "    profiles in the order in which they are specified by '-p' arguments.",
     "    Profiles are defined in file 'etc/reltool.config' in section",
     "    'builderl' under configuration option 'make_profiles'.",
     "",
     "  --verbose",
     "    Prints out options used when executing the command.",
     "",
     "  --, <dep>",
     "    Any string that doesn't match one of the abovementioned options will",
     "    be interpreted as the last part of the dependency directory (which",
     "    is the last element in tuples defined in the dependency file).",
     "    When the optional '--' is provided, any string after '--' will be",
     "    interpreted as the last part of the dependency directory.",
     "",
     "    This option can be used to define a list of dependencies on which",
     "    the <cmd> commands will be executed. If this option is provided",
     "    dependencies not on this list will be ignored.",
     "************************************************************************"
    ].

all_cmds() -> string:join([atom_to_list(X) || X <- ?CMDS], ", ").

start(["-h"]) ->     bld_lib:print(usage());
start(["--help"]) -> bld_lib:print(usage());
start(Other) ->      io:format("~n"), start1(Other, []).

start1(["--"|T], Acc) ->
    start2(T, Acc);
start1(["-d" = Arg, Branch|T], Acc) ->
    start1(T, ensure_one(Arg, {default_branch, to_binary(Branch)}, Acc));
start1(["-b" = Arg, Branch|T], Acc) ->
    start1(T, ensure_one(Arg, {branch, to_binary(Branch)}, Acc));
start1(["-u" = Arg, Url|T], Acc) ->
    start1(T, ensure_one(Arg, {url, to_binary(Url)}, Acc));
start1(["-f"|T], Acc) ->
    start1(T, bld_lib:ensure_member(force, Acc));
start1(["-p", Profile|T], Acc) ->
    start1(T, [{profile, Profile}|Acc]);
start1(["--verbose" | T], Acc) ->
    start1(T, [verbose | Acc]);
start1([Cmd|T], Acc)
  when Cmd =:= "st"; Cmd =:= "get"; Cmd =:= "rm"; Cmd =:= "mk";
       Cmd =:= "eunit" ->
    start1(T, [{cmd, list_to_atom(Cmd)}|Acc]);
start1(Other, Acc) ->
    start2(Other, Acc).

start2([], Acc) -> do_start(ensure_url(lists:reverse(Acc)));
start2(List, Acc) -> start2([], [{dirs, List} | Acc]).

to_binary(undefined) -> undefined;
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(Atom) when is_atom(Atom) -> list_to_binary(atom_to_list(Atom));
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

add_url(OrgOptions) ->
    {Options, Config} = get_builderl_cfg(OrgOptions),
    EnvRepoBase = proplists:get_value(env_repo_base, Config),
    RepoBase = proplists:get_value(default_repo_base, Config),
    Dummy = "default_repo_base not set in the builderl config section "
        "in 'etc/reltool.config'!!!",
    case {RepoBase, EnvRepoBase =/= undefined andalso os:getenv(EnvRepoBase)} of
        {undefined, false} -> add_url(Dummy, Options);
        {_, false} -> add_url(RepoBase, Options);
        {_, Val} -> add_url(Val, Options)
    end.

add_url(RepoBase, Options) -> [{url, to_binary(RepoBase)}|Options].

get_builderl_cfg(Options) ->
    case proplists:get_value(builderl_cfg, Options) of
        undefined ->
            File = bld_rel:get_reltool_config(),
            Config = proplists:get_value(builderl, File, []),
            {[{builderl_cfg, Config} | Options], Config};
        Config ->
            {Options, Config}
    end.

%%------------------------------------------------------------------------------

do_start(Opts0) ->
    not lists:member(verbose, Opts0) orelse
        io:format("Using options: ~p~n~n", [Opts0]),
    bld_cmd:is_cmd(<<"git">>) orelse halt_no_git(),
    Cmds = [X || {cmd, X} <- Opts0],
    length(Cmds) > 0 orelse halt_no_cmd(),

    Opts1 = process_make_profiles(lists:member(mk, Cmds), Opts0),
    Opts2 = process_test_options(lists:member(eunit, Cmds), Opts1),
    {Opts3, Repos, Deps} = process_deps(Opts2),

    bld_make:start_serializer(),
    CmdsTxt = string:join([atom_to_list(X) || X <- Cmds], "; "),
    DirsTxt = string:join(Repos, " "),
    io:format("=== Executing: '~s' in repositories: ~s~n", [CmdsTxt, DirsTxt]),
    Fun = fun(X) -> execute(Cmds, X, Opts3) end,
    Res0 = lists:flatten(bld_lib:call(Fun, Deps)),
    Res1 = [X || {Cmd, _} = X <- Res0, Cmd =/= st],
    bld_make:stop_serializer(),

    case lists:keymember(error, 2, Res1) of
        false ->
            io:format("=== All finished, result OK.~n");
        true ->
            io:format("=== All finished but there were errors! "
                      "Please check the output. ===~n"),
            halt(1)
    end.

halt_no_git() -> bld_lib:print(err_nogit()), halt(1).

err_nogit() ->
    [
     "Error, git command couldn't be found.",
     "Please ensure that git is installed and its executable is available",
     "in one of the paths specified in the PATH environment variable.",
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

process_make_profiles(IsMK, Opts) ->
    combine_profiles(IsMK, Opts, [list_to_atom(X) || {profile, X} <- Opts]).

combine_profiles(false, Opts, []) ->
    Opts;
combine_profiles(IsMK, OldOpts, Profiles) ->
    {Opts, Config} = get_builderl_cfg(OldOpts),
    MKProfiles = proplists:get_value(make_profiles, Config, []),
    Acc0 = get_default_profile(lists:member(default, Profiles), MKProfiles),
    Fun = fun(X, AccIn) -> add_profile(X, AccIn, MKProfiles) end,
    Combined = lists:foldl(Fun, Acc0, Profiles),
    if IsMK =:= false -> [{combined_profile, Combined}|Opts];
       true -> [{combined_profile, init_make(Combined)}|Opts] end.

process_test_options(false, Opts) ->
    Opts;
process_test_options(true, OldOpts) ->
    {Opts, Cfg} = get_builderl_cfg(OldOpts),
    TestOpts = proplists:get_value(test_options, Cfg, []),
    EUOpts = lists:filter(fun do_eunit_opts/1, TestOpts),
    [{eunit_options, lists:filter(fun do_eunit_opts/1, EUOpts)}|Opts].

do_eunit_opts({pa, Path}) ->
    code:add_patha(Path) =:= true orelse halt_eunitpa(Path),
    false;
do_eunit_opts(_) ->
    true.

halt_eunitpa(Path) -> bld_lib:print(err_eunitpa(Path)), halt(1).

err_eunitpa(Path) ->
    ["Error, path '" ++ Path ++ "' specified in 'test_options' in the "
     "'etc/reltool.config' file doesn't exist."].

get_default_profile(false, MKProfiles) ->
    proplists:get_value(default, MKProfiles, []);
get_default_profile(true, _) ->
    [].

add_profile(X, AccIn, MKProfiles) ->
    Res = proplists:get_value(X, MKProfiles),
    Res =/= undefined orelse halt_no_profile(X),
    lists:foldl(fun add_options/2, AccIn, Res).

halt_no_profile(Profile) -> bld_lib:print(err_noprofile(Profile)), halt(1).

err_noprofile(Profile) ->
    ["Error, profile '" ++ atom_to_list(Profile) ++ "' is not defined in the"
     ++ bld_section()].

bld_section() ->
    " 'make_profiles' section of 'builderl' config in the 'etc/reltool.config'"
        " file.".

add_options({Type, Opts} = Profile, AccIn)
  when Type =:= make_options; Type =:= ?ERL_EXT ->
    case proplists:get_value(Type, AccIn) of
        undefined -> [Profile|AccIn];
        List -> lists:keyreplace(Type, 1, AccIn, {Type, List ++ Opts})
    end;
add_options({Type, _}, _) ->
    bld_lib:print(err_unknownopts(Type)), halt(1).

err_unknownopts(Type) ->
    ["Error, unknown type of options '" ++ atom_to_list(Type) ++ "' specified "
     "in the" ++ bld_section()].

init_make(Combined) ->
    io:format("Using compilation options:~n"),
    PFun = fun({T, O}) -> io:format("~s: ~p~n", [T, O]) end,
    lists:foreach(PFun, Combined),
    io:format("~n"),

    MakeOpts = proplists:get_value(make_options, Combined, []),
    MFun = fun(X, Acc) -> do_mk_option(X, Acc, MakeOpts) end,
    case lists:foldl(MFun, [], MakeOpts) of
        [] ->
            Combined;
        AddOpts ->
            Combined2 = lists:keydelete(make_options, 1, Combined),
            [{make_options, MakeOpts ++ AddOpts} | Combined2]
    end.

do_mk_option({pa, Path}, Acc, _MakeOpts) ->
    true = code:add_patha(Path),
    Acc;
do_mk_option({mk_plugin, Src}, Acc, MakeOpts) ->
    {Module, Binary} = erl_compile(Src),
    case code:load_binary(Module, Module, Binary) of
        {module, Mod} ->
            io:format("Loaded.~n~n"),
            [{mk_plugin_info, Mod, Mod:init(MakeOpts)}|Acc];
        {error, _} = Err ->
            Msg = "Error when loading module '~p': ~p~nAborting.~n",
            io:format(Msg, [Module, Err]),
            halt(1)
    end;
do_mk_option(_, Acc, _MakeOpts) ->
    Acc.

erl_compile(Src) ->
    io:format("Compiling and loading '~s.erl':~n", [Src]),
    case compile:file(Src,  [verbose, binary, report, {i, "lib"}]) of
        {ok, ModuleName, Binary} -> {ModuleName, Binary};
        {ok, ModuleName, Binary, _Warnings} -> {ModuleName, Binary};
        {error, _Err, _Warn} -> halt_aborting();
        error -> halt_aborting()
    end.

halt_aborting() ->
    io:format("Aborting due to errors!~n"), halt(1).

%%------------------------------------------------------------------------------

process_deps(OrgOptions) ->
    Default = proplists:get_value(default_branch, OrgOptions),
    Branch = proplists:get_value(branch, OrgOptions),
    {Options, Deps0} = read_deps(Default, Branch, OrgOptions),

    Url = proplists:get_value(url, Options),
    {ok, MP} = re:compile(<<"=REPOBASE=">>),
    Args = [global, {return, binary}],
    {Repos, Deps1} = get_repos(proplists:get_value(dirs, Options), Deps0),
    ReFun = fun(undefined) -> undefined;
               (Z) -> re:replace(Z, MP, Url, Args) end,
    {Options, Repos, [{X, Y, ReFun(Z)} || {X, Y, Z} <- Deps1]}.

get_repos(undefined, Deps) ->
    lists:unzip(Deps);
get_repos(Dirs, Deps) ->
    get_repos(Dirs, Deps, {[], []}, []).

get_repos([Key|T], Deps, {R, D} = Good, Bad) ->
    case lists:keyfind(Key, 1, Deps) of
        {X, Y} -> get_repos(T, Deps, {[X|R], [Y|D]}, Bad);
        false -> get_repos(T, Deps, Good, [Key|Bad])
    end;
get_repos([], _Deps, Good, []) ->
    Good;
get_repos([], _Deps, _, Bad) ->
    halt_no_repositories(length(Bad), string:join(Bad, "', '")).

halt_no_repositories(Length, Dirs) ->
    bld_lib:print(err_norepositories(Length, Dirs)),
    halt(1).

err_norepositories(Length, Dirs) ->
    [
     "Error, " ++ repo(Length, Dirs) ++ " exist in the dependency "
     "file read from the 'deps-versions' folder.\n"
    ].

repo(1, Dirs) -> "repository: '" ++ Dirs ++ "' doesn't";
repo(_, Dirs) -> "repositories: '" ++ Dirs ++ "' don't".

read_deps(Default, undefined, Options) ->
    case {bld_cmd:git_branch(<<".">>), Default} of
        {{0, Branch}, _} -> {Options, read_deps_file1(Branch)};
        {_, undefined} -> try_default_branch(Options);
        {_, Branch} -> {Options, read_deps_file2(Branch)}
    end;
read_deps(_, Force, Options) ->
    {Options, read_deps_file4(Force)}.

read_deps_file1(Branch) ->
    bld_lib:h_line("=== " ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

read_deps_file2(Branch) ->
    bld_lib:h_line("=== invalid branch, using provided: "
                   ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

read_deps_file3(Branch) ->
    bld_lib:h_line("=== invalid branch, using default: "
                   ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

read_deps_file4(Branch) ->
    bld_lib:h_line("=== force-using: " ++ binary_to_list(Branch) ++ " ", $=),
    read_deps_file(Branch).

try_default_branch(OrgOptions) ->
    {Options, Config} = get_builderl_cfg(OrgOptions),
    case proplists:get_value(default_branch, Config) of
        undefined -> halt_no_branch();
        Branch -> {Options, read_deps_file3(to_binary(Branch))}
    end.

read_deps_file(Branch) ->
    DepsFile = filename:join(?DEPSDIR, Branch),
    Msg = "Trying to read deps file ~p: ",
    io:format(standard_io, Msg, [binary_to_list(DepsFile)]),
    case file:consult(DepsFile) of
        {ok, UserCfg} ->
            io:format(standard_io, "OK~n", []),
            bld_lib:h_line("=", $=),
            io:format("~n"),
            {ok, MP} = re:compile(<<"(\\s+)">>),
            [normalize_deps(MP, X) || X <- UserCfg];
        {error, Err} ->
            io:format(standard_io, "Error: ~p, aborting.~n", [Err]),
            halt(1)
    end.

normalize_deps(MP, {Tag, Cmd, AppDir}) ->
    normalize_deps(MP, ?DEFAULTDEPSDIR, Tag, Cmd, AppDir);
normalize_deps(MP, {Dir, Tag, Cmd, AppDir}) ->
    normalize_deps(MP, Dir, Tag, Cmd, AppDir);
normalize_deps(MP, AppDir) when is_list(AppDir), is_integer(hd(AppDir)) ->
    normalize_deps(MP, ?DEFAULTDEPSDIR, undefined, undefined, AppDir).

normalize_deps(MP, Dir, Tag, Cmd, AppDir) ->
    Path = to_binary(filename:join(Dir, AppDir)),
    {AppDir, {Path, to_binary(Tag), compact(MP, Cmd)}}.

compact(_MP, undefined) -> undefined;
compact(MP, What) -> re:replace(What, MP, " ", [global, {return, list}]).

halt_no_branch() -> bld_lib:print(err_nobranch()), halt(1).

err_nobranch() ->
    [
     "Error, couldn't determine git branch in the current directory",
     "and the default branch hasn't been specified. Please add the default",
     "branch to the builderl config section in etc/reltool.config, e.g.:",
     "{default_branch, \"master\"},",
     "or specify the branch with either -b or -d options.",
     "Use -h or --help for more information about options."
    ].

%%------------------------------------------------------------------------------

execute(Cmds, {Path, Tag, Clone}, Options) ->
    Fun = fun(st)    -> execute_st(Path, Clone);
             (get)   -> execute_get(Path, Tag, Clone);
             (rm)    -> execute_rm(Path, Options);
             (mk)    -> execute_mk(Path, Options);
             (eunit) -> execute_eunit(Path, Options)
          end,
    lists:map(fun(X) -> {X, print_result(Fun(X))} end, Cmds).

print_result({ok, Lines}) -> io:format(standard_io, Lines, []);
print_result({error, Lines}) -> io:format(standard_error, Lines, []), error.

format_error(Path, Err) -> format_error(Path, Err, []).

format_error(Path, {0, List}, L) -> format_error(dirty, Path, List, L);
format_error(Path, {_, List}, L) -> format_error(error, Path, List, L).

format_error(Type, Path, List, L) ->
    format_error1(Type, Path, List) ++ L ++ [<<"\n<--\n">>].

format_error1(dirty, Path, L) ->
    [<<"==> ! local changes: ">>, Path, <<"\n ">> | bin_join(L, <<"\n ">>, [])];
format_error1(error, Path, L) ->
    [<<"==> !! error: ">>, Path, <<"\n">> | bin_join(L, <<"\n">>, [])].

bin_join([Line], _, Acc) -> lists:reverse([Line|Acc]);
bin_join([Line|T], Sep, Acc) -> bin_join(T, Sep, [Sep, Line|Acc]);
bin_join([], _, Acc) -> Acc.

not_a_directory(Path) ->
    {ok, [<<"not a directory, ignoring: ">>, Path, <<"\n">>]}.

no_repository(Path) ->
    {ok, [<<"repository not provided, ignoring: ">>, Path, <<"\n">>]}.

%%------------------------------------------------------------------------------

execute_st(Path, undefined) -> no_repository(Path);
execute_st(Path, _Clone) -> format_status(Path, bld_cmd:git_status(Path)).

format_status(Path, false) -> not_a_directory(Path);
format_status(Path, {0, []}) -> {ok, [<<"clean: ">>, Path, <<"\n">>]};
format_status(Path, Err) -> {error, format_error(Path, Err)}.

%%------------------------------------------------------------------------------

execute_get(Path, _Tag, undefined) ->
    no_repository(Path);
execute_get(Path, undefined, Clone) ->
    execute_get(Path, ?MASTER_BRANCH, Clone, bld_cmd:git_status(Path));
execute_get(Path, Tag, Clone) ->
    execute_get(Path, Tag, Clone, bld_cmd:git_status(Path)).

execute_get(Path, Tag, Clone, false) ->
    format_get(Path, bld_cmd:git_clone(Path, Tag, Clone));
execute_get(Path, _, _, {0, []}) ->
    {error, [<<"already exists, ignoring: ">>, Path, <<"\n">>]};
execute_get(Path, _, _, Err) ->
    {error, format_error(Path, Err, [<<"\n---\nnot clean, ignoring...">>])}.

format_get(Path, {0, _}) -> {ok, [<<"cloned: ">>, Path, <<"\n">>]};
format_get(Path, {_, List}) -> {error, format_error(error, Path, List, [])}.

%%------------------------------------------------------------------------------

execute_rm(Path, Options) ->
    Force = lists:member(force, Options),
    execute_rm(Path, Force, bld_cmd:git_status(Path)).

execute_rm(Path, _, false) ->
    not_a_directory(Path);
execute_rm(Path, _, {0, []}) ->
    case format_rm(Path, false, do_rm(Path)) of
        {ok, _} = RetOK -> RetOK;
        {error, Lines} -> {error, format_error(error, Path, Lines, [])}
    end;
execute_rm(Path, false, Err) ->
    {error, format_error(Path, Err)};
execute_rm(Path, true, Err) ->
    LineSep = <<"\n---\n">>,
    case format_rm(Path, true, do_rm(Path)) of
        {ok, Lines} -> {ok, format_error(Path, Err, [LineSep|Lines])};
        {error, Lines} -> {error, format_error(Path, Err, [LineSep|Lines])}
    end.

do_rm(Path) -> bld_cmd:rm_rf(Path).

format_rm(Path, false, {0, []}) -> {ok, [<<"deleted: ">>, Path, <<"\n">>]};
format_rm(Path, true, {0, []}) -> {ok, [<<"force-deleted: ">>, Path]};
format_rm(_, _, {_, L}) -> {error, bin_join(L, <<"\n">>, [])}.

%%------------------------------------------------------------------------------

execute_mk(OPath, Opts) ->
    Path = binary_to_list(OPath),
    Combined = proplists:get_value(combined_profile, Opts, []),
    bld_make:compile(Path, Combined).

%%------------------------------------------------------------------------------

execute_eunit(Path, Options) ->
    DstPath = bld_make:get_dst_path(binary_to_list(Path)),
    true = code:add_patha(DstPath),
    Modules = [list_to_atom(filename:rootname(X)) ||
                  X <- filelib:wildcard("*.beam", DstPath),
                  not lists:suffix("_tests.beam", X)],
    EUOpts = proplists:get_value(eunit_options, Options, []),
    lists:foreach(fun(X) -> run_test(X, EUOpts) end, Modules),
    {ok, [<<"  => Finished in ">>, Path, <<"\n">>]}.

run_test(Mod, Opts) ->
    case eunit:test(Mod, Opts) of
        ok -> ok;
        {error, Err} -> test_error(Mod, Opts, Err)
    end.

test_error(Mod, Opts, Err) ->
    M = "Error when executing eunit tests in module ~s with options ~p:~n~p~n",
    io:format(M, [Mod, Opts, Err]).
