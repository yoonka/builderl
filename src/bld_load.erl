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

-module(bld_load).

-export([boot/3, compile/3, current_app_vsn/1]).

-export([
         builderl/1,
         update_root_dir/1,
         configure/1,
         migresia/1,
         mk_dev/1,
         mk_rel/1,
         init/1,
         config/1,
         start/1,
         stop/1,
         deps/1
        ]).

-include_lib("kernel/include/file.hrl").
-include("../include/load_builderl.hrl").

-define(BUILDERLAPP, "builderl.app").
-define(SKEL_DIR, ["priv", "skel"]).
-define(BLD_APP_MOD_RE, {builderl_generated}).

-define(DEL_LINKS, ["config", "configure", "migresia", "deps", "init", "mk_dev",
                    "mk_rel", "start", "stop", "update_root_dir"]).
-define(BLD_LINKS, ?DEL_LINKS).

boot(SrcPath, DstPath, Root) ->
    Opts = [{i, Root}],
    compile_src(SrcPath, DstPath, Opts, true).

compile(SrcPath, DstPath, _Options) ->
    compile_src(SrcPath, DstPath, [{i, "lib"}], false).

current_app_vsn(Path) ->
    File = filename:join(Path, ?BUILDERLAPP),
    [{application, builderl, List}] = bld_lib:consult_app_file(File),
    "builderl-" ++ proplists:get_value(vsn, List).

%%------------------------------------------------------------------------------

builderl(Args)        -> do_builderl(Args).
update_root_dir(Args) -> bld_cfg:set_root(Args).
configure(Args)       -> bld_cfg:configure(Args).
migresia(Args)        -> bld_cfg:migresia(Args).
mk_dev([])            -> bld_rel:mk_dev().
mk_rel([])            -> bld_rel:mk_rel(?BUILDERLLINK).
init(Args)            -> bld_init:do(Args).
config(Args)          -> bld_cfg:config(Args).
start(Args)           -> bld_run:start(Args).
stop(Args)            -> bld_run:stop(Args).
deps(Args)            -> bld_deps:start(Args).

%%------------------------------------------------------------------------------

compile_src(SrcPath, DstPath, Opts, Load) ->
    try
        compile_src1(SrcPath, DstPath, Opts, Load),
        {ok, [<<"  => Finished compiling in ">>, SrcPath, <<"\n">>]}
    catch
        throw:Err ->
            if Load =:= false -> {error, handle_err(Err)};
               true -> halt(1) end
    end.

compile_src1(SrcPath, DstPath, Opts, Load) ->
    ensure_dir(DstPath),
    Srcs = list_files(SrcPath, ".erl"),
    Beams = list_files(DstPath, ".beam"),
    MaxMTime = get_max_mtime([X || {_, X} <- Srcs]),

    IsDel = compile_modules(SrcPath, DstPath, Srcs, Beams, Opts, false),
    Modules = list_modules(DstPath),
    Load =:= false orelse load_modules(DstPath, Modules),

    case list_files(SrcPath, ".app.src") of
        [App] -> process_app(SrcPath, DstPath, MaxMTime, IsDel, Modules, App);
        [] -> {error, {no_app_src_file, SrcPath}};
        _ -> {error, {multiple_app_src_files, SrcPath}}
    end.

list_file(Path, File) ->
    FileName = filename:join(Path, File),
    case file:read_file_info(FileName, [{time, posix}]) of
        {ok, Info} -> {FileName, Info#file_info.mtime};
        {error, enoent} -> {FileName, undefined}
    end.

list_files(Path, Ext) ->
    All = filelib:wildcard("*" ++ Ext, Path),
    Info = [{filename:basename(X, Ext), get_mtime(Path, X)} || X <- All],
    Sort = fun({A, _}, {B, _}) -> A =< B end,
    lists:sort(Sort, Info).

get_mtime(Path, File) ->
    FileName = filename:join(Path, File),
    {ok, Info} = file:read_file_info(FileName, [{time, posix}]),
    Info#file_info.mtime.

get_max_mtime([]) -> undefined;
get_max_mtime(List) -> lists:max(List).

ensure_dir(Name) ->
    case filelib:is_dir(Name) of
        true -> ok;
        false ->
            case file:make_dir(Name) of
                ok -> ok;
                {error, Err} -> throw({mk_dir, Name, Err})
            end
    end.

%%------------------------------------------------------------------------------

%% Special case when executing from a release
compile_modules(_, _, [], DT, _, _) when length(DT) > 0 ->
    false;
compile_modules(Src, Dst, [{B, SM} | ST], [{B, DM} | DT], Opts, IsDel)
  when DM > SM ->
    compile_modules(Src, Dst, ST, DT, Opts, IsDel);
compile_modules(Src, Dst, [{B, _} | ST], [{B, _} | DT], Opts, IsDel) ->
    do_compile_module(Src, Dst, B, Opts),
    compile_modules(Src, Dst, ST, DT, Opts, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [{DB, _} | _] = D, Opts, IsDel)
  when SB < DB ->
    do_compile_module(Src, Dst, SB, Opts),
    compile_modules(Src, Dst, ST, D, Opts, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [] = D, Opts, IsDel) ->
    do_compile_module(Src, Dst, SB, Opts),
    compile_modules(Src, Dst, ST, D, Opts, IsDel);
compile_modules(Src, Dst, [{SB, _} | _] = S, [{DB, _} | DT], Opts, _IsDel)
  when DB < SB ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, Opts, true);
compile_modules(Src, Dst, [] = S, [{DB, _} | DT], Opts, _IsDel) ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, Opts, true);
compile_modules(_, _, [], [], _, IsDel) ->
    IsDel.

do_compile_module(Src, Dst, Name, Opts) ->
    File = filename:join(Src, Name),
    NOpts = [verbose, report, {outdir, Dst}] ++ Opts,
    case compile:file(File, NOpts) of
        {ok, _Module} ->
            io:format("  ERL: Compiled '~s'.~n", [File]);
        {ok, _Module, Warnings} ->
            Msg = "  ERL: Compiled '~s',~n => Warnings: ~p.~n",
            io:format(Msg, [File, Warnings]);
        {error, Err, Warn} ->
            Msg = "!Ignored '~s',~n => Warnings: ~p,~n => Errors: ~p.~n",
            io:format(Msg, [File, Warn, Err]),
            throw({compile_error, Src, Err});
        error ->
            Msg = "  !Ignored '~s',~n => Unknown error encountered!.~n",
            io:format(Msg, [File]),
            throw({compile_error, Src, unknown_error})
    end.

remove_module(Dst, Name) ->
    File = filename:join(Dst, Name ++ ".beam"),
    case file:delete(File) of
        ok -> io:format("  !Deleted '~s'.~n", [File]);
        {error, Err} -> throw({delete, File, Err})
    end.

%%------------------------------------------------------------------------------

process_app(SrcPath, DstPath, MaxMTime, IsDel, Modules, {AppName, Ts}) ->
    AppDst = list_file(DstPath, AppName ++ ".app"),
    AppSrc = {filename:join(SrcPath, AppName ++ ".app.src"), Ts},
    compile_app(IsDel, AppSrc, AppDst, MaxMTime, Modules).

compile_app(true, {Src, _}, {Dst, _}, _MaxMTime, Modules) ->
    do_compile_app(Src, Dst, Modules);
%% Special case when executing from a release
compile_app(false, {_, undefined}, {_, Ts}, undefined, Modules)
  when Ts =/= undefined, length(Modules) > 0 ->
    ok;
compile_app(false, {Src, _}, {Dst, undefined}, _MaxMTime, Modules) ->
    do_compile_app(Src, Dst, Modules);
compile_app(false, {Src, SrcTime}, {Dst, DstTime}, MaxMTime, Modules)
  when MaxMTime >= DstTime; SrcTime >= DstTime ->
    do_compile_app(Src, Dst, Modules);
compile_app(false, _, _, _, _) ->
    ok.

do_compile_app(Src, Dst, Modules) ->
    case file:read_file(Src) of
        {ok, Bin} -> scan_app(Src, Dst, Modules, Bin);
        {error, Err} -> throw({app_src, Src, Err})
    end.

scan_app(Src, Dst, Modules, OBin) ->
    Pat = <<"=MODULES=|%MODULES%|{{modules}}">>,
    To = io_lib:format("~p", [?BLD_APP_MOD_RE]),
    Bin = iolist_to_binary(re:replace(OBin, Pat, To)),
    case erl_scan:string(binary_to_list(Bin)) of
        {ok, Tks, _} -> write_app(Src, Dst, Modules, erl_parse:parse_term(Tks));
        {error, Err, Loc} -> throw({scan_error, Src, Err, Loc})
    end.

write_app(_Src, Dst, Modules, {ok, {App, Name, List}}) ->
    Term = {modules, [?BLD_APP_MOD_RE]},
    NewList = list_replace(List, Term, {modules, Modules}, []),
    AppOut = io_lib:format("~p.~n", [{App, Name, NewList}]),
    case file:write_file(Dst, AppOut) of
        ok -> io:format("  APP: Created '~s'.~n", [Dst]);
        {error, Err} -> throw({write_error, Dst, Err})
    end;
write_app(Src, _Dst, _Modules, Other) ->
    throw({parse_error, Src, Other}).

list_replace([What|T], What, To, Acc) -> list_replace(T, What, To, [To|Acc]);
list_replace([X|T], What, To, Acc) -> list_replace(T, What, To, [X|Acc]);
list_replace([], _What, _To, Acc) -> lists:reverse(Acc).

%%------------------------------------------------------------------------------

list_modules(Path) ->
    All = filelib:wildcard("*.beam", Path),
    Modules = [filename:basename(X, ".beam") || X <- All],
    [list_to_atom(X) || X <- Modules].

load_modules(Path, Modules) ->
    io:format("Loaded:"),
    lists:foreach(fun(X) -> do_load_module(Path, X) end, Modules),
    io:format("~n").

do_load_module(Path, Module) ->
    File = filename:join(Path, Module),
    case code:load_abs(File) of
        {module, Mod} -> io:format(" ~s", [Mod]);
        {error, Err} -> throw({load_error, File, Err})
    end.

%%------------------------------------------------------------------------------

handle_err({no_app_src_file, SrcPath}) ->
    [<<"Error: No .app.src file in ">>, SrcPath, <<"\n">>];
handle_err({multiple_app_src_files, SrcPath}) ->
    [<<"Error: More than one .app.src file in ">>, SrcPath, <<"\n">>];
handle_err({app_src, Src, Err}) ->
    [<<"Error reading '">>, Src, <<"', reason: ">>, to_term(Err), <<"\n">>];
handle_err({scan_error, Src, Err, Loc}) ->
    [<<"Error reading '">>, Src, <<"' in line ">>, to_term(Loc), <<": ">>,
     to_term(Err), <<"\n">>];
handle_err({parse_error, Src, Other}) ->
    [<<"Error parsing '">>, Src, <<"': ">>, to_term(Other), <<"\n">>];
handle_err({write_error, Dst, Err}) ->
    [<<"Error writing '">>, Dst, <<"': ">>, to_term(Err), <<"\n">>];
handle_err({compile_error, Src, Err}) ->
    [<<"Error when compiling '">>, Src, <<"': ">>, to_term(Err), <<"\n">>];
handle_err({mk_dir, Name, Err}) ->
    [<<"Error, couldn't create the folder '">>, Name, <<"': ">>,
     to_term(Err), <<"\n">>];
handle_err({delete, File, Err}) ->
    [<<"Error when deleting file '">>, File, <<"': ">>, to_term(Err), <<"\n">>];
handle_err({load_error, File, Err}) ->
    [<<"\nError loading module '">>, File, <<"': ">>, to_term(Err), <<"\n">>].

to_term(X) -> io_lib:format("~p", [X]).

%%------------------------------------------------------------------------------

builderl_usage() ->
    [
     "************************************************************************",
     "Helper command to manage the builderl installation.",
     "",
     "Usage:",
     "  builderl.esh [ -h | --help | -v | -a ]",
     "  builderl.esh [ -s <version> | -s novsn ] [ -u ]",
     "",
     "  -h, --help",
     "    This help.",
     "",
     "  -v, --version",
     "    Prints current version.",
     "",
     "  -a",
     "    Prints all available versions. Value 'novsn' is printed if the name",
     "    of the application directory doesn't contain any version.",
     "",
     "  -s <version>, -s novsn",
     "    Switches to the specified version by updating the link in the 'bin'",
     "    folder. If 'novsn' is used then builderl will search for",
     "    the application directory without version.",
     "",
     "  -u",
     "    Updates files and links in the 'bin' folder used by builderl.",
     "    If '-s' is also provided then this option is executed after",
     "    switching to the specified version.",
     "",
     "  -uy",
     "    Like '-u' but overwrites files without asking user for permission",
     "    to do so. Useful in noninteractive scripts.",
     "************************************************************************"
    ].

do_builderl(["-h"]) ->        bld_lib:print(builderl_usage());
do_builderl(["--help"]) ->    bld_lib:print(builderl_usage());
do_builderl(["-v"]) ->        print_version();
do_builderl(["--version"]) -> print_version();
do_builderl(["-a"]) ->        print_all_versions();
do_builderl(Other) ->         builderl1(Other, []).

print_version() ->
    File = filename:join([?BUILDERLLINK, "ebin", ?BUILDERLAPP]),
    [{application, builderl, List}] = bld_lib:consult_app_file(File),
    Vsn = proplists:get_value(vsn, List),
    io:format(standard_io, "~s~n", [Vsn]).

print_all_versions() ->
    [io:format(standard_io, "~s~n", [format_vsn(X)]) || X <- get_versions()].

format_vsn({_, novsn}) -> "novsn";
format_vsn({_, Vsn}) -> Vsn.

get_versions() -> get_versions("deps") ++ get_versions("lib").

get_versions(Dir) ->
    All = filelib:wildcard("builderl*", Dir),
    [format_vsn(Dir, string:tokens(X, "-")) || X <- All].

format_vsn(Dir, ["builderl"]) -> {Dir, novsn};
format_vsn(Dir, ["builderl", Vsn]) -> {Dir, Vsn}.

%%------------------------------------------------------------------------------

builderl1(["-s", "novsn"|T], Acc) -> builderl1(T, [{switch, novsn}|Acc]);
builderl1(["-s", Vsn|T], Acc) ->     builderl1(T, [{switch, Vsn}|Acc]);
builderl1(["-u"|T], Acc) ->          builderl1(T, [update|Acc]);
builderl1(["-uy"|T], Acc) ->         builderl1(T, [yes_update|Acc]);
builderl1([], Acc) ->                builderl2(lists:reverse(Acc));
builderl1(Other, _) ->               bld_lib:halt_badarg(Other).

builderl2(Options) ->
    io:format("Using options: ~p~n~n", [Options]),
    lists:foreach(fun(X) -> exec_builderl(X) end, Options).

exec_builderl({switch, Vsn}) -> switch_to(Vsn);
exec_builderl(update)        -> update_bin(true);
exec_builderl(yes_update)    -> update_bin(false).

switch_to(Vsn) ->
    case lists:keyfind(Vsn, 2, get_versions()) of
        false -> bld_lib:print(no_version(Vsn)), halt(1);
        {Dir, Vsn} -> update_link(link_name(Dir, Vsn))
    end.

no_version(novsn) ->
    [
     "Error:", "Could not locate the 'builderl' application folder with no "
     "version in the folder name.", "Aborting."
    ];
no_version(Vsn) ->
    [
     "Error:", "Couldn't locate the 'builderl' application folder with "
     "version '" ++ Vsn ++ "' in the folder name.", "Aborting."
    ].

link_name(Dir, novsn) -> filename:join(["..", Dir, "builderl"]);
link_name(Dir, Vsn) -> filename:join(["..", Dir, "builderl-" ++ Vsn]).

update_link(NewLink) ->
    bld_lib:rm_link(?BUILDERLLINK),
    bld_lib:mk_link(NewLink, ?BUILDERLLINK).

update_bin(Ask) ->
    update_bin(Ask, [bld_file_type(X) || X <- ["builderl"] ++ ?DEL_LINKS]).

update_bin(true, Types) ->
    io:format(standard_io, "Following files will be replaced:~n", []),
    lists:foreach(fun(X) -> can_replace(X) andalso print_type(X) end, Types),
    Read = io:get_line("Continue (y/n)? [y]: "),
    is_continue(Read) andalso do_continue(Types);
update_bin(false, Types) ->
    do_continue(Types).

bld_file_type(Name) ->
    Filename = filename:join(<<"bin">>, Name ++ ".esh"),
    {Filename, filter_type(bld_lib:file_type(Filename))}.

filter_type({ok, Type}) -> Type;
filter_type({error, _} = Err) -> Err.

can_replace({_, link}) -> true;
can_replace({_, file}) -> true;
can_replace({_, {error, enoent}}) -> false;
can_replace({File, {error, Err}}) ->
    Msg = "Error, can't replace '~s', error: '~p'~n",
    halt_bad_file(Msg, File, Err);
can_replace({File, Type}) ->
    Msg = "Error, can't replace '~s' which is of type '~p'~n",
    halt_bad_file(Msg, File, Type).

halt_bad_file(Msg, File, Arg) ->
    io:format(standard_error, Msg, [File, Arg]),
    halt(1).

print_type({File, Type}) ->
    Bin = << (type_to_bin(Type))/binary, File/binary >>,
    io:format(standard_io, "~s~n", [Bin]).

type_to_bin(link) -> <<"Link: ">>;
type_to_bin(file) -> <<"File: ">>.

is_continue("\n") -> true;
is_continue([$y|_]) -> true;
is_continue([$Y|_]) -> true;
is_continue([$n|_]) -> false;
is_continue([$N|_]) -> false.

do_continue(Types) ->
    lists:foreach(fun(X) -> can_replace(X) andalso delete_type(X) end, Types),
    From = filename:join([?BUILDERLLINK] ++ ?SKEL_DIR ++ [<<"bin">>]),
    Bin = <<"bin">>,
    MainFile = <<"builderl.esh">>,
    bld_lib:cp_file(From, Bin, MainFile),
    bld_lib:chmod_exe(filename:join(Bin, MainFile)),
    lists:foreach(fun(X) -> add_link(MainFile, Bin, X) end, ?BLD_LINKS).

delete_type({Link, link}) -> bld_lib:rm_link(Link);
delete_type({File, file}) -> bld_lib:rm_file(File).

add_link(MainFile, Bin, Base) ->
    bld_lib:mk_link(MainFile, filename:join(Bin, Base ++ ".esh")).
