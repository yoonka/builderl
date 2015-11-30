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

-module(bld_load).

-export([boot/3, current_app_vsn/1]).

-export([
         builderl/1,
         update_root_dir/1,
         configure/1,
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

boot(SrcPath, DstPath, IncPath) ->
    BldErlAppSrc = ?BUILDERLAPP ++ ".src",
    AppSrc = list_file(SrcPath, BldErlAppSrc),
    AppDst = list_file(DstPath, ?BUILDERLAPP),

    Srcs = list_files(SrcPath, ".erl"),
    Beams = list_files(DstPath, ".beam"),
    MaxMTime = get_max_mtime([X || {_, X} <- Srcs]),

    ensure_dir(DstPath),
    IsDel = compile_modules(SrcPath, DstPath, Srcs, Beams, IncPath, false),
    compile_app(IsDel, AppSrc, AppDst, MaxMTime, load_modules(DstPath)).

current_app_vsn(Path) ->
    File = filename:join(Path, ?BUILDERLAPP),
    [{application, builderl, List}] = bld_lib:consult_app_file(File),
    "builderl-" ++ proplists:get_value(vsn, List).

%%------------------------------------------------------------------------------

builderl(Args)        -> do_builderl(Args).
update_root_dir(Args) -> bld_cfg:set_root(Args).
configure(Args)       -> bld_cfg:configure(Args).
mk_dev([])            -> bld_rel:mk_dev().
mk_rel([])            -> bld_rel:mk_rel(?BUILDERLLINK).
init(Args)            -> bld_init:do(Args).
config(Args)          -> bld_cfg:config(Args).
start(Args)           -> bld_run:start(Args).
stop(Args)            -> bld_run:stop(Args).
deps(Args)            -> bld_deps:start(Args).

%%------------------------------------------------------------------------------

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
        false -> mk_dir(Name)
    end.

mk_dir(Name) ->
    io:format(standard_io, "Create folder '~s': ", [Name]),
    case file:make_dir(Name) of
        ok ->
            io:format(standard_io, "Done.~n", []);
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end.

%%------------------------------------------------------------------------------

%% Special case when executing from a release
compile_modules(_, _, [], DT, _, _) when length(DT) > 0 ->
    false;
compile_modules(Src, Dst, [{B, SM} | ST], [{B, DM} | DT], IncPath, IsDel)
  when DM > SM ->
    compile_modules(Src, Dst, ST, DT, IncPath, IsDel);
compile_modules(Src, Dst, [{B, _} | ST], [{B, _} | DT], IncPath, IsDel) ->
    do_compile_module(Src, Dst, B, IncPath),
    compile_modules(Src, Dst, ST, DT, IncPath, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [{DB, _} | _] = D, IncPath, IsDel)
  when SB < DB ->
    do_compile_module(Src, Dst, SB, IncPath),
    compile_modules(Src, Dst, ST, D, IncPath, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [] = D, IncPath, IsDel) ->
    do_compile_module(Src, Dst, SB, IncPath),
    compile_modules(Src, Dst, ST, D, IncPath, IsDel);
compile_modules(Src, Dst, [{SB, _} | _] = S, [{DB, _} | DT], IncPath, _IsDel)
  when DB < SB ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, IncPath, true);
compile_modules(Src, Dst, [] = S, [{DB, _} | DT], IncPath, _IsDel) ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, IncPath, true);
compile_modules(_, _, [], [], _, IsDel) ->
    IsDel.

do_compile_module(Src, Dst, Name, IncPath) ->
    File = filename:join(Src, Name),
    io:format("Compiling: '~s': ", [File]),
    Opts = [verbose, report, {outdir, Dst}, {i, IncPath}],
    case compile:file(File, Opts) of
        {ok, Module} ->
            io:format("Created '~p'.~n", [Module]);
        {ok, Module, Warnings} ->
            io:format("~nWarnings: ~p~nCreated: '~p'~n", [Warnings, Module]);
        {error, Err, Warn} ->
            io:format("~nWarnings: ~p~nErrors: ~p~nAborting...~n", [Warn, Err]),
            halt(1);
        error ->
            io:format("~nUnknown error encountered, aborting...~n", []),
            halt(1)
    end.

remove_module(Dst, Name) ->
    File = filename:join(Dst, Name ++ ".beam"),
    io:format("Deleting '~s': ", [File]),
    case file:delete(File) of
        ok -> io:format("OK~n");
        Err -> do_error(Err)
    end.

%%------------------------------------------------------------------------------

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
    write_app(bld_lib:consult_app_file(Src), Dst, Modules).

write_app([{App, Name, List}], Dst, Modules) ->
    io:format("Writing ~s: ", [Dst]),
    NewList = lists:keyreplace(modules, 1, List, {modules, Modules}),
    AppOut = io_lib:format("~p.~n", [{App, Name, NewList}]),
    case file:write_file(Dst, AppOut) of
        ok -> io:format("OK~n");
        Err -> do_error(Err)
    end.

%%------------------------------------------------------------------------------

load_modules(Path) ->
    All = filelib:wildcard("*.beam", Path),
    Modules = [filename:basename(X, ".beam") || X <- All],
    io:format("Loaded:"),
    lists:foreach(fun(X) -> do_load_module(Path, X) end, Modules),
    io:format("~n"),
    [list_to_atom(X) || X <- Modules].

do_load_module(Path, Module) ->
    File = filename:join(Path, Module),
    case code:load_abs(File) of
        {module, Mod} -> io:format(" ~s", [Mod]);
        Err -> io:format("~n"), do_error(Err)
    end.

%%------------------------------------------------------------------------------

do_error(Err) ->
    io:format("Error '~p', aborting.~n", [Err]),
    halt(1).

%%------------------------------------------------------------------------------

builderl_usage() ->
    [
     "************************************************************************",
     "Helper command to manage the builderl installation.",
     "",
     "Usage:",
     "  builderl.esh [ -h | --help ]",
     "",
     "  -h, --help",
     "    This help.",
     "************************************************************************"
    ].

do_builderl(["-h"]) ->     bld_lib:print(builderl_usage());
do_builderl(["--help"]) -> bld_lib:print(builderl_usage());
do_builderl([]) ->         builderl1().

builderl1() ->
    ok.
