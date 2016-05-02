%% Copyright (c) 2016, Grzegorz Junka
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

-module(bld_make).

-export([boot/2, compile/2]).

-include_lib("kernel/include/file.hrl").
-include("../include/load_builderl.hrl").

-define(BLD_APP_MOD_RE, {builderl_generated}).

-record(opts, {mk_plugin, mk_state, make_opts, compiler_opts}).

boot(Path, Root) ->
    try
        CompilerOpts = [{erl, [verbose, report, {i, Root}, {i, "lib"}]}],
        compile_dep(Path, [load], CompilerOpts, undefined, ok)
    catch
        throw:Err -> io:format(standard_io, handle_err(Err), []), halt(1)
    end.

compile(Path, Combined) ->
    try
        {MakeOpts, CompilerOpts} = get_options(Combined),
        compile_dep(Path, MakeOpts, CompilerOpts)
    catch
        throw:{stop, Type, Mod, Fun, Path} -> ret_stopped(Type, Mod, Fun, Path);
        throw:Err -> {error, handle_err(Err)}
    end.

%%------------------------------------------------------------------------------

get_options(Combined) ->
    case lists:keytake(make_options, 1, Combined) of
        false -> {[], Combined};
        {value, {make_options, MkOpts}, CompilerOpts} -> {MkOpts, CompilerOpts}
    end.

compile_dep(Path, MakeOpts, CompilerOpts) ->
    case lists:keyfind(mk_plugin, 1, MakeOpts) of
        false ->
            compile_dep(Path, MakeOpts, CompilerOpts, undefined, undefined);
        {mk_plugin, Mod, State} ->
            case Mod:handle_dep_mk(Path, State) of
                stop -> ret_stopped(Mod, <<"handle_dep_mk/2">>, Path);
                NState -> compile_dep(Path, MakeOpts, CompilerOpts, Mod, NState)
            end
    end,
    {ok, [<<"  => Finished compiling in ">>, Path, <<"\n">>]}.

ret_stopped(Mod, Fun, Path) ->
    ret_rest([<<"Compilation of '">>, Path, <<"' stopped">>], Mod, Fun).

ret_stopped(Type, Mod, Fun, Path) ->
    Msg = [<<"Compilation of '">>, Type, <<"' stopped for '">>, Path, <<"'">>],
    ret_rest(Msg, Mod, Fun).

ret_rest(Msg, Mod, Fun) ->
    {ok, Msg ++ [<<" in '">>, Mod, <<":">>, Fun, <<"'.\n">>]}.

compile_dep(Path, MakeOpts, CompilerOpts, Mod, State) ->
    SrcPath = filename:join(Path, "src"),
    case filelib:is_dir(SrcPath) of
        true -> compile_src(Path, SrcPath, MakeOpts, CompilerOpts, Mod, State);
        false -> {ok, [<<"No 'src' folder in '">>, Path, <<"', ignoring.\n">>]}
    end.

compile_src(Path, SrcPath, MakeOpts, CompilerOpts, Mod, State) ->
    DstPath = filename:join(Path, "ebin"),
    ErlOpts = [{outdir, DstPath} | proplists:get_value(erl, CompilerOpts, [])],
    Opts = #opts{mk_plugin = Mod, mk_state = State,
                 make_opts = MakeOpts, compiler_opts = ErlOpts},
    if Mod =/= undefined -> do_handle_dep_mk(Path, SrcPath, DstPath, Opts);
       true -> compile_erl(SrcPath, DstPath, Opts) end.

do_handle_dep_mk(Path, SrcPath, DstPath, Opts) ->
    #opts{mk_plugin = Mod, mk_state = State} = Opts,
    case Mod:handle_dep_mk(erl, SrcPath, Opts#opts.compiler_opts, State) of
        stop ->
            throw({stop, erl, Mod, <<"handle_dep_mk/4">>, Path});
        {ignore, NState} ->
            Msg = "  ERL: Compilation in '~s' ignored by ~s:handle_dep_mk/4.~n",
            io:format(Msg, [Path, Mod]),
            Opts#opts{mk_state = NState};
        {NErlOpts, NState} ->
            NOpts = Opts#opts{mk_state = NState, compiler_opts = NErlOpts},
            compile_erl(SrcPath, DstPath, NOpts)
    end.

compile_erl(SrcPath, DstPath, Opts) ->
    ensure_dir(DstPath),
    Srcs = list_files(SrcPath, ".erl"),
    Beams = list_files(DstPath, ".beam"),
    MaxMTime = get_max_mtime([X || {_, X} <- Srcs]),
    {IsDel, NOpts} =
        compile_modules(SrcPath, DstPath, Srcs, Beams, Opts, false),
    Modules = list_modules(DstPath),
    lists:member(load, NOpts#opts.make_opts) =:= false
        orelse load_modules(DstPath, Modules),

    case list_files(SrcPath, ".app.src") of
        [App] -> process_app(SrcPath, DstPath, MaxMTime, IsDel, Modules, App);
        [] -> throw({no_app_src_file, SrcPath});
        _ -> throw({multiple_app_src_files, SrcPath})
    end,
    NOpts.

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
compile_modules(_, _, [], DT, Opts, _) when length(DT) > 0 ->
    {false, Opts};
compile_modules(Src, Dst, [{B, SM} | ST], [{B, DM} | DT], Opts, IsDel)
  when DM > SM ->
    compile_modules(Src, Dst, ST, DT, Opts, IsDel);
compile_modules(Src, Dst, [{B, _} | ST], [{B, _} | DT], Opts, IsDel) ->
    NOpts = do_compile_module(Src, B, Opts),
    compile_modules(Src, Dst, ST, DT, NOpts, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [{DB, _} | _] = D, Opts, IsDel)
  when SB < DB ->
    NOpts = do_compile_module(Src, SB, Opts),
    compile_modules(Src, Dst, ST, D, NOpts, IsDel);
compile_modules(Src, Dst, [{SB, _} | ST], [] = D, Opts, IsDel) ->
    NOpts = do_compile_module(Src, SB, Opts),
    compile_modules(Src, Dst, ST, D, NOpts, IsDel);
compile_modules(Src, Dst, [{SB, _} | _] = S, [{DB, _} | DT], Opts, _IsDel)
  when DB < SB ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, Opts, true);
compile_modules(Src, Dst, [] = S, [{DB, _} | DT], Opts, _IsDel) ->
    remove_module(Dst, DB),
    compile_modules(Src, Dst, S, DT, Opts, true);
compile_modules(_, _, [], [], Opts, IsDel) ->
    {IsDel, Opts}.

do_compile_module(Dep, Name, Opts) ->
    Src = filename:join(Dep, Name),
    if Opts#opts.mk_plugin =/= undefined ->
            do_handle_src_mk(Src, Opts);
       true ->
            do_compile_module(Src, Opts#opts.compiler_opts),
            Opts
    end.

do_handle_src_mk(Src, #opts{mk_plugin = Mod, mk_state = State} = Opts) ->
    case Mod:handle_src_mk(erl, Src, Opts#opts.compiler_opts, State) of
        stop ->
            throw({stop, erl, Mod, <<"handle_src_mk/4">>, Src});
        {ignore, NState} ->
            Msg = "  ERL: Compilation of '~s' ignored by ~s:handle_src_mk/4.~n",
            io:format(Msg, [Src, Mod]),
            Opts#opts{mk_state = NState};
        {NErlOpts, NState} ->
            do_compile_module(Src, NErlOpts),
            Opts#opts{mk_state = NState}
    end.

do_compile_module(Src, ErlOpts) ->
    case compile:file(Src, ErlOpts) of
        {ok, _Module} ->
            io:format("  ERL: Compiled '~s'.~n", [Src]);
        {ok, _Module, Warnings} ->
            Msg = "  ERL: Compiled '~s',~n=> Warnings: ~p.~n",
            io:format(Msg, [Src, Warnings]);
        {error, Err, Warn} ->
            Msg = "!Ignored '~s',~n=> Warnings: ~p,~n=> Errors: ~p.~n",
            io:format(Msg, [Src, Warn, Err]),
            throw({compile_error, Src});
        error ->
            Msg = "!Ignored '~s',~n=> Unknown error encountered!.~n",
            io:format(Msg, [Src]),
            throw({compile_error, Src})
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
handle_err({compile_error, Src}) ->
    [<<"Error when compiling '">>, Src, <<"'.\n">>];
handle_err({mk_dir, Name, Err}) ->
    [<<"Error, couldn't create the folder '">>, Name, <<"': ">>,
     to_term(Err), <<"\n">>];
handle_err({delete, File, Err}) ->
    [<<"Error when deleting file '">>, File, <<"': ">>, to_term(Err), <<"\n">>];
handle_err({load_error, File, Err}) ->
    [<<"\nError loading module '">>, File, <<"': ">>, to_term(Err), <<"\n">>].

to_term(X) -> io_lib:format("~p", [X]).
