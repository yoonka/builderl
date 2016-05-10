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

-export([boot/2, compile/2, get_dst_path/1]).
-export([start_serializer/0, stop_serializer/0]).

-include_lib("kernel/include/file.hrl").
-include("../include/load_builderl.hrl").

-define(BLD_APP_MOD_RE, {builderl_generated}).

-record(opts, {mk_plugin, mk_state, make_opts, compiler_opts, redo_app}).
-record(fi, {base, ext, file, mtime}).

boot(Path, Root) ->
    start_serializer(),
    try
        CompilerOpts = [{erl, [verbose, report, {i, Root}, {i, "lib"}]}],
        compile_dep(Path, [], CompilerOpts, undefined, ok),
        true = code:add_patha(get_dst_path(Path))
    catch
        throw:Err -> io:format(standard_io, handle_err(Err), []), halt(1)
    end,
    stop_serializer().

compile(Path, Combined) ->
    try
        {MakeOpts, CompilerOpts} = get_options(Combined),
        compile_dep(Path, MakeOpts, CompilerOpts)
    catch
        throw:{stop, Type, Mod, Fun, P} -> ret_stopped(Type, Mod, Fun, P);
        throw:Err -> {error, handle_err(Err)}
    end.

start_serializer() ->
    Pid = spawn(fun serializer/0),
    true = register('$bld_print', Pid).

stop_serializer() ->
    '$bld_print' ! {self(), stop},
    receive ok -> ok end.

%%------------------------------------------------------------------------------

serializer() ->
    receive
        {Pid, stop} ->
            Pid ! ok;
        Msg ->
            (catch print_msg(Msg)) =:= ok orelse
                io:format("Couldn't print '~p', ignoring.~n", [Msg]),
            serializer()
    end.

print_msg({Msg}) -> io:format(Msg);
print_msg({Msg, Args}) -> io:format(Msg, Args);
print_msg({error, Msg, Args}) -> io:format(standard_error, Msg, Args);
print_msg(Lines) when is_list(Lines) -> io:format(Lines, []).

format_msg(IsWarning, List) ->
    [format_line(IsWarning, File, L) || {File, Lines} <- List, L <- Lines].

format_line(IsWarning, File, {Line, Module, Info}) ->
    [File, <<":">>, get_line(Line), <<": ">>, get_warning(IsWarning),
     Module:format_error(Info), <<"\n">>].

get_line(none) -> <<"">>;
get_line(Integer) -> integer_to_list(Integer).

get_warning(false) -> <<"">>;
get_warning(true) -> <<"Warning: ">>.

get_dst_path(Path) -> filename:join(Path, "ebin").

%%------------------------------------------------------------------------------

get_options(Combined) ->
    case lists:keytake(make_options, 1, Combined) of
        false -> {[], Combined};
        {value, {make_options, MkOpts}, CompilerOpts} -> {MkOpts, CompilerOpts}
    end.

compile_dep(Path, MakeOpts, CompilerOpts) ->
    case lists:keyfind(mk_plugin_info, 1, MakeOpts) of
        false ->
            compile_dep(Path, MakeOpts, CompilerOpts, undefined, undefined);
        {mk_plugin_info, Mod, State} ->
            case Mod:handle_dep_mk(Path, State) of
                stop ->
                    ret_stopped(Mod, <<"handle_dep_mk/2">>, Path);
                {ok, NState} ->
                    compile_dep(Path, MakeOpts, CompilerOpts, Mod, NState)
            end
    end.

ret_stopped(Mod, Fun, Path) ->
    ret_rest([<<"compilation of '">>, Path], Mod, Fun).

ret_stopped(Type, Mod, Fun, Path) ->
    M = [<<"'">>, atom_to_binary(Type, utf8), <<"' compilation for '">>, Path],
    ret_rest(M, Mod, Fun).

ret_rest(Msg, Mod, Fun) ->
    {ok, [<<"  => Stopped ">>] ++ Msg ++
         [<<"' by '">>, atom_to_binary(Mod, utf8), <<":">>, Fun, <<"'.\n">>]}.

compile_dep(Path, MakeOpts, CompilerOpts, Mod, State) ->
    SrcPath = filename:join(Path, "src"),
    case filelib:is_dir(SrcPath) of
        true -> compile_dep(Path, SrcPath, MakeOpts, CompilerOpts, Mod, State);
        false -> {ok, [<<"No 'src' folder in '">>, Path, <<"', ignoring.\n">>]}
    end.

compile_dep(Path, SrcPath, MakeOpts, CompilerOpts, Mod, State) ->
    Res = file:make_dir(DstPath = get_dst_path(Path)),
    Res =:= ok orelse Res =:= {error, eexist}
        orelse throw({mk_dir, DstPath, element(2, Res)}),

    ErlOpts = [{outdir, DstPath} | proplists:get_value(erl, CompilerOpts, [])],
    Opts0 = #opts{mk_plugin = Mod, mk_state = State,
                  make_opts = MakeOpts, compiler_opts = ErlOpts},

    SrcPaths = [SrcPath | [filename:join(Path, X) || {spa, X} <- MakeOpts]],
    {SrcsOrg, Opts1} = lists:foldl(fun list_src/2, {[], Opts0}, SrcPaths),
    Srcs = lists:sort(fun fi_sort_fun/2, SrcsOrg),
    Opts2 = compile_modules(Srcs, list_dir(DstPath, ".beam", false), Opts1),

    MaxMTime = get_max_mtime([X#fi.mtime || X <- Srcs]),
    case list_dir(SrcPath, ".app.src", false) of
        [App] -> process_app(App, MaxMTime, DstPath, Opts2);
        [] -> throw({no_app_src_file, SrcPath});
        _ -> throw({multiple_app_src_files, SrcPath})
    end,
    {ok, [<<"  => Finished compiling in '">>, Path, <<"'.\n">>]}.

fi_sort_fun(A, B) -> A#fi.base =< B#fi.base.

list_src(SrcPath, {Files, Opts}) ->
    case can_compile_dir(SrcPath, Opts) of
        {true, NOpts} -> {list_dir(SrcPath, ".erl", true) ++ Files, NOpts};
        {false, NOpts} -> {Files, NOpts}
    end.

can_compile_dir(_SrcPath, #opts{mk_plugin = undefined} = Opts) ->
    {true, Opts};
can_compile_dir(SrcPath, #opts{mk_plugin = Mod, mk_state = State} = Opts) ->
    case Mod:handle_dir_mk(SrcPath, State) of
        stop ->
            throw({stop, erl, Mod, <<"handle_dir_mk/2">>, SrcPath});
        {ignore, NState} ->
            Msg = "  => Ignored compilation of '~s' by '~s:handle_dep_mk/4'.~n",
            '$bld_print' ! {Msg, [SrcPath, Mod]},
            {false, Opts#opts{mk_state = NState}};
        {ok, NState} ->
            {true, Opts#opts{mk_state = NState}}
    end.

list_dir(Path, Ext, Recursive) ->
    lists:sort(fun fi_sort_fun/2, list_files(Path, Ext, Recursive)).

list_files(Path, Ext, Recursive) ->
    FFun = fun(X, AccIn) -> file_filter(X, AccIn, Ext, Path) end,
    {Files, Dirs} = lists:foldl(FFun, {[], []}, list_dir(Path)),
    if Dirs =:= []; Recursive =:= false ->
            Files;
       true ->
            RFun = fun(X, AccIn) -> list_files(X, Ext, Recursive) ++ AccIn end,
            lists:foldl(RFun, Files, Dirs)
    end.

list_dir(Path) ->
    case file:list_dir(Path) of
        {ok, List} -> List;
        _ -> []
    end.

file_filter([$.|_], AccIn, _Ext, _Path) ->
    AccIn;
file_filter(Name, {Files, Dirs} = AccIn, Ext, Path) ->
    PathName = filename:join(Path, Name),
    case filelib:is_dir(PathName) of
        true ->
            {Files, [PathName|Dirs]};
        false ->
            case check_ext(Name, Ext) of
                false -> AccIn;
                {B, E} -> {[format_file(B, E, PathName)|Files], Dirs}
            end
    end.

check_ext(Name, undefined) ->
    case filename:extension(Name) of
        [] -> false;
        Ext -> {filename:rootname(Name), Ext}
    end;
check_ext(Name, Ext) ->
    case lists:suffix(Ext, Name) of
        false -> false;
        true -> {lists:sublist(Name, length(Name) - length(Ext)), Ext}
    end.

format_file(Base, Ext, PathName) ->
    {ok, Info} = file:read_file_info(PathName, [{time, posix}]),
    #fi{base = Base, ext = Ext, file = PathName, mtime = Info#file_info.mtime}.

get_max_mtime([]) -> undefined;
get_max_mtime(List) -> lists:max(List).

%%------------------------------------------------------------------------------

compile_modules([#fi{base = B} = Src | ST], [#fi{base = B} = Dst | DT], Opts)
  when Dst#fi.mtime > Src#fi.mtime ->
    compile_modules(ST, DT, Opts);
compile_modules([#fi{base = B} = Src | ST], [#fi{base = B} = Dst | DT], Opts) ->
    remove_module(Dst#fi.file),
    compile_modules(ST, DT, do_compile_module(Src#fi.file, Opts));
compile_modules([Src | ST], [Dst | _] = D, Opts)
  when Src#fi.base < Dst#fi.base ->
    compile_modules(ST, D, do_compile_module(Src#fi.file, Opts));
compile_modules([Src | ST], [] = D, Opts) ->
    compile_modules(ST, D, do_compile_module(Src#fi.file, Opts));
compile_modules([Src | _] = S, [Dst | DT], Opts)
  when Dst#fi.base < Src#fi.base ->
    remove_module(Dst#fi.file),
    compile_modules(S, DT, Opts#opts{redo_app = true});
compile_modules([] = S, [Dst | DT], Opts) ->
    remove_module(Dst#fi.file),
    compile_modules(S, DT, Opts#opts{redo_app = true});
compile_modules([], [], Opts) ->
    Opts.

do_compile_module(Src, #opts{mk_plugin = undefined} = Opts) ->
    do_compile_module1(Src, Opts#opts.compiler_opts),
    Opts;
do_compile_module(Src, #opts{mk_plugin = Mod, mk_state = State} = Opts) ->
    case Mod:handle_src_mk(erl, Src, Opts#opts.compiler_opts, State) of
        stop ->
            throw({stop, erl, Mod, <<"handle_src_mk/4">>, Src});
        {ignore, NState} ->
            Msg = "  => Ignored compilation of '~s' by "
                "'~s:handle_src_mk/4'.~n",
            '$bld_print' ! {Msg, [Src, Mod]},
            Opts#opts{mk_state = NState};
        {ok, NErlOpts, NState} ->
            do_compile_module1(Src, NErlOpts),
            Opts#opts{mk_state = NState}
    end.

do_compile_module1(Src, ErlOpts) ->
    case compile:file(filename:rootname(Src), ErlOpts) of
        {ok, _Module} ->
            '$bld_print' ! {"  ERL: Compiled '~s'.~n", [Src]};
        {ok, _Module, []} ->
            '$bld_print' ! {"  ERL: Compiled '~s'.~n", [Src]};
        {ok, _Module, Warn} ->
            '$bld_print' ! format_msg(true, Warn) ++
                [<<"  ERL: Compiled '">>, Src, <<"' with warnings!\n">>];
        {error, Err, Warn} ->
            '$bld_print' ! format_msg(false, Err) ++ format_msg(true, Warn) ++
                [<<"!Not compiling '">>, Src, <<"' due to errors.\n">>],
            throw({compile_error, Src});
        error ->
            Msg = "!Not compiling '~s' due to an error.~n",
            '$bld_print' ! {Msg, [Src]},
            throw({compile_error, Src})
    end.

remove_module(Dst) ->
    case file:delete(Dst) of
        ok -> '$bld_print' ! {"  Deleted '~s'.~n", [Dst]};
        {error, Err} -> throw({delete, Dst, Err})
    end.

%%------------------------------------------------------------------------------

process_app(App, MaxMTime, DstPath, Opts) ->
    {DstApp, MTime} = list_file(filename:join(DstPath, App#fi.base ++ ".app")),
    case should_create_app(Opts#opts.redo_app, App#fi.mtime, MTime, MaxMTime) of
        true -> do_create_app(App#fi.file, DstApp, DstPath);
        false -> ok
    end.

list_file(DstApp) ->
    case file:read_file_info(DstApp, [{time, posix}]) of
        {ok, Info} -> {DstApp, Info#file_info.mtime};
        {error, enoent} -> {DstApp, undefined}
    end.

should_create_app(true, _, _, _Max) -> true;
should_create_app(_, _, undefined, _Max) -> true;
should_create_app(_, ST, DT, Max) when Max >= DT; ST >= DT -> true;
should_create_app(_, _, _, _) -> false.

do_create_app(Src, Dst, DstPath) ->
    case file:read_file(Src) of
        {ok, Bin} -> scan_app(Src, Dst, DstPath, Bin);
        {error, Err} -> throw({app_src, Src, Err})
    end.

scan_app(Src, Dst, DstPath, OBin) ->
    Pat = <<"=MODULES=|%MODULES%|{{modules}}">>,
    To = io_lib:format("~p", [?BLD_APP_MOD_RE]),
    Bin = iolist_to_binary(re:replace(OBin, Pat, To)),
    case erl_scan:string(binary_to_list(Bin)) of
        {ok, Tks, _} -> write_app(Src, Dst, DstPath, erl_parse:parse_term(Tks));
        {error, Err, Loc} -> throw({scan_error, Src, Err, Loc})
    end.

write_app(_Src, Dst, DstPath, {ok, {App, Name, List}}) ->
    Term = {modules, [?BLD_APP_MOD_RE]},
    NewList = list_replace(List, Term, {modules, list_modules(DstPath)}, []),
    AppOut = io_lib:format("~p.~n", [{App, Name, NewList}]),
    case file:write_file(Dst, AppOut) of
        ok -> '$bld_print' ! {"  APP: Created '~s'.~n", [Dst]};
        {error, Err} -> throw({write_error, Dst, Err})
    end;
write_app(Src, _Dst, _DstPath, Other) ->
    throw({parse_error, Src, Other}).

list_replace([What|T], What, To, Acc) -> list_replace(T, What, To, [To|Acc]);
list_replace([X|T], What, To, Acc) -> list_replace(T, What, To, [X|Acc]);
list_replace([], _What, _To, Acc) -> lists:reverse(Acc).

list_modules(P) ->
    [list_to_atom(filename:rootname(X)) || X <- filelib:wildcard("*.beam", P)].

%%------------------------------------------------------------------------------

handle_err({no_app_src_file, SrcPath}) ->
    [<<"Error: No .app.src file in ">>, SrcPath, <<"\n">>];
handle_err({multiple_app_src_files, SrcPath}) ->
    [<<"Error: More than one .app.src in ">>, SrcPath, <<"\n">>];
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
    [<<"Error when deleting file '">>, File, <<"': ">>, to_term(Err), <<"\n">>].

to_term(X) -> io_lib:format("~p", [X]).
