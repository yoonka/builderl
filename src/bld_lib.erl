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

-module(bld_lib).

-include_lib("builderl/include/builderl.hrl").
-include_lib("kernel/include/file.hrl").

-export([
         halt_badarg/1,
         halt_badtype/1,
         halt_toomany/1,
         add_node/4,
         extract_node/2,
         node_dir/3,
         node_name/2,
         node_name/3,
         start_erl_data/0,
         consult_app_file/1,
         intersection/2,
         trim/1,
         print/1,
         h_line/1,
         h_line/2,
         chmod_exe/1,
         chmod/2,
         mk_link/2,
         mk_dir/1,
         rm_file/1,
         rm_link/1,
         file_type/1,
         ensure_member/2,
         get_rel_dir/0,
         read_builderl_config/0,
         read_builderl_config/1,
         get_default_nodes/1,
         get_default_joins/1,
         get_allowed/1,
         get_params/1,
         keyget/3,
         keyget/2,
         start_node/2,
         running_nodes/0,
         connect_to_node/1,
         get_time_diff/1,
         process_file/3,
         process_file/4,
         read_data/1,
         cp_file/3,
         cp_file/4,
         cp_dir/4,
         check_file_op/1,
         write_terms/2,
         call/2
        ]).

%% Name of the node running this escript
-define(NAME, builderl).

%% Attributes to set on an executable script
-define(EXEMODE, 8#00744).

%% Regular expression that a node suffix must match
-define(SUFFIX_RE, "^[\\w-.#+]*$").

%% How long to wait for epmd to start with the node
-define(EPMD_TIMEOUT, 4000).

%%------------------------------------------------------------------------------

err_badarg(Option) ->
    [
     "Error, unrecognized option(s): '" ++ string:join(Option, ", ")
     ++ "', aborting.",
     "Use -h or --help for the list of options."
    ].

err_badtype(Node) ->
    [
     "Error, unrecognized node type: '" ++ Node ++ "', aborting.",
     "Use -h or --help for help."
    ].

err_duplicate(Node) ->
    [
     "Error, node '" ++ Node ++ "' specified multiple times, aborting.",
     "Use -h or --help for help."
    ].

err_suffix(Suffix) ->
    [
     "Error, incorrect suffix: '" ++ Suffix ++ "'. It must match the following",
     "regular expression: '" ++ ?SUFFIX_RE ++ "'."
    ].

err_toomany(Opt) ->
    ["Error, option '" ++ Opt ++ "' can be specified only once."].

halt_badarg(Other)   -> print(err_badarg(Other)),   halt(1).
halt_badtype(Text)   -> print(err_badtype(Text)),   halt(1).
halt_duplicate(Node) -> print(err_duplicate(Node)), halt(1).
halt_suffix(Suffix)  -> print(err_suffix(Suffix)),  halt(1).
halt_toomany(Opt)    -> print(err_toomany(Opt)),    halt(1).

%%------------------------------------------------------------------------------

add_node(Text, Custom, {_, _, BldCfg} = Params, Options) ->
    {Type, Suffix} = extract_node(Text, Params),
    not is_node(Type, Suffix, Options) orelse halt_duplicate(Text),
    [{node, Custom, Type, Suffix, node_dir(Type, Suffix, BldCfg)} | Options].

is_node(Type, Suffix, Options) ->
    lists:any(fun(X) -> is_node1(X, Type, Suffix) end, Options).

is_node1({node, _, T, S, _}, Type, Suffix)
  when T =:= Type, S =:= Suffix -> true;
is_node1(_X, _Type, _Suffix) -> false.


extract_node(Text, {Allowed, SuffixRe, _}) ->
    case string:chr(Text, $-) of
        0 ->
            Node = Text,
            Suffix = [];
        Idx ->
            Node = string:sub_string(Text, 1, Idx - 1),
            Suffix = string:sub_string(Text, Idx + 1)
    end,
    lists:member(Node, Allowed) orelse halt_badtype(Text),
    Len = length(Suffix),
    re:run(Suffix, SuffixRe) =:= {match, [{0, Len}]} orelse halt_suffix(Suffix),
    {list_to_existing_atom(Node), Suffix}.


node_dir(Node, Suffix, BldCfg) ->
    Path = process_location(proplists:get_value(install_location, BldCfg), []),
    Path ++ node_name(Node, Suffix, BldCfg).

process_location(undefined, _) ->
    "../";
process_location([home|T], Acc) ->
    {ok, [[Home]]} = init:get_argument(home),
    process_location(T, [Home | Acc]);
process_location([Elem|T], Acc) when is_list(Elem) ->
    process_location(T, [Elem | Acc]);
process_location([], Acc) ->
    filename:join(Acc).


node_name(Type, Suffix, BldCfg) when is_atom(Type) ->
    node_name(get_node_name(Type, BldCfg), Suffix);
node_name(Name, Suffix, _BldCfg) ->
    node_name(Name, Suffix).

get_node_name(Type, [{node_type, Type, _, Name, _, _}|_]) -> Name;
get_node_name(Type, [_|T]) -> get_node_name(Type, T);
get_node_name(Type, []) -> halt_bad_node_type(Type).

halt_bad_node_type(Type) ->
    io:format("Unknown node type '~p', aborting.~n", [Type]),
    halt(1).


node_name(Name, Suffix) ->
    if Suffix == [] -> Name;
       true -> Name ++ "-" ++ Suffix end.


start_erl_data() ->
    File = "releases/start_erl.data",
    case file:read_file(File) of
        {ok, Data} -> start_erl_data(Data);
        {error, Err} -> error_start_erl(File, Err)
    end.

start_erl_data(Data) ->
    [ErtsVsn, VsnBin] = binary:split(Data, <<" ">>),
    {trim(ErtsVsn), trim(VsnBin)}.

error_start_erl(File, Err) ->
    io:format("~nError when reading '~s': ~p~n", [File, Err]),
    io:format("Note, the file should be created when compiling the project.~n"),
    halt(1).

%%------------------------------------------------------------------------------

consult_app_file(Src) ->
    case file:consult(Src) of
        {ok, AppSrc} -> AppSrc;
        Err -> error_reading_file(Src, Err)
    end.

error_reading_file(Src, Err) ->
    io:format("Error when reading file '~p': '~p'. Aborting.~n", [Src, Err]),
    halt(1).

%%------------------------------------------------------------------------------

intersection(L1, L2) ->
    lists:filter(fun(X) -> lists:member(X, L1) end, L2).

%% Strip all leading and/or trailing white characters
trim(What) ->
    re:replace(What, "(^\\s+)|(\\s+$)", "", [global, {return, list}]).

print(Lines) -> [io:format(standard_error, X ++ "~n", []) || X <- Lines].

h_line(Msg) ->
    h_line(Msg, Msg, 80, $-).

h_line(Msg, Char) ->
    h_line(Msg, Msg, 80, Char).

h_line([$~, $n, T], Org, Line, Char) ->
    h_line(T, Org, Line + 2, Char);
h_line(Msg, Org, Line, Char) ->
    HLine = Msg ++ lists:duplicate(Line - length(Org), Char) ++ "~n",
    io:format(standard_io, HLine, []).

chmod_exe(File) ->
    io:format(standard_io, "Change mode of '~s' to ~.8B: ", [File, ?EXEMODE]),
    check_file_op(file:change_mode(File, ?EXEMODE)).

chmod(File, Mode) ->
    io:format(standard_io, "Change mode of '~s' to ~.8B: ", [File, Mode]),
    check_file_op(file:change_mode(File, Mode)).

mk_link(To, From) ->
    io:format(standard_io, "Create link to '~s' from '~s': ", [To, From]),
    check_file_op(file:make_symlink(To, From)).

mk_dir(Name) ->
    io:format(standard_io, "Create folder '~s': ", [Name]),
    check_file_op(file:make_dir(Name)).

rm_file(File) ->
    io:format(standard_io, "Remove file '~s': ", [File]),
    check_file_op(file:delete(File)).

rm_link(Link) ->
    io:format(standard_io, "Remove link '~s': ", [Link]),
    case file:read_link(Link) of
        {ok, _} ->
            check_file_op(file:delete(Link));
        {error, enoent} ->
            io:format(standard_io, "Doesn't exist, ignoring...~n", []);
        {error, einval} ->
            io:format(standard_io, "Not a link, aborting!~n", []),
            halt(1);
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end.

file_type(Name) ->
    case file:read_file_info(Name) of
        {ok, #file_info{type = regular}} -> file_type1(Name);
        {ok, #file_info{type = Type}}
          when Type =:= device;
               Type =:= directory;
               Type =:= other -> {ok, Type};
        {error, _} = Err -> Err
    end.

file_type1(Name) ->
    case file:read_link(Name) of
        {ok, _} -> {ok, link};
        {error, einval} -> {ok, file};
        {error, _} = Err -> Err
    end.

ensure_member(Elem, List) ->
    case lists:member(Elem, List) of
        true -> List;
        false -> [Elem | List]
    end.

%%------------------------------------------------------------------------------

get_rel_dir() ->
    {_, Vsn} = start_erl_data(),
    filename:join("releases", Vsn).


read_builderl_config() ->
    read_builderl_config(get_rel_dir()).

read_builderl_config(RelDir) ->
    CfgFile = filename:join(RelDir, ?BUILDERL_CONFIG),
    io:format("Reading '~s': ", [CfgFile]),
    case file:consult(CfgFile) of
        {ok, Cfg} ->
            io:format("OK~n~n"),
            Cfg;
        {error, Err} ->
            io:format("Error '~p', aborting.~n", [Err]),
            halt(1)
    end.


get_default_nodes(BldCfg) ->
    keyget(default_nodes, BldCfg, []).


get_default_joins(BldCfg) ->
    keyget(default_joins, BldCfg, []).


get_allowed(BldCfg) ->
    [atom_to_list(T) || {node_type, T, _, N, _, _} <- BldCfg, N =/= undefined].


get_params(BldCfg) ->
    {ok, SuffixRe} = re:compile(?SUFFIX_RE),
    {get_allowed(BldCfg), SuffixRe, BldCfg}.


%% The builderl config list is not a property list
keyget(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
        false -> Default;
        {Key, Value} -> Value
    end.

keyget(Key, List) ->
    keyget(Key, List, undefined).

%%------------------------------------------------------------------------------

start_node({Name, Base}, Suffix) ->
    Cmd = filename:join([Base, "bin", Name ++ Suffix]),
    io:format(standard_io, "Executing '~s'.~n", [Cmd]),
    Res = os:cmd(Cmd),
    io:format("Result: ~p~n~n", [Res]).


running_nodes() ->
    case net_adm:names() of
        {error, address} -> [];
        {ok, All} -> [Name || {Name, _} <- All]
    end.


connect_to_node(Dir) ->
    Cookie = trim(read_data(filename:join(Dir, ".erlang.cookie"))),
    {NameType, Hostname} = extract_host_name(Dir),
    Arg = [?NAME, NameType],
    Res = start_distribution(Arg),
    io:format("Started Net Kernel with Arg: ~p, Result: ~p~n", [Arg, Res]),
    io:format("Running as node:~p~n~n", [node()]),
    erlang:set_cookie(node(), list_to_atom(Cookie)),
    Remote = list_to_atom(Hostname),
    case net_adm:ping(Remote) of
        pang -> {false, Remote};
        pong -> {true, Remote}
    end.

extract_host_name(Dir) ->
    VmArgs = read_data(filename:join([Dir, "etc", "vm.args"])),
    Lines = binary:split(VmArgs, <<"\n">>, [global]),
    SName = <<"-sname">>,
    Name = <<"-name">>,
    case first_match([SName, Name], Lines) of
        {SName, Host} -> {shortnames, Host};
        {Name, Host} -> {longnames, Host}
    end.

first_match(ToFind, [H|T]) ->
    case binary:match(H, ToFind) of
        nomatch -> first_match(ToFind, T);
        {_, _} = Part -> extract_host(Part, H)
    end;
first_match(_, []) ->
    Msg = "Error, neither '-sname' nor '-name' found in 'vm.args', aborting!~n",
    io:format(Msg),
    halt(1).

extract_host({Start, Length} = Part, Line) ->
    NewStart = Start + Length + 1,
    Host = binary:part(Line, {NewStart, byte_size(Line) - NewStart}),
    {binary:part(Line, Part), trim(binary_to_list(Host))}.

start_distribution(Arg) ->
    error_logger:tty(false),
    io:format("Waiting for epmd to start...~n", []),
    start_distribution(Arg, ?EPMD_TIMEOUT, os:timestamp()),
    error_logger:tty(true).

start_distribution(Arg, Timeout, Start) ->
    case net_kernel:start(Arg) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> already_started;
        {error, Err} -> no_distribution(Arg, Timeout, Start, Err)
    end.

no_distribution(Arg, Timeout, Start, Err) ->
    case get_time_diff(Start) >= Timeout of
        true -> halt_no_distribution(Err);
        false -> ok
    end,
    timer:sleep(500),
    start_distribution(Arg, Timeout, Start).

halt_no_distribution(Err) ->
    Msg = "Error, couldn't switch to distributed node: ~p~n",
    io:format(standard_error, Msg, [Err]),
    halt(1).

get_time_diff(Start) ->
    timer:now_diff(os:timestamp(), Start) div 1000.

%%------------------------------------------------------------------------------

process_file([H|_T] = Src, Dest, Args) when is_list(H) ->
    process_file(filename:join(Src), Dest, Args);
process_file(Src, [H|_T] = Dest, Args) when is_list(H) ->
    process_file(Src, filename:join(Dest), Args);
process_file(Src, Dest, Args) ->
    case lists:keytake(options, 1, Args) of
        {value, {options, Opts}, Args1} -> process_file(Src, Dest, Args1, Opts);
        false -> process_file(Src, Dest, Args, [])
    end.

process_file(Src, Dest, Args, Options) ->
    Data = read_data(Src),
    io:format(standard_io, "Write file '~s': ", [Dest]),
    case not lists:member(force, Options) andalso filelib:is_file(Dest) of
        false ->
            process_file1(Data, Dest, Args);
        true ->
            io:format(standard_error, "Error, file already exists!~n", []),
            halt(1)
    end.

read_data(Src) ->
    case file:read_file(Src) of
        {ok, Data} ->
            Data;
        {error, Err} ->
            Msg = "Can not read file: '~s', Error: ~p~n",
            io:format(standard_error, Msg, [Src, Err]),
            halt(1)
    end.

process_file1(Data, Dest, []) ->
    case file:write_file(Dest, Data) of
        ok ->
            io:format(standard_io, "Done.~n", []);
        {error, Err} ->
            io:format(standard_io, "Error:~n~1000p~n", [Err]),
            halt(1)
    end;
process_file1(Data, Dest, Args) ->
    NewData = lists:foldl(fun(X, D) -> replace(X, D) end, Data, Args),
    process_file1(NewData, Dest, []).

replace({Name, Value}, Src) -> re:replace(Src, Name, Value);
replace({Name, Value, Opts}, Src) -> re:replace(Src, Name, Value, Opts).

%%------------------------------------------------------------------------------

cp_file(Src, Dst, File) ->
    cp_file(Src, Dst, File, []).


cp_file(Src, Dest, File, Args) ->
    SrcFile = filename:join(Src, File),
    DestFile = filename:join(Dest, File),
    process_file(SrcFile, DestFile, Args).


cp_dir(Src, Dest, Templates, CfgArgs) ->
    Msg = "Processing files from '~s'~n  to '~s'...~n",
    io:format(standard_io, Msg, [Src, Dest]),

    {Dirs0, Files, Tmpls} = relative_paths(Src, Templates),
    Dirs = lists:sort(Dirs0),
    lists:foreach(fun(X) -> mk_dir(filename:join(Dest, X)) end, Dirs),
    lists:foreach(fun(X) -> cp_file(Src, Dest, X, []) end, Files),
    lists:foreach(fun(X) -> cp_file(Src, Dest, X, CfgArgs) end, Tmpls),

    io:format(standard_io, "Done processing files from '~s'.~n", [Src]).

relative_paths(Src, Templates) ->
    Paths = bld_compat:wildcard(filename:join(Src, "**")),
    {Files, Dirs} = lists:partition(fun(X) -> filelib:is_regular(X) end, Paths),
    TmplFun = fun(X) -> lists:member(filename:basename(X), Templates) end,
    {Tmpls, Regular} = lists:partition(TmplFun, Files),

    Root = filename:split(Src),
    {subtract_root(Root, Dirs),
     subtract_root(Root, Regular),
     subtract_root(Root, Tmpls)}.

subtract_root(SrcElems, Paths) ->
    [filename:join(filename:split(X) -- SrcElems) || X <- Paths].

%%------------------------------------------------------------------------------

check_file_op(ok) ->
    io:format(standard_io, "Done.~n", []);
check_file_op({error, Err}) ->
    io:format(standard_io, "Error:~n~1000p~n", [Err]),
    halt(1).


write_terms(File, Terms) ->
    FormatFun = fun(Term) -> io_lib:format("~p.~n", [Term]) end,
    Texts = lists:map(FormatFun, Terms),
    ok = file:write_file(File, Texts).

%%------------------------------------------------------------------------------

call(Fun, ArgL) ->
    ReplyTo = self(),
    Keys = [spawn(fun() -> ReplyTo ! {self(), promise_reply, Fun(A)} end)
            || A <- ArgL],

    Yield = fun(Key) ->
                    receive
                        {Key, promise_reply, {error, _R} = E} -> E;
                        {Key, promise_reply, {'EXIT', {error, _R} = E}} -> E;
                        {Key, promise_reply, {'EXIT', R}} -> {error, R};
                        {Key, promise_reply, R} -> R
                    end
            end,
    [Yield(Key) || Key <- Keys].
