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

-export([current_app_vsn/1]).

-include_lib("builderl/include/load_builderl.hrl").

-define(BUILDERLAPP, "builderl.app").
-define(SKEL_DIR, ["priv", "skel"]).

-define(DEL_LINKS, ["config", "configure", "migresia", "deps", "init", "mk_dev",
                    "mk_rel", "start", "stop", "update_root_dir"]).
-define(BLD_LINKS, ?DEL_LINKS).

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

current_app_vsn(Path) -> "builderl-" ++ get_app_vsn(Path).

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
    Path = filename:join(?BUILDERLLINK, "ebin"),
    io:format(standard_io, "~s~n", [get_app_vsn(Path)]).

get_app_vsn(Path) ->
    File = filename:join(Path, ?BUILDERLAPP),
    [{application, builderl, List}] = bld_lib:consult_app_file(File),
    proplists:get_value(vsn, List).

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

%%------------------------------------------------------------------------------
