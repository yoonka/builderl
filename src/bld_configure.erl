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

-module(bld_configure).

-export([
         do/3
        ]).

do(Cwd, {Node, DestName, Dir}, OldArgs) ->
    io:format(standard_io, "Configure '~s' in '~s'.~n", [Node, Dir]),

    ok = file:set_cwd(Dir),
    {ok, Root} = file:get_cwd(),
    io:format(standard_io, "Using '~s' as node root.~n", [Root]),

    Args = [{<<"{{pipe_dir}}">>, "/var/run/" ++ DestName ++ "/"},
            {<<"{{abs_node_root}}">>, Root, [global]},
            {<<"{{node_cmd_name}}">>, DestName} | OldArgs],

    ok = file:set_cwd("bin"),
    bld_lib:process_file("env.sh.src", "env.sh", Args, [force]),
    bld_lib:process_file("runner.src", "runner", Args, [force]),
    bld_lib:chmod_exe("env.sh"),
    bld_lib:chmod_exe("runner"),

    ok = file:set_cwd(Root),
    SrvSrc = filename:join(["etc", "init.d", "daemon.src"]),
    SrvDst = filename:join(["etc", "init.d", DestName]),
    bld_lib:process_file(SrvSrc, SrvDst, Args, [force]),
    bld_lib:chmod(SrvDst, 8#00755),

    ok = file:set_cwd(Cwd),
    io:format(standard_io, "Finished.~n~n", []),
    [io:format(standard_io, X ++ "~n", []) || X <- link_info(Root, DestName)].

link_info(Folder, Name) ->
    [
     "Now please create this link:",
     "ln -s " ++ Folder ++ "/bin/runner /usr/sbin/" ++ Name,
     "",
     "And copy this file '" ++ Folder ++ "/etc/init.d/"
     ++ Name ++ "' to '/etc/init.d/'",
     ""
    ].
