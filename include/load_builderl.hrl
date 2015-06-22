%% -*- erlang -*-
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

-export([
         load_builderl/0
        ]).

%% This module requires the link to the current version of builderl.
%% The link is always in the bin folder of the project's root folder.
-define(BUILDERLROOT, "bin").
-define(BUILDERLLINK, filename:join(?BUILDERLROOT, "builderl")).
-define(BLDLOADMOD, "bld_load").

load_builderl() ->
    {ok, Cwd} = file:get_cwd(),
    SrcPath = filename:join([Cwd, ?BUILDERLLINK, "src"]),
    DstPath = filename:join([Cwd, ?BUILDERLLINK, "ebin"]),
    io:format("*** Loading 'builderl' from '~s' ***~n", [DstPath]),

    %% Just try to load and compile on-the-fly if needed
    %% Proper compilation will be done in bld_load anyway
    case code:load_abs(filename:join(DstPath, ?BLDLOADMOD)) of
        {module, Mod} ->
            io:format("Pre-loaded: ~p~n", [Mod]);
        _ ->
            File = filename:join(SrcPath, ?BLDLOADMOD),
            case compile:file(File, [binary, report]) of
                {ok, Mod, Bin} ->
                    {module, Mod} = code:load_binary(Mod, File ++ ".erl", Bin),
                    io:format("Compiled and loaded: '~p'.~n", [Mod]);
                Err ->
                    io:format("Error when loading '~s': ~p~n.", [File, Err]),
                    halt(1)
            end
    end,

    bld_load:boot(SrcPath, DstPath, ?BUILDERLROOT),

    io:format("*** Loading 'builderl' finished. ***~n~n").
