%% -*- erlang -*-

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

-export([load_builderl/0]).

%% This module requires the link to the current version of builderl.
%% The link is always in the bin folder of the project's root folder.
-define(BUILDERLROOT, "bin").
-define(BUILDERLLINK, filename:join(?BUILDERLROOT, "builderl")).
-define(BOOTMODULE, "bld_make").

load_builderl() ->
    {ok, Cwd} = file:get_cwd(),
    DepPath = filename:join(Cwd, ?BUILDERLLINK),

    %% Just try to load and compile on-the-fly if needed
    %% Proper compilation will be done in bld_make anyway
    case code:load_abs(filename:join([DepPath, "ebin", ?BOOTMODULE])) of
        {module, _Mod} ->
            ok;
        _ ->
            File = filename:join([Cwd, ?BUILDERLLINK, "src", ?BOOTMODULE]),
            case compile:file(File, [binary, report]) of
                {ok, Mod, Bin} ->
                    {module, _Mod} = code:load_binary(Mod, File ++ ".erl", Bin);
                Err ->
                    io:format("Error when loading '~s': ~p~n.", [File, Err]),
                    halt(1)
            end
    end,

    bld_make:boot(DepPath, ?BUILDERLROOT),

    io:format("== Using 'builderl' from '~s' ==~n", [DepPath]).
