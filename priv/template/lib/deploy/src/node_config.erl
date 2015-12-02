-module(node_config).

-export([install/2]).

log(F0, A0) ->
    F1 = "~p:~p: " ++ F0 ++ "~n",
    A1 = [?MODULE, ?LINE] ++ A0,
    io:format(F1, A1).

install({_Id, Node}, SetupCfg) ->
    Config = bld_cfg:load_config(SetupCfg),
    log("Config file: ~p~n", [Config]),
    ok = mnesia:create_schema([Node]).
