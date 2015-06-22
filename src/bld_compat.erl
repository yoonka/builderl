%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2011. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%

-module(bld_compat).

-export([
         filtermap/2,
         wildcard/1
        ]).

%%------------------------------------------------------------------------------
%% Copied from module lists in R17

filtermap(F, [Hd|Tail]) ->
    case F(Hd) of
        true ->
            [Hd|filtermap(F, Tail)];
        {true,Val} ->
            [Val|filtermap(F, Tail)];
        false ->
            filtermap(F, Tail)
    end;
filtermap(F, []) when is_function(F, 1) -> [].

%%------------------------------------------------------------------------------
%% Copied from module filelib in R17

-define(HANDLE_ERROR(Expr),
	try
	    Expr
	catch
	    error:{badpattern,_}=UnUsUalVaRiAbLeNaMe ->
		%% Get the stack backtrace correct.
		error(UnUsUalVaRiAbLeNaMe)
	end).

wildcard(Pattern) when is_list(Pattern) ->
    ?HANDLE_ERROR(do_wildcard(Pattern, ".", file)).

do_wildcard(Pattern, Cwd, Mod) ->
    {Compiled,PrefixLen} = compile_wildcard(Pattern, Cwd),
    Files0 = do_wildcard_1(Compiled, Mod),
    Files = if
		PrefixLen =:= 0 ->
		    Files0;
		true ->
		    [lists:nthtail(PrefixLen, File) || File <- Files0]
	    end,
    lists:sort(Files).

do_wildcard_1({exists,File}, Mod) ->
    case eval_read_link_info(File, Mod) of
	{ok,_} -> [File];
	_ -> []
    end;
do_wildcard_1([Base|Rest], Mod) ->
    do_wildcard_2([Base], Rest, [], Mod).

do_wildcard_2([File|Rest], Pattern, Result, Mod) ->
    do_wildcard_2(Rest, Pattern, do_wildcard_3(File, Pattern, Result, Mod), Mod);
do_wildcard_2([], _, Result, _Mod) ->
    Result.

do_wildcard_3(Base, [[double_star]|Rest], Result, Mod) ->
    do_double_star(".", [Base], Rest, Result, Mod, true);
do_wildcard_3(Base0, [Pattern|Rest], Result, Mod) ->
    case do_list_dir(Base0, Mod) of
	{ok, Files} ->
	    Base = prepare_base(Base0),
	    Matches = do_wildcard_4(Pattern, Base, Files),
	    do_wildcard_2(Matches, Rest, Result, Mod);
	_ ->
	    Result
    end;
do_wildcard_3(Base, [], Result, _Mod) ->
    [Base|Result].

do_wildcard_4(Pattern, Base, Files) ->
    case will_always_match(Pattern) of
	false ->
	    [Base++F || F <- Files, match_part(Pattern, F)];
	true ->
	    [Base++F || F <- Files]
    end.

match_part([question|Rest1], [_|Rest2]) ->
    match_part(Rest1, Rest2);
match_part([accept], _) ->
    true;
match_part([double_star], _) ->
    true;
match_part([star|Rest], File) ->
    do_star(Rest, File);
match_part([{one_of, Ordset}|Rest], [C|File]) ->
    gb_sets:is_element(C, Ordset) andalso match_part(Rest, File);
match_part([{alt, Alts}], File) ->
    do_alt(Alts, File);
match_part([C|Rest1], [C|Rest2]) when is_integer(C) ->
    match_part(Rest1, Rest2);
match_part([X|_], [Y|_]) when is_integer(X), is_integer(Y) ->
    false;
match_part([], []) ->
    true;
match_part([], [_|_]) ->
    false;
match_part([_|_], []) ->
    false.

will_always_match([accept]) -> true;
will_always_match(_) -> false.

prepare_base(Base0) ->
    Base1 = filename:join(Base0, "x"),
    "x"++Base2 = lists:reverse(Base1),
    lists:reverse(Base2).

do_double_star(Base, [H|T], Rest, Result, Mod, Root) ->
    Full = case Root of
	       false -> filename:join(Base, H);
	       true -> H
	   end,
    Result1 = case do_list_dir(Full, Mod) of
        {ok, Files} ->
            do_double_star(Full, Files, Rest, Result, Mod, false);
        _ -> Result
    end,
    Result2 = case Root andalso Rest == [] of
        true  -> Result1;
        false -> do_wildcard_3(Full, Rest, Result1, Mod)
    end,
    do_double_star(Base, T, Rest, Result2, Mod, Root);
do_double_star(_Base, [], _Rest, Result, _Mod, _Root) ->
    Result.

do_star(Pattern, [_|Rest]=File) ->
    match_part(Pattern, File) orelse do_star(Pattern, Rest);
do_star(Pattern, []) ->
    match_part(Pattern, []).

do_alt([Alt|Rest], File) ->
    match_part(Alt, File) orelse do_alt(Rest, File);
do_alt([], _File) ->
    false.

do_list_dir(Dir, Mod) ->     eval_list_dir(Dir, Mod).

	    
%%% Compiling a wildcard.

compile_wildcard(Pattern, Cwd0) ->
    [Root|Rest] = filename:split(Pattern),
    case filename:pathtype(Root) of
	relative ->
	    Cwd = prepare_base(Cwd0),
	    compile_wildcard_2([Root|Rest], {cwd,Cwd});
	_ ->
	    compile_wildcard_2(Rest, {root,0,Root})
    end.

compile_wildcard_2([Part|Rest], Root) ->
    case compile_part(Part) of
	Part ->
	    compile_wildcard_2(Rest, compile_join(Root, Part));
	Pattern ->
	    compile_wildcard_3(Rest, [Pattern,Root])
    end;
compile_wildcard_2([], {root,PrefixLen,Root}) ->
    {{exists,Root},PrefixLen}.

compile_wildcard_3([Part|Rest], Result) ->
    compile_wildcard_3(Rest, [compile_part(Part)|Result]);
compile_wildcard_3([], Result) ->
    case lists:reverse(Result) of
	[{root,PrefixLen,Root}|Compiled] ->
	    {[Root|Compiled],PrefixLen};
	[{cwd,Root}|Compiled] ->
	    {[Root|Compiled],length(filename:join(Root, "x"))-1}
    end.

compile_join({cwd,"."}, File) ->
    {root,0,File};
compile_join({cwd,Cwd}, File0) ->
    File = filename:join([File0]),
    Root = filename:join(Cwd, File),
    PrefixLen = length(Root) - length(File),
    {root,PrefixLen,Root};
compile_join({root,PrefixLen,Root}, File) ->
    {root,PrefixLen,filename:join(Root, File)}.

compile_part(Part) ->
    compile_part(Part, false, []).

compile_part_to_sep(Part) ->
    compile_part(Part, true, []).

compile_part([], true, _) ->
    badpattern(missing_delimiter);
compile_part([$,|Rest], true, Result) ->
    {ok, $,, lists:reverse(Result), Rest};
compile_part([$}|Rest], true, Result) ->
    {ok, $}, lists:reverse(Result), Rest};
compile_part([$?|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [question|Result]);
compile_part([$*,$*], Upto, Result) ->
    compile_part([], Upto, [double_star|Result]);
compile_part([$*,$*|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [star|Result]);
compile_part([$*], Upto, Result) ->
    compile_part([], Upto, [accept|Result]);
compile_part([$*|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [star|Result]);
compile_part([$[|Rest], Upto, Result) ->
    case compile_charset(Rest, ordsets:new()) of
	{ok, Charset, Rest1} ->
	    compile_part(Rest1, Upto, [Charset|Result]);
	error ->
	    compile_part(Rest, Upto, [$[|Result])
    end;
compile_part([${|Rest], Upto, Result) ->
    case compile_alt(Rest) of
	{ok, Alt} ->
	    lists:reverse(Result, [Alt]);
	error ->
	    compile_part(Rest, Upto, [${|Result])
    end;
compile_part([X|Rest], Upto, Result) ->
    compile_part(Rest, Upto, [X|Result]);
compile_part([], _Upto, Result) ->
    lists:reverse(Result).

compile_charset([$]|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element($], Ordset));
compile_charset([], _Ordset) ->
    error;
compile_charset(List, Ordset) ->
    compile_charset1(List, Ordset).

compile_charset1([Lower, $-, Upper|Rest], Ordset) when Lower =< Upper ->
    compile_charset1(Rest, compile_range(Lower, Upper, Ordset));
compile_charset1([$]|Rest], Ordset) ->
    {ok, {one_of, gb_sets:from_ordset(Ordset)}, Rest};
compile_charset1([X|Rest], Ordset) ->
    compile_charset1(Rest, ordsets:add_element(X, Ordset));
compile_charset1([], _Ordset) ->
    error.
    
compile_range(Lower, Current, Ordset) when Lower =< Current ->
    compile_range(Lower, Current-1, ordsets:add_element(Current, Ordset));
compile_range(_, _, Ordset) ->
    Ordset.

compile_alt(Pattern) ->
    compile_alt(Pattern, []).

compile_alt(Pattern, Result) ->
    case compile_part_to_sep(Pattern) of
	{ok, $,, AltPattern, Rest} ->
	    compile_alt(Rest, [AltPattern|Result]);
	{ok, $}, AltPattern, Rest} ->
	    NewResult = [AltPattern|Result],
	    RestPattern = compile_part(Rest),
	    {ok, {alt, [Alt++RestPattern || Alt <- NewResult]}};
	Pattern ->
	    error
    end.

badpattern(Reason) ->
    error({badpattern,Reason}).

eval_read_link_info(File, file) ->
    file:read_link_info(File);
eval_read_link_info(File, erl_prim_loader) ->
    case erl_prim_loader:read_link_info(File) of
        error -> {error, erl_prim_loader};
        Res-> Res
    end;
eval_read_link_info(File, Mod) ->
    Mod:read_link_info(File).

eval_list_dir(Dir, file) ->
    file:list_dir(Dir);
eval_list_dir(Dir, erl_prim_loader) ->
    case erl_prim_loader:list_dir(Dir) of
	error -> {error, erl_prim_loader};
	Res-> Res
    end;
eval_list_dir(Dir, Mod) ->
    Mod:list_dir(Dir).

%% End of copied from module filelib in R17
%%------------------------------------------------------------------------------
