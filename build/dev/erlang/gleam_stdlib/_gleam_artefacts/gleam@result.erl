-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1, partition/1]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BDJ} | {error, BDK}, fun((BDJ) -> BDN)) -> {ok, BDN} |
    {error, BDK}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BDQ} | {error, BDR}, fun((BDR) -> BDU)) -> {ok, BDQ} |
    {error, BDU}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BDX} | {error, BDY}} | {error, BDY}) -> {ok, BDX} |
    {error, BDY}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BEF} | {error, BEG}, fun((BEF) -> {ok, BEJ} | {error, BEG})) -> {ok,
        BEJ} |
    {error, BEG}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BEO} | {error, BEP}, fun((BEO) -> {ok, BES} | {error, BEP})) -> {ok,
        BES} |
    {error, BEP}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BEX} | {error, any()}, BEX) -> BEX.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BFB} | {error, any()}, fun(() -> BFB)) -> BFB.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BFG}, BFG) -> BFG.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BFJ} | {error, BFJ}) -> BFJ.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BFM} | {error, any()}) -> {ok, BFM} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BFS} | {error, BFT}, {ok, BFS} | {error, BFT}) -> {ok, BFS} |
    {error, BFT}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BGA} | {error, BGB}, fun(() -> {ok, BGA} | {error, BGB})) -> {ok,
        BGA} |
    {error, BGB}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BGI} | {error, BGJ})) -> {ok, list(BGI)} | {error, BGJ}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, BHG}, BHJ) -> {ok, BHJ} | {error, BHG}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BHM} | {error, any()}, BHQ) -> {ok, BHM} | {error, BHQ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BHT} | {error, any()})) -> list(BHT).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec do_partition(list({ok, BGX} | {error, BGY}), list(BGX), list(BGY)) -> {list(BGX),
    list(BGY)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BGQ} | {error, BGR})) -> {list(BGQ), list(BGR)}.
partition(Results) ->
    do_partition(Results, [], []).
