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

-spec map({ok, BHF} | {error, BHG}, fun((BHF) -> BHJ)) -> {ok, BHJ} |
    {error, BHG}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BHM} | {error, BHN}, fun((BHN) -> BHQ)) -> {ok, BHM} |
    {error, BHQ}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BHT} | {error, BHU}} | {error, BHU}) -> {ok, BHT} |
    {error, BHU}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BIB} | {error, BIC}, fun((BIB) -> {ok, BIF} | {error, BIC})) -> {ok,
        BIF} |
    {error, BIC}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BIK} | {error, BIL}, fun((BIK) -> {ok, BIO} | {error, BIL})) -> {ok,
        BIO} |
    {error, BIL}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BIT} | {error, any()}, BIT) -> BIT.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BIX} | {error, any()}, fun(() -> BIX)) -> BIX.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BJC}, BJC) -> BJC.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BJF} | {error, BJF}) -> BJF.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BJI} | {error, any()}) -> {ok, BJI} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BJO} | {error, BJP}, {ok, BJO} | {error, BJP}) -> {ok, BJO} |
    {error, BJP}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BJW} | {error, BJX}, fun(() -> {ok, BJW} | {error, BJX})) -> {ok,
        BJW} |
    {error, BJX}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BKE} | {error, BKF})) -> {ok, list(BKE)} | {error, BKF}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, BLC}, BLF) -> {ok, BLF} | {error, BLC}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BLI} | {error, any()}, BLM) -> {ok, BLI} | {error, BLM}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BLP} | {error, any()})) -> list(BLP).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec do_partition(list({ok, BKT} | {error, BKU}), list(BKT), list(BKU)) -> {list(BKT),
    list(BKU)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BKM} | {error, BKN})) -> {list(BKM), list(BKN)}.
partition(Results) ->
    do_partition(Results, [], []).
