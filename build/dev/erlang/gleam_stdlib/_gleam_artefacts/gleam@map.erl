-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, update/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(KV, KW)) -> list({KV, KW}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({LA, LB})) -> map_(LA, LB).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(LF, any()), LF) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(LN, LO), LN) -> {ok, LO} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(LT, LU), LT, LU) -> map_(LT, LU).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec update(map_(NY, NZ), NY, fun((gleam@option:option(NZ)) -> NZ)) -> map_(NY, NZ).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec map_values(map_(LZ, MA), fun((LZ, MA) -> MD)) -> map_(LZ, MD).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(MG, any())) -> list(MG).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), MM)) -> list(MM).
values(Map) ->
    maps:values(Map).

-spec filter(map_(MQ, MR), fun((MQ, MR) -> boolean())) -> map_(MQ, MR).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(MW, MX), list(MW)) -> map_(MW, MX).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(ND, NE), map_(ND, NE)) -> map_(ND, NE).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(NL, NM), NL) -> map_(NL, NM).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(NR, NS), list(NR)) -> map_(NR, NS).
drop(Map, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Map;

        [X | Xs] ->
            drop(delete(Map, X), Xs)
    end.

-spec do_fold(list({OF, OG}), OI, fun((OI, OF, OG) -> OI)) -> OI.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(OJ, OK), ON, fun((ON, OJ, OK) -> ON)) -> ON.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
