-module(gleam@map).
-compile([no_auto_import, nowarn_unused_vars]).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, update/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(KD, KE)) -> list({KD, KE}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({KI, KJ})) -> map_(KI, KJ).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(KN, any()), KN) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(KV, KW), KV) -> {ok, KW} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(LB, LC), LB, LC) -> map_(LB, LC).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec update(map_(NG, NH), NG, fun((gleam@option:option(NH)) -> NH)) -> map_(NG, NH).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec map_values(map_(LH, LI), fun((LH, LI) -> LL)) -> map_(LH, LL).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(LO, any())) -> list(LO).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), LU)) -> list(LU).
values(Map) ->
    maps:values(Map).

-spec filter(map_(LY, LZ), fun((LY, LZ) -> boolean())) -> map_(LY, LZ).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(ME, MF), list(ME)) -> map_(ME, MF).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(ML, MM), map_(ML, MM)) -> map_(ML, MM).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(MT, MU), MT) -> map_(MT, MU).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(MZ, NA), list(MZ)) -> map_(MZ, NA).
drop(Map, Disallowed_keys) ->
    case Disallowed_keys of
        [] ->
            Map;

        [X | Xs] ->
            drop(delete(Map, X), Xs)
    end.

-spec do_fold(list({NN, NO}), NQ, fun((NQ, NN, NO) -> NQ)) -> NQ.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(NR, NS), NV, fun((NV, NR, NS) -> NV)) -> NV.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).