-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

-spec first({FL, any()}) -> FL.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), FO}) -> FO.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({FP, FQ}) -> {FQ, FP}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({FR, FS}, fun((FR) -> FT)) -> {FT, FS}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({FU, FV}, fun((FV) -> FW)) -> {FU, FW}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
