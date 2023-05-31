-module(gleam@list).
-compile([no_auto_import, nowarn_unused_vars]).

-export([is_empty/1, first/1, rest/1, new/0, prepend/2, length/1, reverse/1, append/2, contains/2, map/2, fold/3, group/2, map_fold/3, reduce/2, last/1, filter/2, filter_map/2, index_map/2, try_map/2, drop/2, at/2, take/2, flatten/1, flat_map/2, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, key_find/2, all/2, any/2, zip/2, strict_zip/2, window_by_2/1, unzip/1, intersperse/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, try_each/2, partition/2, permutations/1, window/2, drop_while/2, take_while/2, chunk/2, sized_chunk/2, scan/3, combinations/2, combination_pairs/1, transpose/1, interleave/1, shuffle/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(SW) :: {continue, SW} | {stop, SW}.

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec first(list(TG)) -> {ok, TG} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _] ->
            {ok, X}
    end.

-spec rest(list(TK)) -> {ok, list(TK)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_ | Xs] ->
            {ok, Xs}
    end.

-spec new() -> list(any()).
new() ->
    [].

-spec prepend(list(XF), XF) -> list(XF).
prepend(List, Item) ->
    [Item | List].

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(SZ)) -> list(SZ).
reverse(Xs) ->
    lists:reverse(Xs).

-spec append(list(XB), list(XB)) -> list(XB).
append(First, Second) ->
    lists:append(First, Second).

-spec contains(list(TE), TE) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | _] when Head =:= Elem ->
            true;

        [_ | Tail] ->
            contains(Tail, Elem)
    end.

-spec do_map(list(UZ), fun((UZ) -> VB), list(VB)) -> list(VB).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(VE), fun((VE) -> VG)) -> list(VG).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec update_group(fun((TP) -> TQ)) -> fun((gleam@map:map_(TQ, list(TP)), TP) -> gleam@map:map_(TQ, list(TP))).
update_group(F) ->
    fun(Groups, Elem) -> case gleam@map:get(Groups, F(Elem)) of
            {ok, Existing} ->
                gleam@map:insert(Groups, F(Elem), [Elem | Existing]);

            {error, _} ->
                gleam@map:insert(Groups, F(Elem), [Elem])
        end end.

-spec fold(list(YA), YC, fun((YC, YA) -> YC)) -> YC.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec group(list(TX), fun((TX) -> TZ)) -> gleam@map:map_(TZ, list(TX)).
group(List, Key) ->
    fold(List, gleam@map:new(), update_group(Key)).

-spec map_fold(list(VI), VK, fun((VK, VI) -> {VK, VL})) -> {VK, list(VL)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec reduce(list(AHX), fun((AHX, AHX) -> AHX)) -> {ok, AHX} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec last(list(AIK)) -> {ok, AIK} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec do_filter(list(UD), fun((UD) -> boolean()), list(UD)) -> list(UD).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(UH), fun((UH) -> boolean())) -> list(UH).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(UK), fun((UK) -> {ok, UM} | {error, any()}), list(UM)) -> list(UM).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(US), fun((US) -> {ok, UU} | {error, any()})) -> list(UU).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_index_map(list(VN), fun((integer(), VN) -> VP), integer(), list(VP)) -> list(VP).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(VS), fun((integer(), VS) -> VU)) -> list(VU).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(VW), fun((VW) -> {ok, VY} | {error, VZ}), list(VY)) -> {ok,
        list(VY)} |
    {error, VZ}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(WG), fun((WG) -> {ok, WI} | {error, WJ})) -> {ok, list(WI)} |
    {error, WJ}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(WP), integer()) -> list(WP).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_ | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec at(list(ABW), integer()) -> {ok, ABW} | {error, nil}.
at(List, Index) ->
    case Index >= 0 of
        true ->
            _pipe = List,
            _pipe@1 = drop(_pipe, Index),
            first(_pipe@1);

        false ->
            {error, nil}
    end.

-spec do_take(list(WS), integer(), list(WS)) -> list(WS).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(WW), integer()) -> list(WW).
take(List, N) ->
    do_take(List, N, []).

-spec reverse_and_prepend(list(XI), list(XI)) -> list(XI).
reverse_and_prepend(Prefix, Suffix) ->
    case Prefix of
        [] ->
            Suffix;

        [Head | Tail] ->
            reverse_and_prepend(Tail, [Head | Suffix])
    end.

-spec do_flatten(list(list(XM)), list(XM)) -> list(XM).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            reverse(Acc);

        [List | Further_lists] ->
            do_flatten(Further_lists, reverse_and_prepend(List, Acc))
    end.

-spec flatten(list(list(XR))) -> list(XR).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(XV), fun((XV) -> list(XX))) -> list(XX).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold_right(list(YD), YF, fun((YF, YD) -> YF)) -> YF.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(YG), YI, fun((YI, YG, integer()) -> YI), integer()) -> YI.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(YJ), YL, fun((YL, YJ, integer()) -> YL)) -> YL.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(YM), YO, fun((YO, YM) -> {ok, YO} | {error, YP})) -> {ok,
        YO} |
    {error, YP}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {ok, Result} ->
                    try_fold(Rest, Result, Fun);

                {error, _} = Error ->
                    Error
            end
    end.

-spec fold_until(list(YU), YW, fun((YW, YU) -> continue_or_stop(YW))) -> YW.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(YY), fun((YY) -> boolean())) -> {ok, YY} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _ ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(AAC), fun((AAC) -> {ok, AAE} | {error, any()})) -> {ok, AAE} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _ ->
                    find_map(Rest, Fun)
            end
    end.

-spec key_find(list({ADT, ADU}), ADT) -> {ok, ADU} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec all(list(AAK), fun((AAK) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    all(Tail, Predicate);

                false ->
                    false
            end
    end.

-spec any(list(AAM), fun((AAM) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    true;

                false ->
                    any(Tail, Predicate)
            end
    end.

-spec do_zip(list(AAO), list(AAQ), list({AAO, AAQ})) -> list({AAO, AAQ}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_, _} ->
            reverse(Acc)
    end.

-spec zip(list(AAU), list(AAW)) -> list({AAU, AAW}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(AAZ), list(ABB)) -> {ok, list({AAZ, ABB})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec window_by_2(list(AGM)) -> list({AGM, AGM}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec do_unzip(list({ABK, ABL}), list(ABK), list(ABL)) -> {list(ABK), list(ABL)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({ABK, ABL})) -> {list(ABK), list(ABL)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(ABP), ABP, list(ABP)) -> list(ABP).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(ABT), ABT) -> list(ABT).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec unique(list(ACA)) -> list(ACA).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_up(
    integer(),
    integer(),
    list(ACD),
    list(ACD),
    list(ACD),
    fun((ACD, ACD) -> gleam@order:order())
) -> list(ACD).
merge_up(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_up(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_up(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Ax@1, Bx@1) of
                gt ->
                    merge_up(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare);

                _ ->
                    merge_up(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare)
            end
    end.

-spec merge_down(
    integer(),
    integer(),
    list(ACI),
    list(ACI),
    list(ACI),
    fun((ACI, ACI) -> gleam@order:order())
) -> list(ACI).
merge_down(Na, Nb, A, B, Acc, Compare) ->
    case {Na, Nb, A, B} of
        {0, 0, _, _} ->
            Acc;

        {_, 0, [Ax | Ar], _} ->
            merge_down(Na - 1, Nb, Ar, B, [Ax | Acc], Compare);

        {0, _, _, [Bx | Br]} ->
            merge_down(Na, Nb - 1, A, Br, [Bx | Acc], Compare);

        {_, _, [Ax@1 | Ar@1], [Bx@1 | Br@1]} ->
            case Compare(Bx@1, Ax@1) of
                lt ->
                    merge_down(Na - 1, Nb, Ar@1, B, [Ax@1 | Acc], Compare);

                _ ->
                    merge_down(Na, Nb - 1, A, Br@1, [Bx@1 | Acc], Compare)
            end
    end.

-spec merge_sort(
    list(ACN),
    integer(),
    fun((ACN, ACN) -> gleam@order:order()),
    boolean()
) -> list(ACN).
merge_sort(L, Ln, Compare, Down) ->
    N = Ln div 2,
    A = L,
    B = drop(L, N),
    case Ln < 3 of
        true ->
            case Down of
                true ->
                    merge_down(N, Ln - N, A, B, [], Compare);

                false ->
                    merge_up(N, Ln - N, A, B, [], Compare)
            end;

        false ->
            case Down of
                true ->
                    merge_down(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, false),
                        merge_sort(B, Ln - N, Compare, false),
                        [],
                        Compare
                    );

                false ->
                    merge_up(
                        N,
                        Ln - N,
                        merge_sort(A, N, Compare, true),
                        merge_sort(B, Ln - N, Compare, true),
                        [],
                        Compare
                    )
            end
    end.

-spec sort(list(ACQ), fun((ACQ, ACQ) -> gleam@order:order())) -> list(ACQ).
sort(List, Compare) ->
    merge_sort(List, length(List), Compare, true).

-spec do_shuffle_by_pair_indexes(list({float(), AJM})) -> list({float(), AJM}).
do_shuffle_by_pair_indexes(List_of_pairs) ->
    sort(
        List_of_pairs,
        fun(A_pair, B_pair) ->
            gleam@float:compare(
                erlang:element(1, A_pair),
                erlang:element(1, B_pair)
            )
        end
    ).

-spec tail_recursive_range(integer(), integer(), list(integer())) -> list(integer()).
tail_recursive_range(Start, Stop, Acc) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            reverse([Stop | Acc]);

        gt ->
            tail_recursive_range(Start - 1, Stop, [Start | Acc]);

        lt ->
            tail_recursive_range(Start + 1, Stop, [Start | Acc])
    end.

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    tail_recursive_range(Start, Stop, []).

-spec do_repeat(ACW, integer(), list(ACW)) -> list(ACW).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(ACZ, integer()) -> list(ACZ).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(ADB), integer(), list(ADB)) -> {list(ADB), list(ADB)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(ADG), integer()) -> {list(ADG), list(ADG)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(ADK), fun((ADK) -> boolean()), list(ADK)) -> {list(ADK),
    list(ADK)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _ ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(ADP), fun((ADP) -> boolean())) -> {list(ADP), list(ADP)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec do_pop(list(AEC), fun((AEC) -> boolean()), list(AEC)) -> {ok,
        {AEC, list(AEC)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(AEC), fun((AEC) -> boolean())) -> {ok, {AEC, list(AEC)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(AEL), fun((AEL) -> {ok, AEN} | {error, any()}), list(AEL)) -> {ok,
        {AEN, list(AEL)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(AEL), fun((AEL) -> {ok, AEN} | {error, any()})) -> {ok,
        {AEN, list(AEL)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({AEU, AEV}), AEU) -> {ok, {AEV, list({AEU, AEV})}} |
    {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _ ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({AFA, AFB}), AFA, AFB) -> list({AFA, AFB}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(AFE), fun((AFE) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec try_each(list(AFH), fun((AFH) -> {ok, any()} | {error, AFK})) -> {ok, nil} |
    {error, AFK}.
try_each(List, Fun) ->
    case List of
        [] ->
            {ok, nil};

        [X | Xs] ->
            case Fun(X) of
                {ok, _} ->
                    try_each(Xs, Fun);

                {error, E} ->
                    {error, E}
            end
    end.

-spec do_partition(list(AFU), fun((AFU) -> boolean()), list(AFU), list(AFU)) -> {list(AFU),
    list(AFU)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(AFU), fun((AFU) -> boolean())) -> {list(AFU), list(AFU)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(AFY)) -> list(list(AFY)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _ ->
            _pipe = L,
            _pipe@5 = index_map(_pipe, fun(I_idx, I) -> _pipe@1 = L,
                    _pipe@2 = index_fold(
                        _pipe@1,
                        [],
                        fun(Acc, J, J_idx) -> case I_idx =:= J_idx of
                                true ->
                                    Acc;

                                false ->
                                    [J | Acc]
                            end end
                    ),
                    _pipe@3 = reverse(_pipe@2),
                    _pipe@4 = permutations(_pipe@3),
                    map(_pipe@4, fun(Permutation) -> [I | Permutation] end) end),
            flatten(_pipe@5)
    end.

-spec do_window(list(list(AGC)), list(AGC), integer()) -> list(list(AGC)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(AGI), integer()) -> list(list(AGI)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec drop_while(list(AGP), fun((AGP) -> boolean())) -> list(AGP).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(AGS), fun((AGS) -> boolean()), list(AGS)) -> list(AGS).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    do_take_while(Tail, Predicate, [Head | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(AGW), fun((AGW) -> boolean())) -> list(AGW).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(AGZ), fun((AGZ) -> AHB), AHB, list(AGZ), list(list(AGZ))) -> list(list(AGZ)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [Head | Tail] ->
            Key = F(Head),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Tail, F, Key, [Head], New_acc);

                _ ->
                    do_chunk(Tail, F, Key, [Head | Current_chunk], Acc)
            end;

        _ ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(AHH), fun((AHH) -> any())) -> list(list(AHH)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(
    list(AHM),
    integer(),
    integer(),
    list(AHM),
    list(list(AHM))
) -> list(list(AHM)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [Head | Tail] ->
            Chunk = [Head | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Tail,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Tail, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(AHT), integer()) -> list(list(AHT)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec do_scan(list(AIB), AID, list(AID), fun((AID, AIB) -> AID)) -> list(AID).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(AIG), AII, fun((AII, AIG) -> AII)) -> list(AII).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec combinations(list(AIO), integer()) -> list(list(AIO)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _ ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(AIS)) -> list(list({AIS, AIS})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(AIW)) -> list({AIW, AIW}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec transpose(list(list(AJD))) -> list(list(AJD)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.

-spec interleave(list(list(AIZ))) -> list(AIZ).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec do_shuffle_pair_unwrap(list({float(), AJI}), list(AJI)) -> list(AJI).
do_shuffle_pair_unwrap(List, Acc) ->
    case List of
        [] ->
            Acc;

        _ ->
            [Elem_pair | Enumerable] = List,
            do_shuffle_pair_unwrap(
                Enumerable,
                [erlang:element(2, Elem_pair) | Acc]
            )
    end.

-spec shuffle(list(AJP)) -> list(AJP).
shuffle(List) ->
    _pipe = List,
    _pipe@1 = fold(
        _pipe,
        [],
        fun(Acc, A) -> [{gleam@float:random(0.0, 1.0), A} | Acc] end
    ),
    _pipe@2 = do_shuffle_by_pair_indexes(_pipe@1),
    do_shuffle_pair_unwrap(_pipe@2, []).
