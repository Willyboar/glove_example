-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EKO) -> EKP), fun((EKP) -> EKQ)) -> fun((EKO) -> EKQ).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EKR, EKS) -> EKT)) -> fun((EKR) -> fun((EKS) -> EKT)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EKV, EKW, EKX) -> EKY)) -> fun((EKV) -> fun((EKW) -> fun((EKX) -> EKY))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((ELA, ELB, ELC, ELD) -> ELE)) -> fun((ELA) -> fun((ELB) -> fun((ELC) -> fun((ELD) -> ELE)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((ELG, ELH, ELI, ELJ, ELK) -> ELL)) -> fun((ELG) -> fun((ELH) -> fun((ELI) -> fun((ELJ) -> fun((ELK) -> ELL))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((ELN, ELO, ELP, ELQ, ELR, ELS) -> ELT)) -> fun((ELN) -> fun((ELO) -> fun((ELP) -> fun((ELQ) -> fun((ELR) -> fun((ELS) -> ELT)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((ELV, ELW) -> ELX)) -> fun((ELW, ELV) -> ELX).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(ELY) -> ELY.
identity(X) ->
    X.

-spec constant(ELZ) -> fun((any()) -> ELZ).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EMB, fun((EMB) -> any())) -> EMB.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EMD) -> EME), EMD) -> EME.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EMF, EMG) -> EMH), EMF, EMG) -> EMH.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EMI, EMJ, EMK) -> EML), EMI, EMJ, EMK) -> EML.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
