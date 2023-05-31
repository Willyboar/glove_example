-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((EBL) -> EBM), fun((EBM) -> EBN)) -> fun((EBL) -> EBN).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((EBO, EBP) -> EBQ)) -> fun((EBO) -> fun((EBP) -> EBQ)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EBS, EBT, EBU) -> EBV)) -> fun((EBS) -> fun((EBT) -> fun((EBU) -> EBV))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EBX, EBY, EBZ, ECA) -> ECB)) -> fun((EBX) -> fun((EBY) -> fun((EBZ) -> fun((ECA) -> ECB)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((ECD, ECE, ECF, ECG, ECH) -> ECI)) -> fun((ECD) -> fun((ECE) -> fun((ECF) -> fun((ECG) -> fun((ECH) -> ECI))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((ECK, ECL, ECM, ECN, ECO, ECP) -> ECQ)) -> fun((ECK) -> fun((ECL) -> fun((ECM) -> fun((ECN) -> fun((ECO) -> fun((ECP) -> ECQ)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((ECS, ECT) -> ECU)) -> fun((ECT, ECS) -> ECU).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(ECV) -> ECV.
identity(X) ->
    X.

-spec constant(ECW) -> fun((any()) -> ECW).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(ECY, fun((ECY) -> any())) -> ECY.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EDA) -> EDB), EDA) -> EDB.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EDC, EDD) -> EDE), EDC, EDD) -> EDE.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EDF, EDG, EDH) -> EDI), EDF, EDG, EDH) -> EDI.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
