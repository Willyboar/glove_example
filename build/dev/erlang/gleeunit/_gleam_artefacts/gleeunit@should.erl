-module(gleeunit@should).
-compile([no_auto_import, nowarn_unused_vars]).

-export([equal/2, be_true/1, be_false/1, fail/0, not_equal/2, be_ok/1, be_error/1]).

-spec equal(EWB, EWB) -> nil.
equal(Field@0, Field@1) ->
    gleeunit_ffi:should_equal(Field@0, Field@1).

-spec be_true(boolean()) -> nil.
be_true(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, true).

-spec be_false(boolean()) -> nil.
be_false(Actual) ->
    _pipe = Actual,
    gleeunit_ffi:should_equal(_pipe, false).

-spec fail() -> nil.
fail() ->
    be_true(false).

-spec not_equal(EWC, EWC) -> nil.
not_equal(Field@0, Field@1) ->
    gleeunit_ffi:should_not_equal(Field@0, Field@1).

-spec be_ok({ok, EWD} | {error, any()}) -> EWD.
be_ok(Field@0) ->
    gleeunit_ffi:should_be_ok(Field@0).

-spec be_error({ok, any()} | {error, EWH}) -> EWH.
be_error(Field@0) ->
    gleeunit_ffi:should_be_error(Field@0).
