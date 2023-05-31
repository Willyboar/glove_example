-module(glove_example).
-compile([no_auto_import, nowarn_unused_vars]).

-export([main/0]).

-spec main() -> nil.
main() ->
    Data_def = {data_def,
        {linkage, false, none, none},
        <<"str"/utf8>>,
        none,
        [{byte, {str, <<"hello world"/utf8>>}}, {byte, {constant, 0}}]},
    Main_func = {function,
        glove:public(),
        <<"main"/utf8>>,
        [],
        {some, word},
        [{block,
                <<"@start"/utf8>>,
                [{assign,
                        {temporary, <<"r"/utf8>>},
                        word,
                        {call,
                            {global, <<"puts"/utf8>>},
                            [{long, {global, <<"str"/utf8>>}}]}},
                    {volatile, {ret, {some, {const, 0}}}}]}]},
    Empty_module = {module, [], [], []},
    _pipe = Empty_module,
    _pipe@1 = glove:add_data(_pipe, Data_def),
    _pipe@2 = glove:add_function(_pipe@1, Main_func),
    _pipe@3 = glove:display_module(_pipe@2),
    gleam@io:print(_pipe@3).
