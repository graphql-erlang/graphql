-module(graphql_test_schema).
-include("types.hrl").

%% API
-export([schema_root/0]).

print(Text, Args) -> io:format(Text ++ "~n", Args).

schema_root()-> ?SCHEMA(#{
  query => fun query/0
}).

enumOneTwo() -> ?ENUM("EnumOneTwo", "Test description", [
  ?ENUM_VAL(1, <<"ONE">>, "This is 1 represent as text"),
  ?ENUM_VAL(2, <<"TWO">>, "This is 2 represent as text")
]).

query() -> ?OBJECT("QueryRoot", "This is Root Query Type", #{
    "hello" => ?FIELD(?STRING, "This is hello world field"),
    "range" => ?FIELD(?LIST(?INT), "Sequence range", #{
        <<"seq">> => #{type => ?INT}
      },
      fun(_, #{<<"seq">> := Seq}) -> lists:seq(0, Seq) end
    ),
    <<"range_objects">> => #{
      type => ?LIST(fun valueObject/0),
      args => #{
        <<"seq">> => #{type => ?INT}
      },
      resolver => fun(_, #{<<"seq">> := Seq}) -> lists:seq(0, Seq) end
    },
    <<"arg">> => ?FIELD(fun arg/0, "Argument schema", #{
        "hello" => ?ARG(?STRING, <<"default value">>, "Hello desction"),
        "argument" => #{ type => ?STRING, default => <<"Default argument value">>},
        % scalars
        "bool" => ?ARG(?BOOLEAN),
        "enum" => ?ARG(fun enumOneTwo/0),
        "float" => #{ type => ?FLOAT },
        "int" => #{ type => ?INT },
        "list" => #{ type => ?LIST(?INT) },
        "str" => #{ type => ?STRING }
      },
      fun(_, Args) -> Args end
    ),
    <<"arg_non_null">> => #{
      type => fun arg/0,
      args => #{
        <<"int">> => #{type => ?NON_NULL(?INT) }
      },
      resolver => fun(_, Args) -> Args end
    },
    <<"arg_non_null_list">> => #{
      type => fun arg/0,
      args => #{
        <<"list">> => #{type => ?NON_NULL(?LIST(?INT)) }
      },
      resolver => fun(_, Args) -> Args end
    },
    <<"arg_bool">> => #{
      type => ?BOOLEAN,
      args => #{
        <<"bool">> => #{ type => ?BOOLEAN, description => <<"Proxied argument to result">> }
      },
      resolver => fun(_, #{<<"bool">> := Value}) -> Value end
    },
    <<"arg_without_resolver">> => #{
      type => fun arg/0,
      args => #{
        <<"argument">> => #{ type => ?STRING, default => <<"Default argument value">>}
      },
      description => <<"Argument schema">>
    },
    <<"arg_without_defaults">> => #{
      type => fun arg/0,
      description => <<"Pass arguments count down to three">>,
      args => #{
        <<"argument">> => #{ type => ?STRING }
      },
      resolver => fun(_, Args) ->
        print("RESOLVER FOR arg_without_defaults", []),
        #{<<"arguments_count">> => length(maps:keys(Args))}
      end
    },
    <<"nest">> => #{
      type => fun nest/0,
      description => <<"go deeper inside">>,
      resolver => fun() -> #{} end
    },
    <<"non_null">> => #{
      type => ?NON_NULL(?INT),
      args => #{
        <<"int">> => #{type => ?NON_NULL(?INT)}
      },
      resolver => fun(_, #{<<"int">> := Int}) -> Int end
    },
    <<"non_null_invalid">> => #{
      type => ?NON_NULL(?INT),
      resolver => fun() -> null end
    },
    <<"enum">> => #{
      type => ?INT,
      args => #{
        <<"e">> => #{
          type => ?ENUM(<<"Test">>, <<"Test description">>, [
            ?ENUM_VAL(1, <<"ONE">>, <<"This is 1 represent as text">>),
            ?ENUM_VAL(2, <<"TWO">>, <<"This is 2 represent as text">>)
          ])
        }
      },
      resolver => fun(_, #{<<"e">> := E}) -> E end
    },
    <<"enum_value">> => #{
      type => fun enumOneTwo/0,
      args => #{
        <<"e">> => #{
          type => fun enumOneTwo/0
        }
      },
      resolver => fun(_, #{<<"e">> := E}) -> E end
    },
    <<"enum_non_null">> => #{
      type => ?INT,
      args => #{
        <<"e">> => #{
          type => ?NON_NULL(?ENUM(<<"Test">>, <<"Test description">>, [
            ?ENUM_VAL(1, <<"ONE">>, <<"This is 1 represent as text">>),
            ?ENUM_VAL(1, <<"TWO">>, <<"This is 2 represent as text">>)
          ]))
        }
      },
      resolver => fun(_, #{<<"e">> := E}) -> E end
    },
    <<"union">> => #{
      type => ?UNION(<<"TestUnionType">>, <<"Many types in one type :)">>, [
        fun nest/0,
        fun hello/0
      ], fun
        ({nest, V}, _)-> {fun nest/0, V};
        ({hello, V}, _) -> {fun hello/0, V}
      end),
      args => #{
        <<"type">> => #{
          type => ?ENUM(<<"EnumUnionTest">>, <<>>, [
            ?ENUM_VAL(nest, <<"NEST">>, <<>>),
            ?ENUM_VAL(hello, <<"HELLO">>, <<>>)
          ])
        }
      },
      resolver => fun
        (_, #{<<"type">> := nest}) -> {nest, #{  }};
        (_, #{<<"type">> := hello}) -> {hello, #{ <<"name">> => <<"Union">>}}
      end
    },
    <<"union_default_resolve_type">> => #{
      type => ?UNION(<<"TestUnionTypeDefaultRosolve">>, <<"Many types in one type :)">>, [
        fun nest/0,
        fun hello/0
      ]),
      args => #{
        <<"type">> => #{
          type => ?ENUM(<<"EnumUnionTest">>, <<>>, [
            ?ENUM_VAL(nest, <<"NEST">>, <<>>),
            ?ENUM_VAL(hello, <<"HELLO">>, <<>>)
          ])
        }
      },
      resolver => fun
        (_, #{<<"type">> := nest}) -> {fun nest/0, #{  }};
        (_, #{<<"type">> := hello}) -> {fun hello/0, #{ <<"name">> => <<"Union">>}}
      end
    },

    "newNotation" => ?FIELD(fun newNotation/0, null, fun newNotation_resolver/0)
  }).

hello()-> graphql:objectType(<<"Hello">>, <<>>, #{
  <<"name">> => #{
    type => ?STRING
  }
}).

nest()->
  graphql:objectType(<<"Nest">>, <<"Test schema for nesting">>, #{
    <<"info">> => #{
      type => ?STRING,
      description => <<"Information">>,
      resolver => fun(_,_) -> <<"information does not availiable">> end
    },
    <<"nest">> => #{
      type => fun nest/0,
      description => <<"go deeper inside">>,
      resolver => fun() -> #{} end
    }
  }).

arg()-> ?OBJECT("Arg", "when you pass argument - that return in specified field", #{
    "greatings_for" => ?FIELD(?STRING, "Proxy hello argument to response",
      fun(Obj, _) -> maps:get(<<"hello">>, Obj, undefined) end
    ),
    <<"argument">> => #{
      type => ?STRING,
      description => <<"This is argument passed to the parrent. It must be authomaticly resolved">>
    },

    <<"arguments_count">> => #{
      type => ?INT,
      description => <<"Passed from parrent - count of arguments">>
    },

    % scalars
    <<"bool">> => #{ type => ?BOOLEAN },
    <<"enum">> => #{ type => fun enumOneTwo/0 },
    <<"float">> => #{ type => ?FLOAT },
    <<"int">> => #{ type => ?INT },
    <<"list">> => #{ type => ?LIST(?INT) },
    <<"str">> => #{ type => ?STRING }
  }).

valueObject() -> graphql:objectType(<<"ValueObject">>, <<"">>, #{
  <<"value">> => #{
    type => ?LIST(?INT),
    description => <<"range of object value">>,
    resolver => fun(Value) -> lists:seq(0, Value) end
  }
}).

newNotation() -> ?OBJECT("NewNotation", "Test macros for new notation style", #{
  "string" => ?FIELD(?STRING, "Field description"),
  "deprecated" => ?FIELD(?STRING, "Test deprecation"),
  "enum" => ?ENUM("EnumNewNotation", "String description", [
    ?ENUM_VAL(1, "One", "String one description")
  ])
}).

newNotation_resolver() -> #{
  <<"field">> => <<"field">>,
  <<"deprecated">> => <<"okay">>,
  <<"enum">> => 1
}.