-module(graphql_test_schema).
-author("mrchex").

%% API
-export([schema_root/0]).

print(Text, Args) -> io:format(Text ++ "~n", Args).

schema_root()->
  graphql:schema(#{
    query => fun query/0
  }).

query() ->
  graphql:objectType(<<"QueryRoot">>, <<"This is Root Query Type">>, #{
    <<"hello">> => #{
      type => string,
      description => <<"This is hello world field">>
    },
    <<"arg">> => #{
      type => {object, fun arg/0},
      args => #{
        <<"hello">> => #{ type => string, default => <<"default value">> },
        <<"argument">> => #{ type => string, default => <<"Default argument value">>}
      },
      description => <<"Argument schema">>,
      resolver => fun(_, Args) -> Args end
    },
    <<"arg_bool">> => #{
      type => boolean,
      args => #{
        <<"bool">> => #{ type => boolean, description => <<"Proxied argument to result">> }
      },
      resolver => fun(_, #{<<"bool">> := Value}) -> Value end
    },
    <<"arg_without_resolver">> => #{
      type => {object, fun arg/0},
      args => #{
        <<"argument">> => #{ type => string, default => <<"Default argument value">>}
      },
      description => <<"Argument schema">>
    },
    <<"arg_without_defaults">> => #{
      type => {object, fun arg/0},
      description => <<"Pass arguments count down to three">>,
      args => #{
        <<"argument">> => #{ type => string }
      },
      resolver => fun(_, Args) ->
        print("RESOLVER FOR arg_without_defaults", []),
        #{<<"arguments_count">> => length(maps:keys(Args))}
      end
    },
    <<"nest">> => #{
      type => {object, fun nest/0},
      description => <<"go deeper inside">>
    }
  }).

nest()->
  graphql:objectType(<<"Nest">>, <<"Test schema for nesting">>, #{
    <<"info">> => #{
      type => string,
      description => <<"Information">>,
      resolver => fun(_,_) -> <<"information does not availiable">> end
    },
    <<"nest">> => #{
      type => {object, fun nest/0},
      description => <<"go deeper inside">>
    }
  }).

arg()->
  graphql:objectType(<<"Arg">>, <<"when you pass argument - that return in specified field">>, #{
    <<"greatings_for">> => #{
      type => srtring,
      description => <<"Proxy hello argument to response">>,
      resolver => fun(Obj, _) -> maps:get(<<"hello">>, Obj, undefined) end
    },
    <<"argument">> => #{
      type => string,
      description => <<"This is argument passed to the parrent. It must be authomaticly resolved">>
    },
    <<"arguments_count">> => #{
      type => integer,
      description => <<"Passed from parrent - count of arguments">>
    }
  }).