-include_lib("eunit/include/eunit.hrl").

-module(graphql_test).
-author("mrchex").


recursion_nesting_test()->
  Document = <<"{
    nest {
      info
      nest {
        info
        nest {
          nest {
            info
          }
        }
      }
    }
  }">>,

  ?assertEqual( #{
    data => #{
      <<"nest">> => #{
        <<"info">> => <<"information does not availiable">>,
        <<"nest">> => #{
          <<"info">> => <<"information does not availiable">>,
          <<"nest">> => #{
            <<"nest">> => #{
              <<"info">> => <<"information does not availiable">>
            }
          }
        }
      }
    }, errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{}) ).


arguments_valid_passing_test() ->
  Document = <<"{
    arg(hello: \"world\") {
      greatings_for
    }
  }">>,

  ?assertEqual( #{
    data => #{
      <<"arg">> => #{
        <<"greatings_for">> => <<"world">>
      }
    },
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{}) ).


default_argument_passing_test() ->
  Document = <<"{ arg { greatings_for } }">>,
  ?assertEqual(#{
    data => #{
      <<"arg">> => #{
        <<"greatings_for">> => <<"default value">>
      }
    },
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

% is correnct this test? If in schema arguments defined - need it pass to the resolver or not?
no_arguments_passing_test() ->
  Document = <<"{ arg_without_defaults { arguments_count } }">>,
  ?assertEqual(#{
    data => #{
      <<"arg_without_defaults">> => #{
        <<"arguments_count">> => 0
      }
    },
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).


%%default_resolver_must_pass_own_arguments_to_child_test() ->
%%  Document = <<"{
%%    arg:arg_without_resolver(argument: \"ok\") {
%%      argument
%%    }
%%  }">>,
%%
%%  ?assertEqual(#{
%%    data => #{
%%      <<"arg">> => #{
%%        <<"argument">> => <<"ok">>
%%      }
%%    },
%%    errors => []
%%  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).



%%receiver(I)->
%%  case I of
%%    0 -> ok;
%%    _ -> receive
%%      {ololo, ok, _} ->
%%        receiver(I-1)
%%    end
%%  end.
%%
%%ololo() ->
%%
%%  %%  Sync results:
%%  %%    Time start: {12,28,41}
%%  %%    Time end: {12,34,10}
%%  %%    Operations performed: 1000000
%%
%%  %% Async results:
%%  %%    Time start: {12,40,37}
%%  %%    Time end: {12,40,39}
%%  %%    Operations performed: 10000
%%
%%  %% with io:format
%%  %%    Time start: {12,41,15}
%%  %%    Time end: {12,44,20}
%%  %%    Operations performed: 1000000
%%
%%  %% without io:format
%%  %%    Time start: {12,41,15}
%%  %%    Time end: {12,44,20}
%%  %%    Operations performed: 1000000
%%
%%  TimeStart = time(),
%%  CountIterations = 1000000,
%%  Self = self(),
%%
%%  lists:foreach(fun(I) ->
%%    io:format("["),
%%    spawn(fun() ->
%%      ok = ololo(CountIterations, Self)
%%    end),
%%
%%    receiver(CountIterations),
%%    io:format("~p]", [I])
%%  end, lists:seq(0, 1000)),
%%
%%  TimeEnd = time(),
%%  io:format("~n~nTime start: ~p~nTime end: ~p~nOperations performed: ~p~n", [TimeStart, TimeEnd, CountIterations]).
%%
%%ololo(0, _)-> ok;
%%ololo(I, Pid)->
%%  spawn(fun() ->
%%
%%    Document = <<"{
%%      nest {
%%        info
%%        nest {
%%          info
%%          nest {
%%            nest {
%%              info
%%            }
%%          }
%%        }
%%      }
%%    }">>,
%%    graphql:execute(graphql_test_schema:schema_root(), Document, #{}),
%%
%%    Pid ! {ololo, ok, I}
%%  end),
%%  ololo(I-1, Pid).
