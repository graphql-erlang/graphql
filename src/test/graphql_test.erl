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
    % TODO: fix sorting?
    data => [{<<"nest">>,
      [{<<"info">>,<<"information does not availiable">>},
        {<<"nest">>,
          [{<<"info">>,<<"information does not availiable">>},
            {<<"nest">>,
              [{<<"nest">>,
                [{<<"info">>,
                  <<"information does not availiable">>}]}]}]}]}], errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{}) ).

arguments_valid_passing_test() ->
  Document = <<"{
    arg(hello: \"world\") {
      greatings_for
    }
  }">>,

  ?assertEqual( #{
    data => [
      {<<"arg">>, [
        {<<"greatings_for">>, <<"world">>}
      ]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{}) ).


default_argument_passing_test() ->
  Document = <<"{ arg { greatings_for } }">>,
  ?assertEqual(#{
    data => [
      {<<"arg">>, [
        {<<"greatings_for">>, <<"default value">>}
      ]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

% is correnct this test? If in schema arguments defined - need it pass to the resolver or not?
no_arguments_passing_test() ->
  Document = <<"{ arg_without_defaults { arguments_count } }">>,
  ?assertEqual(#{
    data => [
      {<<"arg_without_defaults">>, [
        {<<"arguments_count">>, 1}
      ]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

map_support_default_resolver_test() ->
  Document = <<"{ hello }">>,
  ?assertEqual(#{
    data => [
      {<<"hello">>, <<"world">>}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{<<"hello">> => <<"world">>})).

proplists_support_default_resolver_test() ->
  Document = <<"{ hello }">>,
  ?assertEqual(#{
    data => [
      {<<"hello">>, <<"proplists">>}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, [{<<"hello">>, <<"proplists">>}])).

fragment_test()->
  Document = "{ nest { ...NestFragmentTest } } fragment NestFragmentTest on Nest { info }",
  ?assertEqual(#{
    data => [
      {<<"nest">>, [{<<"info">>,<<"information does not availiable">>}]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

subselection_not_provided_error_test() ->
  Document = <<"{ range_objects(seq: 2) }">>,
  ?assertEqual(#{
    error => <<"No sub selection provided for `ValueObject`">>,
    type => complete_value
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).


%%%%%
% Types

boolean_type_test() ->
  Document = <<"{ arg_bool(bool: true) }">>,
  ?assertEqual(#{
    data => [
      {<<"arg_bool">>, true}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

boolean_type_validation_test() ->
  Document = <<"{ arg_bool(bool: \"invalid boolean type\") }">>,
  ?assertEqual(#{
    error => <<"Unexpected type StringValue, expected BooleanValue">>,
    type => type_validation
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).


integer_type_test() ->
  Document = <<"{ arg(int: 10) { int } }">>,
  ?assertEqual(#{
    data => [
      {<<"arg">>, [
        {<<"int">>, 10}
      ]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).


list_type_test() ->
  Document = <<"{ range(seq: 10) }">>,
  ?assertEqual(#{
    data => [
      {<<"range">>, [0,1,2,3,4,5,6,7,8,9,10]}
    ],
    errors => []
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

list_of_object_with_list_of_int_test() ->
  Document = <<"{ range_objects(seq: 2) { value } }">>,
  ?assertEqual(#{data => [
    {<<"range_objects">>, [
      [{<<"value">>,[0]}],
      [{<<"value">>,[0,1]}],
      [{<<"value">>,[0,1,2]}]
    ]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

non_null_valid_test()->
  Document = <<"{ non_null(int: 10) }">>,
  ?assertEqual(#{data => [
    {<<"non_null">>, 10}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

non_null_invalid_arguments_test()->
  Document = <<"{ non_null }">>,
  ?assertEqual(#{
    error => <<"Null value provided to non null type">>,
    type => non_null
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

non_null_invalid_result_test()->
  Document = <<"{ non_null_invalid }">>,
  ?assertEqual(#{
    error => <<"Non null type cannot be null">>,
    type => complete_value
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_test() ->
  Document = <<"{ enum(e: ONE) }">>,
  ?assertEqual(#{data => [
    {<<"enum">>, 1}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

non_null_enum_test() ->
  Document = <<"{ enum_non_null(e: ONE) }">>,
  ?assertEqual(#{data => [
    {<<"enum_non_null">>, 1}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

non_null_enum_invalid_test() ->
  Document = <<"{ enum_non_null }">>,
  ?assertEqual(#{
    error => <<"Null value provided to non null type">>,
    type => non_null
  }, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).


enum_error_test() ->
  Document = <<"{ enum(e: MANY) }">>,
  ?assertEqual(#{
    error => <<"Cannot find enum: MANY">>,
    type => enum
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
