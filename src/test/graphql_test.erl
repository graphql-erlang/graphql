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

fragment_inline_test()->
  Document = "{ nest { ... on Nest { info } } }",
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

list_in_args_test() ->
  Document = <<"{ arg(list: [1,2,3]) { list } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [
      {<<"list">>, [1,2,3]}
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

%%% Enum

enum_arg_test() ->
  Document = <<"{ enum(e: ONE) }">>,
  ?assertEqual(#{data => [
    {<<"enum">>, 1}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_arg_null_test() ->
  Document = <<"{ enum }">>,
  ?assertEqual(#{data => [
    {<<"enum">>, null}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_field_test() ->
  Document = <<"{ enum_value(e: ONE) }">>,
  ?assertEqual(#{data => [
    {<<"enum_value">>, <<"ONE">>}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_field_null_test() ->
  Document = <<"{ enum_value }">>,
  ?assertEqual(#{data => [
    {<<"enum_value">>, null}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_non_null_test() ->
  Document = <<"{ enum_non_null(e: ONE) }">>,
  ?assertEqual(#{data => [
    {<<"enum_non_null">>, 1}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

enum_non_null_invalid_test() ->
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


%%% Variables

variable_bool_test() ->
  Document = <<"query($var: Boolean) { arg(bool: $var) { bool } }">>,
  VariableValues = #{
    <<"var">> => true
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"bool">>, true}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_default_bool_test() ->
  Document = <<"query($var: Boolean = true) { arg(bool: $var) { bool } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"bool">>, true}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

variable_enum_one_test() ->
  Document = <<"query($var: EnumOneTwo) { arg(enum: $var) { enum } }">>,
  VariableValues = #{
    <<"var">> => <<"ONE">>
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"enum">>, <<"ONE">>}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_enum_two_test() ->
  Document = <<"query($var: EnumOneTwo) { arg(enum: $var) { enum } }">>,
  VariableValues = #{
    <<"var">> => <<"TWO">>
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"enum">>, <<"TWO">>}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_default_enum_test() ->
  Document = <<"query($var: EnumOneTwo = ONE) { arg(enum: $var) { enum } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"enum">>, <<"ONE">>}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

variable_float_test() ->
  Document = <<"query($var: Float) { arg(float: $var) { float } }">>,
  VariableValues = #{
    <<"var">> => 100.500
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"float">>, 100.5}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_default_float_test() ->
  Document = <<"query($var: FLOAT = 1.5) { arg(float: $var) { float } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"float">>, 1.5}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

variable_int_test() ->
  Document = <<"query($var: Int) { arg(int: $var) { int } }">>,
  VariableValues = #{
    <<"var">> => 100500
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"int">>, 100500}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_default_int_test() ->
  Document = <<"query($var: Int = 100500) { arg(int: $var) { int } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"int">>, 100500}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

variable_list_test() ->
  Document = <<"query($var: [Int]) { arg(list: $var) { list } }">>,
  VariableValues = #{
    <<"var">> => [1,0,0,5,0,0]
  },
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"list">>, [1,0,0,5,0,0]}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_default_list_test() ->
  Document = <<"query($var: [Int] = [1,0,0,5,0,0]) { arg(list: $var) { list } }">>,
  ?assertEqual(#{data => [
    {<<"arg">>, [{<<"list">>, [1,0,0,5,0,0]}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, #{})).

variable_non_null_test() ->
  Document = <<"query($var: Int!) { arg_non_null(int: $var) { int } }">>,
  VariableValues = #{
    <<"var">> => 100500
  },
  ?assertEqual(#{data => [
    {<<"arg_non_null">>, [{<<"int">>, 100500}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).

variable_list_non_null_test() ->
  Document = <<"query($var: [Int]!) { arg_non_null_list(list: $var) { list } }">>,
  VariableValues = #{
    <<"var">> => [1,2,3]
  },
  ?assertEqual(#{data => [
    {<<"arg_non_null_list">>, [{<<"list">>, [1,2,3]}]}
  ], errors => []}, graphql:execute(graphql_test_schema:schema_root(), Document, VariableValues, #{}, #{})).
