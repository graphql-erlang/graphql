-module(graphql_schema).
-author("mrchex").

%% API
-export([
  get_name/1,

  get_field/2,

  get_type_from_definition/1,
  get_field_resolver/2,

  get_argument_definitions/2,

  check_type/2,

  get_enum/2

]).

get_name(ObjectType)->
  io:format("Object: ~p", [ObjectType]),
  maps:get(name, ObjectType).

get_field(FieldName, ObjectType)->
  Fields = maps:get(fields, ObjectType),
  maps:get(FieldName, Fields, undefined).

get_type_from_definition(Definition) ->
  graphql_type:unwrap_type(maps:get(type, Definition)).

get_field_resolver(FieldName, ObjectType)->
  Field = get_field(FieldName, ObjectType),
  maps:get(resolver, Field, fun(ObjectValue, ArgumentValues) ->
    default_resolver(ObjectType, FieldName, ObjectValue, ArgumentValues)
  end).

% if this field is scalar type -
default_resolver(_ObjectType, FieldName, ObjectValue, _ArgumentValues)->
  case is_map(ObjectValue) of
    true -> maps:get(FieldName, ObjectValue, null);
    false -> proplists:get_value(FieldName, ObjectValue, null)
  end.



get_argument_definitions(FieldName, ObjectType)->
  FieldDefinition = get_field(FieldName, ObjectType),
  maps:get(args, FieldDefinition, #{}).

check_type(string, Value)-> is_binary(Value);
check_type(integer, Value)-> is_integer(Value);
check_type(boolean, Value)-> is_boolean(Value).

get_enum(Schema, Enum) ->
  io:format("Schema: ~p~n", [Schema]),
  case maps:get(enums, Schema, undefined) of
    undefined -> undefined;
    Enums -> maps:get(Enum, Enums, undefined)
  end.
