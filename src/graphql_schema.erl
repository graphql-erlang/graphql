-module(graphql_schema).
-author("mrchex").

%% API
-export([
  get_name/1,

  get_field/2,

  get_field_type/1,
  get_field_resolver/2,

  get_argument_definitions/2,
  get_argument_type/1,
  get_argument_default/1,

  check_type/2

]).

get_name(ObjectType)->
  io:format("Object: ~p", [ObjectType]),
  maps:get(name, ObjectType).

get_field(FieldName, ObjectType)->
  Fields = maps:get(fields, ObjectType),
  maps:get(FieldName, Fields, undefined).

is_field_not_null({not_null, _}) -> true;
is_field_not_null(_) -> false.

is_field_type_scalar({object, _}) -> false;
is_field_type_scalar(_) -> true.

get_field_type(Field)->
  maps:get(type, Field).

get_field_resolver(FieldName, ObjectType)->
  Field = get_field(FieldName, ObjectType),
  maps:get(resolver, Field, fun(ObjectValue, ArgumentValues) ->
    default_resolver(ObjectType, FieldName, ObjectValue, ArgumentValues)
  end).

% if this field is scalar type -
default_resolver(ObjectType, FieldName, ObjectValue, ArgumentValues)->
  Field = get_field(FieldName, ObjectType),
  FieldType = get_field_type(Field),

  case {is_field_type_scalar(FieldType), is_field_not_null(FieldType)} of
    % when field is scalar and allow null
    {true, false} ->
      case is_map(ObjectValue) of
        true -> maps:get(FieldName, ObjectValue, null);
        false -> proplists:get_value(FieldName, ObjectValue, null)
      end;

    % when fied is scalar non null field
    {true, true} -> case maps:get(FieldName, ObjectValue, undefined) of
        undefined -> throw({error, validation, <<"Unexpected default resolve value for non-null field">>});
        NotNullValue -> NotNullValue
      end;

    % when field is ObjectType - return own arguments as default object
    {false, _} -> ArgumentValues
  end.


get_argument_definitions(FieldName, ObjectType)->
  FieldDefinition = get_field(FieldName, ObjectType),
  maps:get(args, FieldDefinition, #{}).

get_argument_type(ArgumentDefinition) ->
  maps:get(type, ArgumentDefinition).

get_argument_default(ArgumentDefinition) ->
  maps:get(default, ArgumentDefinition, undefined).

check_type(string, Value)-> is_binary(Value);
check_type(integer, Value)-> is_integer(Value).