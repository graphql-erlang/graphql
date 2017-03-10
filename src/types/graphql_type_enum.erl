-module(graphql_type_enum).

%% API
-export([
  type/3
]).

-spec type(binary(), binary(), list(map())) -> map().
type(Name, Description, EnumValues)->

  #{
    kind => 'ENUM',
    name => Name,
    description => Description,
    enumValues => EnumValues,
    serialize => fun serialize/3,
    parse_value => fun parse_value/2,
    parse_literal => fun parse_literal/2
  }.

serialize(null, _, _) -> null;
serialize(EnumValue, #{enumValues := Values}, _) ->
  find_enum_by_val(EnumValue, Values).

parse_value(null, _) -> null.
%%  io:format("PARSE VALUE?: ~p~n",[Value]).

parse_literal(null, _) -> null;
parse_literal(#{kind := 'EnumValue', value := EnumValue}, #{enumValues := Values})->
  find_enum(EnumValue, Values).

find_enum(EnumName, [])->
  throw({error, enum, <<"Cannot find enum: ", EnumName/binary>>});
find_enum(EnumName, [#{name := EnumName, value := Value}|_]) -> Value;
find_enum(EnumName, [_|Tail]) -> find_enum(EnumName, Tail).

find_enum_by_val(Value, [])->
  throw({error, enum, <<"Cannot find enum name by value: ", Value/binary>>});
find_enum_by_val(Value, [#{name := EnumName, value := Value}|_]) -> EnumName;
find_enum_by_val(Value, [_|Tail]) -> find_enum_by_val(Value, Tail).

