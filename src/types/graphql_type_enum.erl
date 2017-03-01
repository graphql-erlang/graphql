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
    parse_literal => fun parse_literal/2
  }.

parse_literal(#{kind := 'EnumValue', value := EnumValue}, #{enumValues := Values})->
  find_enum(EnumValue, Values).

find_enum(EnumValue, [])->
  throw({error, enum, <<"Cannot find enum: ", EnumValue/binary>>});
find_enum(EnumValue, [#{name := EnumValue, value := Value}|_]) -> Value;
find_enum(EnumValue, [_|Tail]) -> find_enum(EnumValue, Tail).


