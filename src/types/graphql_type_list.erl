-module(graphql_type_list).

-export([
  type/1
]).

type(InnerType)-> #{
  kind => 'LIST',
  name => null,
  ofType => InnerType,

  serialize => fun serialize/1,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value) -> Value.

parse_value(null, _) -> null;
parse_value(Value, _) when is_list(Value) -> Value.

parse_literal(#{kind := 'ListValue', values := Values}, _) ->
  Values.
