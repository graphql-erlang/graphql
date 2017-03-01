-module(graphql_type_non_null).

-export([
  type/1
]).

type(InnerType)-> #{
  kind => 'NON_NULL',
  name => null,
  ofType => InnerType,

  serialize => fun serialize/1,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value) -> Value.

parse_value(null, _) -> throw({error, non_null, <<"Null value provided to non null type">>});
parse_value(Value, _) -> Value.

parse_literal(Literal, #{ofType := InnerType})->
  ParseLiteral = maps:get(parse_literal, graphql_type:unwrap_type(InnerType)),
  case ParseLiteral(Literal, graphql_type:unwrap_type(InnerType)) of
    null -> throw({error, non_null, <<"Null provided to non null type">>});
    Result -> Result
  end.
