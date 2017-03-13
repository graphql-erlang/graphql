-module(graphql_type_non_null).

-export([
  type/1
]).

type(InnerType)-> #{
  kind => 'NON_NULL',
  name => null,
  ofType => InnerType,

  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.



parse_value(null, _) -> throw({error, non_null, <<"Null value provided to non null type">>});
parse_value(#{kind := 'NonNullValue', value := Value}, #{ofType := InnerType}) ->
  Type = graphql_type:unwrap_type(InnerType),
  ParseValue = maps:get(parse_value, Type),
  case ParseValue(Value, Type) of
    null -> throw({error, non_null, <<"Null provided to non null type">>});
    Result -> Result
  end.

parse_literal(null, _) -> throw({error, non_null, <<"Null value provided to non null type">>});
parse_literal(Literal, #{ofType := InnerType})->
  Type = graphql_type:unwrap_type(InnerType),
  ParseLiteral = maps:get(parse_literal, Type),
  case ParseLiteral(Literal, Type) of
    null -> throw({error, non_null, <<"Null provided to non null type">>});
    Result -> Result
  end.
