-module(graphql_type_list).

-export([
  type/1
]).

type(InnerType)-> #{
  kind => 'LIST',
  name => null,
  ofType => InnerType,

  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

parse_value(null, _) -> null;
parse_value(
  #{kind := Kind, values := Values},
  #{ofType := InnerType}
) when Kind =:= 'ListValue' orelse Kind =:= <<"ListValue">> ->
  ParseValue = maps:get(parse_value, graphql_type:unwrap_type(InnerType)),
  lists:map(fun(Value) ->
    ParseValue(Value, InnerType)
  end, Values).

parse_literal(null, _) -> null;
parse_literal(#{kind := 'ListValue', values := Values}, #{ofType := InnerType}) ->
  InnerTypeUnwrapped = graphql_type:unwrap_type(InnerType),
  ParseLiteral = maps:get(parse_literal, InnerTypeUnwrapped),
  lists:map(fun(Value) ->
    ParseLiteral(Value, InnerTypeUnwrapped)
  end, Values).
