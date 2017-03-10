-module(graphql_type_float).

-export([
  type/0
]).

type()-> #{
  kind => 'SCALAR',
  name => 'Float',
  ofType => null,
  description => <<
    "The `Float` scalar type represents signed double-precision fractional ",
    "values as specified by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
  >>,

  serialize => fun serialize/3,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value,_,_) -> coerce(Value).


parse_value(null, _) -> coerce(null);
parse_value(#{kind := <<"FloatValue">>, value := Value}, _) -> coerce(Value);
parse_value(#{kind := 'FloatValue', value := Value}, _) -> coerce(Value).

-spec parse_literal(map(), map()) -> float().
parse_literal(null, _) -> null;
parse_literal(#{kind := Kind, value := Value}, _) when
  Kind =:= 'IntValue' orelse
  Kind =:= 'FloatValue' ->
    coerce(Value);
parse_literal(#{kind := Kind}, _) ->
  throw({error, type_validation, <<"Unexpected value type. Got: ", (atom_to_binary(Kind, utf8))/binary, ", expected: FloatValue">>}).


%% TODO: add binary, string and float representation
-spec coerce(null | float() | integer()) -> float().
coerce(null) -> null;
coerce(Value) when is_integer(Value) -> Value * 1.0;
coerce(Value) when is_float(Value) -> Value;
coerce(_) -> throw({error, type_validation, <<"Cannot coerce Float type.">>}).
