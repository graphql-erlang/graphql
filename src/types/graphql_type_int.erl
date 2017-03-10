-module(graphql_type_int).

-export([
  type/0
]).

type()-> #{
  kind => 'SCALAR',
  name => 'Int',
  ofType => null,
  description => <<
    "The `Int` scalar type represents non-fractional signed whole numeric ",
    "values. Int can represent values between -(2^53 - 1) and 2^53 - 1 since ",
    "represented in JSON as double-precision floating point numbers specified ",
    "by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
  >>,

  serialize => fun serialize/3,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value,_,_) -> coerce(Value).

parse_value(Value, _) -> coerce(Value).

parse_literal(null, _) -> null;
parse_literal(#{kind := Kind, value := Value}, _) when
  Kind =:= 'IntValue' ->
    coerce(Value);
parse_literal(#{kind := Kind}, _) ->
  throw({error, type_validation, <<"Unexpected value type. Got: ", (atom_to_binary(Kind, utf8))/binary, ", expected: IntValue">>}).


%% TODO: add binary, string and float representation
-spec coerce(null | float() | integer()) -> integer().
coerce(null) -> null;
coerce(Value) when is_float(Value)->
  IntValue = round(Value),
  case Value > IntValue of
    true -> throw({error, type_validation, <<"Cannot coerce float into Int type.">>});
    false -> IntValue
  end;
coerce(Value) when is_integer(Value) -> Value;
coerce(_) -> throw({error, type_validation, <<"Cannot coerce Int type.">>}).
