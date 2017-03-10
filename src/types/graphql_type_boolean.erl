-module(graphql_type_boolean).

-export([
  type/0
]).

type()-> #{
  kind => 'SCALAR',
  name => 'Boolean',
  ofType => null,
  description => <<"The `Boolean` scalar type represents `true` or `false`.">>,

  serialize => fun serialize/3,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value,_,_) -> coerce(Value).

parse_value(Value, _) -> coerce(Value).

parse_literal(null, _) -> null;
parse_literal(#{kind := 'BooleanValue', value := Value}, _) -> Value;
parse_literal(#{kind := Kind}, _) ->
  throw({error, type_validation, <<"Unexpected type ", (atom_to_binary(Kind, utf8))/binary, ", expected BooleanValue">>}).


%% TODO: add binary and string representation
-spec coerce(integer() | boolean() | binary() | list()) -> boolean().
coerce(null) -> null;
coerce(Value) when is_boolean(Value) -> Value;
coerce(_) -> throw({error, type_validation, <<"Cannot coerce boolean type">>}).