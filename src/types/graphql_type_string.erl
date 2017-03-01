-module(graphql_type_string).

-export([
  type/0
]).

type()-> #{
  kind => 'SCALAR',
  name => 'String',
  ofType => null,
  description => <<
    "The `String` scalar type represents textual data, represented as UTF-8",
    "character sequences. The String type is most often used by GraphQL to",
    "represent free-form human-readable text."
  >>,

  serialize => fun serialize/1,
  parse_value => fun parse_value/2,
  parse_literal => fun parse_literal/2
}.

serialize(Value) -> coerce(Value).
parse_value(Value, _) -> coerce(Value).

-spec parse_literal(map(), map()) -> binary().
parse_literal(#{kind := 'StringValue', value := Value}, _) -> Value;
parse_literal(#{kind := Kind}, _) ->
  throw({error, type_validation, <<"Unexpected type ", (atom_to_binary(Kind, utf8))/binary, ", expected StringValue">>}).


-spec coerce(atom() | binary() | list()) -> binary().
coerce(null) -> null;
coerce(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
coerce(Value) when is_binary(Value) -> Value;
coerce(Value) when is_list(Value) -> list_to_binary(Value);
coerce(_) -> throw({error, type_validation, <<"Cannot coerce string type">>}).