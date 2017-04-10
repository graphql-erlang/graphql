-module(graphql_type_enum_value).

%% API
-export([
  type/3
]).

-spec type(Value::any(), Name::graphql_type:optional_string(), Description::graphql_type:optional_string()|null) -> graphql_type:type().
type(Value, Name0, Description)->

  % Fixme #63 https://github.com/graphql-erlang/graphql/issues/63
  Name = case is_atom(Name0) of
    true -> Name0;
    false -> graphql_type:optional_string(Name0)
  end,

  #{
    kind => 'ENUM_VALUE',
    value => Value,
    name => Name,
    description => graphql_type:optional_string(Description),
    isDeprecated => false,
    deprecationReason => null
  }.

