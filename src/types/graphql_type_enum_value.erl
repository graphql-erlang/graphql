-module(graphql_type_enum_value).

%% API
-export([
  type/3
]).

-spec type(Value::any(), Name::graphql_type:optional_string(), Description::graphql_type:optional_string()|null) -> graphql_type:type().
type(Value, Name, Description)->
  #{
    kind => 'ENUM_VALUE',
    value => Value,
    name => graphql_type:optional_string(Name),
    description => graphql_type:optional_string(Description),
    isDeprecated => false,
    deprecationReason => null
  }.

