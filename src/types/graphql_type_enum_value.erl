-module(graphql_type_enum_value).

%% API
-export([
  type/3, type/4
]).

type(Value, Name, Description) -> type(Value, Name, Description, #{}).
type(Value, Name, Description, Overwrite)->
  Base = #{
    kind => 'ENUM_VALUE',
    value => Value,
    name => Name,
    description => Description,
    isDeprecated => false,
    deprecationReason => null
  },
  maps:merge(Base, Overwrite).

