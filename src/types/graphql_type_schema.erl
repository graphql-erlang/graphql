-module(graphql_type_schema).
%%-behaviour(graphql_type_abstract).

%% API
-export([
  new/1
]).

-type t() :: #{
  query => map(),
  mutation => map() | null
}.

-spec new(map()) -> t().
new(#{query := QueryRootDefinition} = Schema) ->
  QueryRoot = case is_map(QueryRootDefinition) of
    true -> QueryRootDefinition;
    _ -> QueryRootDefinition()
  end,

  Schema#{
    query => graphql_schema_introspection:inject(QueryRoot),
    mutation => null,
    directives => []
  }.