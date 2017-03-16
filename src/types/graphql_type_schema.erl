-module(graphql_type_schema).

%% API
-export([
  new/1
]).

-type t() :: #{
  query => map(),
  mutation => map() | null
}.

-spec new(map()) -> t().
new(Schema) ->
  Query = graphql_type:unwrap_type(maps:get(query, Schema)),
  Mutation = case maps:get(mutation, Schema, null) of
    null -> null;
    Mutation0 -> graphql_type:unwrap_type(Mutation0)
  end,

  Schema#{
    query => graphql_introspection:inject(Query),
    mutation => Mutation
  }.