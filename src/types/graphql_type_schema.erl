-module(graphql_type_schema).

%% API
-export([
  new/1, new/2
]).

-type t() :: #{
  query => map(),
  mutation => map() | null
}.

-spec new(map()) -> t().
new(Schema) -> new(Schema, true).

-spec new(map(), boolean()) -> t().
new(Schema, InjectIntrospection) ->

  Query = case {maps:get(query, Schema, null), InjectIntrospection} of
    {null, _} -> null;
    {Query1, false} -> graphql_type:unwrap_type(Query1);
    {Query1, true} -> graphql_introspection:inject(graphql_type:unwrap_type(Query1))
  end,


  Mutation = case maps:get(mutation, Schema, null) of
    null -> null;
    Mutation0 -> graphql_type:unwrap_type(Mutation0)
  end,

  Schema#{
    query => Query,
    mutation => Mutation
  }.