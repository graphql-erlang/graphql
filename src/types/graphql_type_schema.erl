-module(graphql_type_schema).

%% API
-export([
  new/1, new/2
]).

-type t() :: #{
  query => map() | null,
  mutation => map() | null,
  subscription => map() | null
}.

-spec new(map()) -> t().
new(Schema) -> new(Schema, false).

-spec new(map(), boolean()) -> t().
new(Schema, InjectIntrospection) when is_boolean(InjectIntrospection) ->

  Query = case {maps:get(query, Schema, null), InjectIntrospection} of
    {null, _} -> null;
    {Query1, false} -> graphql_type:unwrap_type(Query1);
    {Query1, true} -> graphql_introspection:inject(graphql_type:unwrap_type(Query1))
  end,

  Mutation = case maps:get(mutation, Schema, null) of
    null -> null;
    Mutation0 -> graphql_type:unwrap_type(Mutation0)
  end,

  Subscription = case maps:get(subscription, Schema, null) of
    null -> null;
    Subscription0 -> graphql_type:unwrap_type(Subscription0)
  end,

  Schema#{
    kind => 'SCHEMA',
    '__introspection_inject' => InjectIntrospection,

    query => Query,
    mutation => Mutation,
    subscription => Subscription

  }.
