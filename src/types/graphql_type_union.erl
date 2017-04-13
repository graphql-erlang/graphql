-module(graphql_type_union).

%% API
-export([
  type/3, type/4
]).

-spec type(binary(), binary(), list( function() )) -> map().
type(Name, Description, PossibleTypes)->
  type(Name, Description, PossibleTypes, fun({Type, Value}, _) ->
    #{ name := TypeName } = graphql_type:unwrap_type(Type),
    {find_type(PossibleTypes, TypeName), Value}
  end).

-spec type(binary(), binary(), list(map() | function()), function()) -> map().
type(Name0, Description, PossibleTypes, ResolveType)->

  % Fixme #63 https://github.com/graphql-erlang/graphql/issues/63
  Name = case is_atom(Name0) of
    true -> Name0;
    false -> graphql_type:optional_string(Name0)
  end,

  #{
    kind => 'UNION',
    name => Name,
    description => graphql_type:optional_string(Description),

    possibleTypes => PossibleTypes,

    resolve_type => ResolveType
  }.


find_type([], _) -> throw({error, union, <<"Cannon find union type">>});
find_type([Type|Tail], ExpectedTypeName) ->
  case graphql_type:unwrap_type(Type) of
    #{ name := ExpectedTypeName} = ExpectedType -> ExpectedType;
    _ -> find_type(Tail, ExpectedTypeName)
  end.
