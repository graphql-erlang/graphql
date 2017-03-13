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
type(Name, Description, PossibleTypes, ResolveType)->

  #{
    kind => 'UNION',
    name => Name,
    description => Description,

    possibleTypes => PossibleTypes,

    resolve_type => ResolveType
  }.


find_type([], _) -> throw({error, union, <<"Cannon find union type">>});
find_type([Type|Tail], ExpectedTypeName) ->
  case graphql_type:unwrap_type(Type) of
    #{ name := ExpectedTypeName} = ExpectedType -> ExpectedType;
    _ -> find_type(Tail, ExpectedTypeName)
  end.