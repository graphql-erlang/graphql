-module(graphql_type).

%% API
-export([
  unwrap_type/1
]).


unwrap_type(Type) ->
  case Type of
    #{kind := _} -> Type;
    _ when is_function(Type) -> Type();
    _ ->
      io:format("Cannot unwrap type: ~p~n", [Type]),
      throw({error, field_type, <<"Unexpected type">>})
  end.
