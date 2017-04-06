-module(graphql_type).

%% API
-export([
  unwrap_type/1,
  silent_unwrap_type/1,
  optional_string/1,
  deprecated/2
]).

-export_type([
  optional_string/0,
  type/0
]).

-type type() :: map() | fun() | {map() | fun(), binary() | string()}.
-type optional_string() :: string() | binary().

-spec unwrap_type(type()) -> map().
unwrap_type(Type) ->
  case Type of
    #{kind := _} -> Type;
    _ when is_function(Type) -> Type();
    _ ->
      io:format("Cannot unwrap type: ~p~n", [Type]),
      throw({error, field_type, <<"Unexpected type">>})
  end.

-spec silent_unwrap_type(type()) -> map().
silent_unwrap_type(Type) when is_function(Type) -> silent_unwrap_type(Type());
silent_unwrap_type(#{kind := _} = Type) -> {ok, Type};
silent_unwrap_type(_) -> {error, invalid_type}.



-spec optional_string(null | binary() | string()) -> binary() | null.
optional_string(null) -> null;
optional_string(V) when is_list(V) -> list_to_binary(V);
optional_string(V) when is_binary(V) -> V.


-spec deprecated(DeprecationReason::optional_string(), map()) -> map().
deprecated(DeprecationReason, #{kind := Kind} = FieldOrEnumVal)
  when Kind =:= 'FIELD'
  orelse Kind =:= 'ENUM_VALUE' ->
    FieldOrEnumVal#{
      isDeprecated => true,
      deprecationReason => graphql_type:optional_string(DeprecationReason)
    }.