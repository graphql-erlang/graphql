-module(graphql).
-include("types.hrl").

%% API
-export([
  schema/1,
  objectType/2, objectType/3,
  execute/3, execute/4, execute/5, execute/6,
  upmap/3, pmap/3
]).

%%%% schema definitions helpers %%%%

% Definition is #{ query => objectType }
-spec schema(map()) -> map().
schema(#{query := QueryRootDefinition} = Schema) ->
  QueryRoot = case is_map(QueryRootDefinition) of
    true -> QueryRootDefinition;
    _ -> QueryRootDefinition()
  end,

  Schema#{
    query => graphql_schema_introspection:inject(QueryRoot)
  }.

-spec objectType(binary(), map())-> map().
-spec objectType(binary(), binary()|null, map())-> map().
objectType(Name, Fields) -> objectType(Name, null, Fields).
objectType(Name, Description, Fields)->
  #{
    kind => 'OBJECT',
    name => Name,
    ofType => null,
    description => Description,
    fields => Fields#{
      <<"__typename">> => #{
        type => ?STRING,
        description => <<"Name of current type">>,
        resolver => fun() -> Name end
      }
    }
  }.


%%%% execution %%%%

execute(Schema, Document, InitialValue)->
  execute(Schema, Document, null, #{}, InitialValue, #{}).
execute(Schema, Document, InitialValue, Context)->
  execute(Schema, Document, null, #{}, InitialValue, Context).
execute(Schema, Document, VariableValues, InitialValue, Context)->
  execute(Schema, Document, null, VariableValues, InitialValue, Context).
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context)->
  io:format("parsing start~n"),
  {T, R} = timer:tc(graphql_parser, parse, [Document]),
  io:format("Parse time: ~p~n", [T]),
  case R of
    {ok, DocumentParsed} ->
      graphql_execution:execute(Schema, DocumentParsed, OperationName, VariableValues, InitialValue, Context);
    {error, Error} ->
      #{error => Error}
  end.


%%%% helpers %%%%

-spec upmap(fun(), list(), integer()) -> list().
upmap(F, L, Timeout) ->
  Parent = self(),
  Ref = make_ref(),
  [receive {Ref, Result} -> Result after Timeout -> throw(timeout) end
    || _ <- [spawn(fun () -> Parent ! {Ref, F(X)} end) || X <- L]].

-spec pmap(fun(), list(), integer()) -> list().
pmap(F, L, Timeout) ->
  Parent = self(),
  L2 = lists:map(fun(El) ->
    Ref = make_ref(),
    spawn(fun() -> Parent ! {Ref, F(El)} end),
    Ref
  end, L),

  [receive {Ref, Result} -> Result after Timeout -> throw(timeout) end
    || Ref <- L2].



