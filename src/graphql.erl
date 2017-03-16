-module(graphql).
-include("types.hrl").

%% API
-export([

  % back capability
  schema/1,
  objectType/2, objectType/3,


  % execution
  execute/3, execute/4, execute/5, execute/6,
  upmap/3, pmap/3
]).

%%%% for macros haters %%%%

schema(Schema) -> ?SCHEMA(Schema).

objectType(Name, Fields) -> ?OBJECT(Name, Fields).
objectType(Name, Description, Fields)-> ?OBJECT(Name, Description, Fields).

%%%% execution %%%%

execute(Schema, Document, InitialValue)->
  execute(Schema, Document, null, #{}, InitialValue, #{}).
execute(Schema, Document, InitialValue, Context)->
  execute(Schema, Document, null, #{}, InitialValue, Context).
execute(Schema, Document, VariableValues, InitialValue, Context)->
  execute(Schema, Document, null, VariableValues, InitialValue, Context).
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context)->
  case graphql_parser:parse(Document) of
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

