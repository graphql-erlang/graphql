-module(graphql).
-include("types.hrl").

%% API
-export([

  % back capability
  schema/1,
  objectType/2, objectType/3,


  % execution
  exec/2, exec/3,
  execute/3, execute/4, execute/5, execute/6, % deprecated, use exec instead

  % helpers
  upmap/3, pmap/3  % not used now
]).

%%%% for macros haters %%%%

schema(Schema) -> ?SCHEMA(Schema).

objectType(Name, Fields) -> ?OBJECT(Name, Fields).
objectType(Name, Description, Fields)-> ?OBJECT(Name, Description, Fields).

%%%% execution %%%%
exec(Schema, Document) -> exec(Schema, Document, #{}).
exec(Schema, Document, Options)->
  InitialValue = maps:get(initial, Options, #{}),
  VariableValues = maps:get(variable_values, Options, #{}),
  OperationName = maps:get(operation_name, Options, null),
  Context = maps:get(context, Options, #{}),
  ReturnMaps = maps:get(return_maps, Options, true),

  case graphql_parser:parse(Document) of
    {ok, DocumentParsed} ->
      case graphql_execution:execute(Schema, DocumentParsed, OperationName, VariableValues, InitialValue, Context) of
        #{data := [{_,_}|_] = Proplist} ->
          case ReturnMaps of
            true -> #{data => to_map_recursive(Proplist)}
          end;
        #{errors := _} = Resp -> Resp
      end;

    {error, {Line, graphql_parser_yecc, Reason}} -> {#{errors => [#{
      line => Line,
      message => Reason
    }]}, Context}
  end.

% execute is deprecated
execute(Schema, Document, InitialValue)->
  execute(Schema, Document, null, #{}, InitialValue, #{}).
execute(Schema, Document, InitialValue, Context)->
  execute(Schema, Document, null, #{}, InitialValue, Context).
execute(Schema, Document, VariableValues, InitialValue, Context)->
  execute(Schema, Document, null, VariableValues, InitialValue, Context).
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context)->
  case graphql_parser:parse(Document) of
    {ok, DocumentParsed} ->
      case graphql_execution:execute(Schema, DocumentParsed, OperationName, VariableValues, InitialValue, Context) of
        #{data := _} = Res -> Res#{ errors => []};
        Res -> Res
      end;
    {error, {_, _, Error}} ->
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

%%%-------------------------------------------------------------------
%% @doc
%% Convert proplist to map recursively
%% @end
%%%-------------------------------------------------------------------
-spec to_map_recursive(list()) -> maps:map().
to_map_recursive(Proplist) ->
  recursive_apply(fun maps:from_list/1, Proplist).


%%%-------------------------------------------------------------------
%% @doc
%% Recursively apply Fun to proplist (or list of proplists)
%% @end
%%%-------------------------------------------------------------------
-spec recursive_apply(fun((term()) -> term()), list()) -> term().
recursive_apply(Fun, [{_,_}|_] = Proplist) ->
  InnerRec = lists:map(fun({K, V}) ->
    V2 = case V of
      [{_,_}|_] -> recursive_apply(Fun, V);
      V when is_list(V) -> lists:map(fun(X) -> recursive_apply(Fun, X) end, V);
      _ -> V
    end,
    {K, V2}
  end, Proplist),
  Fun(InnerRec);

recursive_apply(Fun, List) when is_list(List) ->
  lists:map(fun(X) -> recursive_apply(Fun, X) end, List);

recursive_apply(_Fun, Val) ->
  Val.