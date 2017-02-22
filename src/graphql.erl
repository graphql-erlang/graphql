%%%-------------------------------------------------------------------
%%% @author mrchex
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. Jan 2017 12:25 PM
%%%-------------------------------------------------------------------
-module(graphql).
-author("mrchex").

%% API
-export([
  schema/1,
  objectType/2, objectType/3,
  field/3, field/4, field/5,
  arg/2, arg/3,
  execute/3, execute/4, execute/5, execute/6,
  upmap/3, pmap/3
]).

%%%% schema definitions helpers %%%%

% Definition is #{ query => objectType }
schema(#{query := QueryRootDefinition} = Schema) ->
  QueryRoot = case is_map(QueryRootDefinition) of
    true -> QueryRootDefinition;
    _ -> QueryRootDefinition()
  end,

  Schema#{
    query => graphql_schema_introspection:inject(QueryRoot)
  }.

objectType(Name, Fields) -> objectType(Name, null, Fields).
objectType(Name, Description, Fields)->

  #{
    kind => <<"OBJECT">>,
    name => Name,
    description => Description,
    fields => Fields
  }.


field(Name, Type, Description)-> field(Name, Type, Description, undefined, undefined).
field(Name, Type, Description, Resolver) -> field(Name, Type, Description, undefined, Resolver).
field(Name, Type, Description, ArgsDefinitions, Resolver) ->

  % collect to map result of arg/2..3
  Args = lists:foldl(fun(#{name := ArgName} = Arg, Acc) ->
    Acc#{ ArgName => Arg }
  end, #{}, ArgsDefinitions),

  #{
    name => Name,
    type => Type,
    desctiption => Description,
    args => Args,
    resolver => Resolver
  }.

arg(Name, Type) -> arg(Name, Type, undefined).
arg(Name, Type, Default) ->
  #{
    name => Name,
    type => Type,
    default => Default
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



