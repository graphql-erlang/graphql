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
  execute/3, execute/4, execute/5
]).

%%%% schema definitions helpers %%%%

% Definition is #{ query => objectType }
schema(#{query := QueryRootDefinition}) ->
  QueryRoot = case is_map(QueryRootDefinition) of
    true -> QueryRootDefinition;
    _ -> QueryRootDefinition()
  end,

  #{
    query => QueryRoot
  }.

objectType(Name, Fields) -> objectType(Name, null, Fields).
objectType(Name, Description, Fields)->

  #{
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
  execute(Schema, Document, null, #{}, InitialValue).
execute(Schema, Document, VariableValues, InitialValue)->
  execute(Schema, Document, null, VariableValues, InitialValue).
execute(Schema, Document, OperationName, VariableValues, InitialValue)->
  case graphql_parser:parse(Document) of
    {ok, DocumentParsed} ->
      graphql_execution:execute(Schema, DocumentParsed, OperationName, VariableValues, InitialValue);
    {error, Error} ->
      #{error => Error}
  end.



