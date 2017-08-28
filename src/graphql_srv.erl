-module(graphql_srv).
-behaviour(gen_server).

%% API
-export([
  start_link/1,  % without options
  start_link/2   % with options
]).

-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
  schema,
  types,
  options
}).

start_link(SchemaDef) -> start_link(SchemaDef, #{}).
start_link(SchemaDef, Options) ->
  ServerName = maps:get(server_name, Options, ?SERVER),

  % Check Schema to be actual ?SCHEMA
  case graphql_type:silent_unwrap_type(SchemaDef) of
    {ok, #{'__introspection_inject' := true}} -> {error, "Schema includes introspection"};
    {ok, Schema} -> gen_server:start_link({local, ServerName}, ?MODULE, [Schema, Options], []);
    {error, Error} -> {error, {invalid_schema, Error}}
  end.

init([Schema0, Options]) ->

  Schema = case maps:get(introspection, Options, false) of
    false -> Schema0;
    true ->
      Query = maps:get(query, Schema0),
      QueryFields = maps:get(fields, Query),
      Schema0#{
        query => Query#{
          fields => QueryFields#{
            <<"__schema">> => graphql_introspection_types:get_schema_field()
          }
        }
      }
  end,

  case collect_types(Schema) of
    {error, name_collision, Edge} ->
      {stop, {"Schema must contain unique named types but contains multiple types named", maps:get(name, Edge)}};

    {ok, Schema1, Types} ->
      {ok, #state{ schema = Schema1, types = Types, options = Options }}
  end.

handle_call({exec, Document, Options}, From, State)->

  WorkerPid = proc_lib:spawn(fun() ->
    Result = execute(Document, Options, State),
    gen_server:reply(From, Result)
  end),

  erlang:send_after(4500, self(), {execution_timeout, From, WorkerPid}),

  {noreply, State};

handle_call({get_type, TypeName}, _From, State) ->
  Types = State#state.types,
  {reply, maps:get(TypeName, Types), State};

handle_call({reload, SchemaDef}, _, State) ->
  case graphql_type:silent_unwrap_type(SchemaDef) of
    {error, Error} -> {reply, {invalid_schema, Error}, State};
    {ok, #{'__introspection_inject' := true}} -> {reply, {error, "Schema includes introspection"}, State};
    {ok, Schema} ->
      case init([Schema, State#state.options]) of
        {ok, NewState} -> {reply, ok, NewState};
        Error -> {reply, {error, Error}, State}
      end
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info({execution_timeout, From, WorkerPid}, State) ->
  case is_process_alive(WorkerPid) of
    true ->
      erlang:exit(WorkerPid, execution_timeout),
      gen_server:reply(From, {error, timeout}),
      {noreply, State};

    false ->
      {noreply, State}
  end;

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

% Async execution
execute(Document, Options, #state{ schema = Schema, types = Types}) ->

  Context = maps:get(context, Options, #{}),
  graphql:exec(Schema, Document, Options#{
    context => Context#{
      '__types' => Types
    }
  }).


% Compile schema
collect_types(Schema) ->
  Operations0 = case maps:get(query, Schema, null) of
    null -> [];
    Query -> [Query]
  end,

  Operations1 = case maps:get(mutation, Schema, null) of
    null -> Operations0;
    Mutation -> [Mutation|Operations0]
  end,

  Operations = case maps:get(subscription, Schema, null) of
    null -> Operations1;
    Subscription -> [Subscription|Operations1]
  end,

  case collect_types(Operations, [], []) of
    {ok, Names, Types} ->

      NameToAtom = lists:foldl(fun
        (X, Acc) when is_binary(X) ->
          dbg("DEPRECATION WARNING. Type name must be atom but got binary: ~p", [X]),
          Acc#{X => binary_to_atom(X, utf8)};
        (X, Acc) when is_list(X) ->
          dbg("DEPRECATION WARNING. Type name must be atom but got list: ~p", [X]),
          Acc#{X => list_to_atom(X)};
        (X, Acc) when is_atom(X) -> Acc#{X => X}
      end, #{}, Names),

      { ok
      , Schema#{
          query => schema_operation_name(query, Schema, NameToAtom),
          mutation => schema_operation_name(mutation, Schema, NameToAtom),
          subscription => schema_operation_name(subscription, Schema, NameToAtom)
        }
      , flat_graph(NameToAtom, Types)
      };

    Error -> Error
  end.

schema_operation_name(Operation, Schema, NameToAtom)->
  case maps:get(Operation, Schema, null) of
    null -> null;
    #{name := Name} -> maps:get(Name, NameToAtom)
  end.

collect_types([], VisitedTypes, Acc) -> {ok, VisitedTypes, Acc};
collect_types([#{kind := Kind, ofType := OfType}|TypesTail], V, A) when
  Kind =:= 'LIST' orelse
    Kind =:= 'NON_NULL' ->
  collect_types([graphql_type:unwrap_type(OfType)|TypesTail], V, A);
collect_types([Edge|TypesTail], VisitedTypes, Acc)->
  EdgeName = maps:get(name, Edge),

  % check is visited
  case lists:member(EdgeName, VisitedTypes) of
    % skip this edge, but add inner types for inspection
    true ->
      % check is this type actually visited
      case lists:member(Edge, Acc) of
        true ->  collect_types(TypesTail, VisitedTypes, Acc);
        false -> {error, name_collision, Edge}
      end;
    % collect this edge and add to inner types for inspection
    false ->

      InnerTypes = case Edge of
        #{kind := 'OBJECT'} -> extract_field_types(Edge, Acc);
        #{kind := 'UNION', possibleTypes := PossibleTypes} ->
          [graphql_type:unwrap_type(X) || X <- PossibleTypes];
        _ -> []
      end,

      collect_types(InnerTypes ++ TypesTail, [EdgeName|VisitedTypes], [Edge|Acc])
  end.

extract_field_types(Object, IgnoreTypes) ->
  maps:fold(fun(_, #{type := FieldType} = Field, Acc)->
    Type = graphql_type:unwrap_type(FieldType),
    ArgsTypes = case maps:get(args, Field, null) of
      null -> [];
      Args -> extract_field_types(#{fields => Args}, IgnoreTypes)
    end,

    case lists:member(Type, IgnoreTypes) of
      true -> ArgsTypes ++ Acc;
      false -> ArgsTypes ++ [Type|Acc]
    end
  end, [], maps:get(fields, Object)).


flat_graph(NameToAtom, Types)->

  FlatTypes = lists:map(fun(Type) ->
    unwrap(Type, NameToAtom)
  end, Types),

  lists:foldl(fun(Type, Acc) ->
    TypeName = maps:get(name, Type),
    TypeNameAtom = maps:get(TypeName, NameToAtom),
    Acc#{TypeNameAtom => Type#{
      name => TypeNameAtom
    }}
  end, #{}, FlatTypes).

unwrap(#{kind := 'OBJECT', fields := Fields} = Object, NameToAtom)->
  Object#{
    fields => maps:fold(fun(Key, Type, Acc) ->
      Acc#{ Key => unwrap(Type, NameToAtom) }
    end, #{}, Fields)
  };
unwrap(#{kind := 'FIELD', type := Type, args := Args} = Field, NameToAtom)->
  UnwrappedType = graphql_type:unwrap_type(Type),
  Field#{
    type => type_to_atom(UnwrappedType, NameToAtom),
    args => maps:fold(fun(Key, Arg, Acc) ->
      Acc#{ Key => unwrap(Arg, NameToAtom) }
    end, #{}, Args)
  };
unwrap(#{kind := 'INPUT_VALUE', type := Type} = InputValue, NameToAtom) ->
  UnwrappedType = graphql_type:unwrap_type(Type),
  InputValue#{
    type => type_to_atom(UnwrappedType, NameToAtom)
  };
unwrap(Type, _) ->
  case maps:get(kind, Type, null) of
    null ->
      dbg("ERROR: Wrong type: ~p", [Type]),
      throw("Wrong type. All types should have kind");
    _ -> Type
  end.

type_to_atom(#{ofType := InnerType, name := null} = Type, NameToAtom) -> % list, non null types
  case graphql_type:unwrap_type(InnerType) of
    #{ofType := _} = InnerTypeUnwrapped ->
      Type#{ ofType => type_to_atom(InnerTypeUnwrapped, NameToAtom) };
    #{name := InnerTypeName} ->
      Type#{ ofType => maps:get(InnerTypeName, NameToAtom) }
  end;

type_to_atom(#{name := TypeName}, NameToAtom)->
  maps:get(TypeName, NameToAtom).

dbg(T)-> dbg(T, []).
dbg(T, A)-> io:format(" * " ++ T ++ "~n", A).
