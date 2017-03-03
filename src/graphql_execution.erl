-module(graphql_execution).
-author("mrchex").

%% API
-export([
  execute/6
]).

%%print(Text)-> print(Text, []).
print(Text, Args) -> io:format(Text ++ "~n", Args).

% Operation name can be null
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context0)->
  Context = Context0#{
    '__schema' => Schema,
    '__fragments' => collect_fragments(Document)
  },
  try executor(Schema, Document, OperationName, VariableValues, InitialValue, Context) of
    Result -> Result
  catch
    {error, Type, Msg} ->
      print("Error in ~p! Msg: ~p", [Type, Msg]),
      #{error => Msg, type => Type}
  end.


executor(Schema, Document, OperationName, VariableValues, InitialValue, Context)->
  Operation = get_operation(Document, OperationName),
  CoercedVariableValues = coerceVariableValues(Schema, Operation, VariableValues),

  case Operation of
    #{operation := query} ->
      execute_query(Operation, Schema, CoercedVariableValues, InitialValue, Context);
    #{operation := WantedOperation} ->
      throw({error, execute, <<"Currently operation ", WantedOperation/binary, " does not support">>})
  end.

% throw validation error when operation not found or document define multiple
get_operation(Document, OperationName)->
  Definitions = maps:get(definitions, Document, []),
  case get_operation_from_definitions(Definitions, OperationName) of
    {ok, Operation} -> Operation;
    {error, Error} -> throw({error, get_operation, Error})
  end.

get_operation_from_definitions(Definitions, OperationName) ->
  case get_operation_from_definitions(Definitions, OperationName, undefined) of
    undefined -> {error, <<"Operation not found">>};
    Operation -> {ok, Operation}
  end.


% TODO: has no spec result: when expented only one operation given first
% FIXME: http://facebook.github.io/graphql/#sec-Executing-Requests
get_operation_from_definitions([], _, Operation)-> Operation;
% when we first meet operation - continue with what name
get_operation_from_definitions([#{kind := 'OperationDefinition', operation := OperationName} = Operation|Tail], null, _)->
  get_operation_from_definitions(Tail, OperationName, Operation);
% when we meet another operation that named like we already founded
get_operation_from_definitions([#{kind := 'OperationDefinition', operation := OperationName}|_], OperationName, _)->
  {error, <<"Document defines multiple operations, otherwise the document is expected to only contain a single operation">>};
get_operation_from_definitions([_|Tail], OperationName, Operation)->
  get_operation_from_definitions(Tail, OperationName, Operation).


% TODO: implement me http://facebook.github.io/graphql/#CoerceVariableValues()
coerceVariableValues(Schema, Operation, VariableValues)->
  #{}.

% TODO: complete me http://facebook.github.io/graphql/#CoerceArgumentValues()
coerceArgumentValues(ObjectType, Field, VariableValues, Context) ->
  Arguments = maps:get(arguments, Field, []),
  FieldName = get_field_name(Field),
  ArgumentDefinitions = graphql_schema:get_argument_definitions(FieldName, ObjectType),

%%  print("ARGUMENT DEFINITIONS: ~p", [ArgumentDefinitions]),

  maps:fold(fun(ArgumentName, ArgumentDefinition, CoercedValues) ->
    ArgumentType = graphql_schema:get_type_from_definition(ArgumentDefinition),
    DefaultValue = graphql_schema:get_argument_default(ArgumentDefinition),

    print("ARGUMENT COERCE: ~p", [ArgumentName]),

    % 5 of http://facebook.github.io/graphql/#sec-Coercing-Field-Arguments
    CoercedValue = case get_field_argument_by_name(ArgumentName, Field) of

      #{  % h.Let coercedValue be the result of coercing value
        kind := 'Argument',
        name := #{kind := 'Name', value := ArgumentName},
        value := Value0
      } ->
        ParseLiteral = maps:get(parse_literal, ArgumentType),
        ParseLiteral(Value0, ArgumentType);

      % f. Otherwise, if value does not exist (was not provided in argumentValues:
      % f.i. If defaultValue exists (including null):
      % f.i.1. Add an entry to coercedValues named argName with the value defaultValue.
      undefined ->
        ParseValue = maps:get(parse_value, ArgumentType),
        ParseValue(DefaultValue, ArgumentType)

      % f.iii - Otherwise, continue to the next argument definition.
    end,

    CoercedValues#{ ArgumentName => CoercedValue}
  end, #{}, ArgumentDefinitions).


% http://facebook.github.io/graphql/#sec-Executing-Operations
execute_query(Query, Schema, VariableValues, InitialValue, Context) ->
  QueryType = maps:get(query, Schema),
  SelectionSet = maps:get(selectionSet, Query),
%%  Data = execute_selection_set(SelectionSet, QueryType, InitialValue, VariableValues, Context),
  Parallel = false,  % FIXME: enable parallel when we can
  {T, Data} = timer:tc(fun execute_selection_set/6, [SelectionSet, QueryType, InitialValue, VariableValues, Context, Parallel]),
  io:format("EXECUTE SELECTION SET TIMER: ~p~n", [T]),
  #{
    data => Data,
    errors => []
  }.

% http://facebook.github.io/graphql/#sec-Executing-Selection-Sets
execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Context)->
  execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Context, false).

execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Context, Parallel)->
  Fragments = maps:get('__fragments', Context),
  GroupedFieldSet = collect_fields(ObjectType, SelectionSet, VariableValues, Fragments),

  MapFun = fun({ResponseKey, Fields})->
    % 6.3 - 3.a. Let fieldName be the name of the first entry in fields.
    #{value := FieldName} = maps:get(name, lists:nth(1, Fields)),
    Field = case graphql_schema:get_field(FieldName, ObjectType) of
      undefined ->
        ErrorMsg = <<
          "Field `", FieldName/binary,
          "` does not exist in ObjectType `",
          (graphql_schema:get_name(ObjectType))/binary, "`"
        >>,
        throw({error, validation_error, ErrorMsg});
      Field0 -> Field0
    end,

    % TODO: Must be implemented when we learn why its needed and what the point of use case
    % TODO: c.If fieldType is null:
    % TODO:    i.Continue to the next iteration of groupedFieldSet.
    FieldType = graphql_schema:get_type_from_definition(Field),

    ResponseValue = executeField(ObjectType, ObjectValue, Fields, FieldType, VariableValues, Context),
    {ResponseKey, ResponseValue}

  end,

  case Parallel of
    true -> graphql:upmap(MapFun, GroupedFieldSet, 5000);
    false -> lists:map(MapFun, GroupedFieldSet)
  end.

collect_fragments(Document) ->
  lists:foldl(fun(Definition, Fragments) ->
    Kind = maps:get(kind, Definition),
    case Kind of
      'FragmentDefinition' ->
        FragmentName = maps:get(value, maps:get(name, Definition)),
        Fragments#{FragmentName => Definition};
      _ -> Fragments
    end
  end, #{}, maps:get(definitions, Document)).

% TODO: does not support directives and inline fragments (3.a, 3.b, 3.e): http://facebook.github.io/graphql/#CollectFields()
collect_fields(ObjectType, SelectionSet, VariableValues, Fragments) ->
  collect_fields(ObjectType, SelectionSet, VariableValues, Fragments, []).
collect_fields(ObjectType, SelectionSet, VariableValues, Fragments, VisitedFragments0) ->
  Selections = maps:get(selections, SelectionSet),
  {CollectedFields, _} = lists:foldl(fun(Selection, {GroupedFields, VisitedFragments})->
    case Selection of

      % 3.c
      #{kind := 'Field'} ->
        ResponseKey = get_response_key_from_selection(Selection),
        GroupForResponseKey = proplists:get_value(ResponseKey, GroupedFields, []),

        {[
          {ResponseKey, [Selection|GroupForResponseKey]}
          | GroupedFields
        ], VisitedFragments};

      % 3.d
      #{kind := 'FragmentSpread', name := #{kind := 'Name', value := FragmentSpreadName}} ->
        case lists:member(FragmentSpreadName, VisitedFragments) of
          true -> {GroupedFields, VisitedFragments};
          false ->
            case maps:get(FragmentSpreadName, Fragments, undefined) of
              undefined -> {GroupedFields, VisitedFragments};  % 3.d.v
              Fragment ->
                #{
                  typeCondition := #{
                    kind := 'NamedType',
                    name := #{kind := 'Name', value := FragmentType}
                  }
                } = Fragment,
                case does_fragment_type_apply(ObjectType, FragmentType) of
                  false -> {GroupedFields, VisitedFragments};  % 3.d.vii
                  true ->
                    FragmentSelectionSet = maps:get(selectionSet, Fragment),
                    FragmentGroupedField = collect_fields(ObjectType, FragmentSelectionSet, VariableValues, Fragments, [FragmentSpreadName|VisitedFragments]),
                    {GroupedFields ++ FragmentGroupedField, VisitedFragments}
                end
            end
        end
    end
  end, {[], VisitedFragments0}, Selections),
  CollectedFields.

does_fragment_type_apply(ObjectType, FragmentType)->
  case ObjectType of
    #{ name := FragmentType } -> true;
    _ -> false
  end.

get_response_key_from_selection(#{alias := #{value := Key}}) -> Key;
get_response_key_from_selection(#{name := #{value := Key}}) -> Key.

executeField(ObjectType, ObjectValue, [Field|_]=Fields, FieldType, VariableValues, Context)->
  ArgumentValues = coerceArgumentValues(ObjectType, Field, VariableValues, Context),
  FieldName = get_field_name(Field),

  case resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context) of
    {ResolvedValue, OverwritenContext} ->
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, OverwritenContext);
    ResolvedValue ->
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, Context)
  end.


get_field_name(#{name := #{value := FieldName}}) -> FieldName.

get_field_arguments(Field)->
  case maps:get(arguments, Field, null) of
    null -> [];
    Args -> Args
  end.

find_argument(_, []) -> undefined;
find_argument(ArgumentName, [#{name := #{ value := ArgumentName }} = Arg|_])-> Arg;
find_argument(ArgumentName, [_|Tail])-> find_argument(ArgumentName, Tail).

get_field_argument_by_name(ArgumentName, Field)->
  Arguments = get_field_arguments(Field),
  find_argument(ArgumentName, Arguments).

resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context)->
  Resolver = graphql_schema:get_field_resolver(FieldName, ObjectType),
  case erlang:fun_info(Resolver, arity) of
    {arity, 0} -> Resolver();
    {arity, 1} -> Resolver(ObjectValue);
    {arity, 2} -> Resolver(ObjectValue, ArgumentValues);
    {arity, 3} -> Resolver(ObjectValue, ArgumentValues, Context)
  end.

% TODO: complete me http: //facebook.github.io/graphql/#CompleteValue()
completeValue(FieldTypeDefinition, Fields, Result, VariablesValues, Context)->

  % unwrap type
  FieldType = graphql_type:unwrap_type(FieldTypeDefinition),

  case FieldType of
    #{kind := 'SCALAR', serialize := Serialize} ->
      case erlang:fun_info(Serialize, arity) of
        {arity, 1} -> Serialize(Result);
        {arity, 2} -> Serialize(Result, Context)
      end;
    #{kind := 'OBJECT', name := Name} ->
      case Result of
        null -> null;
        _ ->
          case mergeSelectionSet(Fields) of
            [] ->
              throw({error, complete_value, <<"No sub selection provided for `", Name/binary ,"`">>});
            SubSelectionSet ->
              execute_selection_set(#{selections => SubSelectionSet}, FieldType, Result, VariablesValues, Context)
          end
      end;
    #{kind := 'LIST', ofType := InnerTypeFun} ->
      case is_list(Result) of
        false when Result =:= null -> null;
        false when Result =/= null ->
          print("Actual result: ~p", [Result]),
          throw({error, result_validation, <<"Non list result for list field type">>});
        true ->
          lists:map(fun(ResultItem) ->
            completeValue(InnerTypeFun, Fields, ResultItem, VariablesValues, Context)
          end, Result)
      end;
    #{kind := 'NON_NULL', ofType := InnerTypeFun} ->
      case completeValue(InnerTypeFun, Fields, Result, VariablesValues, Context) of
        % FIXME: make error message more readable
        null -> throw({error, complete_value, <<"Non null type cannot be null">>});
        CompletedValue -> CompletedValue
      end;

    _ ->
      print("Provided type: ~p", [FieldType]),
      throw({error, complete_value, <<"Cannot complete value for provided type">>})

  end.

mergeSelectionSet(Fields)->
  lists:foldl(fun(Field, SelectionSet) ->
    FieldSelectionSet = maps:get(selectionSet, Field, null),
    case FieldSelectionSet of
      null -> SelectionSet;
      #{selections := Selections} -> SelectionSet ++ Selections
    end
  end, [], Fields).

