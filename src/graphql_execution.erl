-module(graphql_execution).
-author("mrchex").

%% API
-export([
  execute/6
]).

print(Text)-> print(Text, []).
print(Text, Args) -> io:format(Text ++ "~n", Args).

% Operation name can be null
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context)->
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
  Fragments = collect_fragments(Document),

  case Operation of
    #{operation := query} ->
      execute_query(Operation, Schema, CoercedVariableValues, InitialValue, Fragments, Context);
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

coerce_value_type(#{kind := 'IntValue', value := Value}) when is_integer(Value)-> Value;
coerce_value_type(#{kind := 'IntValue', value := Value}) when is_binary(Value)->
  binary_to_integer(Value);
coerce_value_type(#{kind := 'StringValue', value := Value})->
  Value;
coerce_value_type(#{kind := 'BooleanValue', value := Value})
  when is_boolean(Value)-> Value.


% TODO: complete me http://facebook.github.io/graphql/#CoerceArgumentValues()
coerceArgumentValues(ObjectType, Field, VariableValues) ->
  Arguments = maps:get(arguments, Field, []),
  FieldName = get_field_name(Field),
  ArgumentDefinitions = graphql_schema:get_argument_definitions(FieldName, ObjectType),

%%  print("ARGUMENT DEFINITIONS: ~p", [ArgumentDefinitions]),

  maps:fold(fun(ArgumentName, ArgumentDefinition, CoercedValues) ->
    ArgumentType = graphql_schema:get_argument_type(ArgumentDefinition),
    DefaultValue = graphql_schema:get_argument_default(ArgumentDefinition),

    print("ARGUMENT COERCE: ~p", [ArgumentName]),

    % 5 of http://facebook.github.io/graphql/#sec-Coercing-Field-Arguments
    CoercedValue = case get_field_argument_by_name(ArgumentName, Field) of

      #{  % h.Let coercedValue be the result of coercing value
        name := #{},
        value := Value0
      } ->
        Value = coerce_value_type(Value0),
        case graphql_schema:check_type(ArgumentType, Value) of
          false ->
            ErrorMsg = <<
              "Field(",
              FieldName/binary,
              ") unexpected argument type. Expected (defined in schema): ",
              (atom_to_binary(ArgumentType, utf8))/binary
            >>,

            throw({error, args_validation, ErrorMsg});

          % if validation passed - let CoercedValue be NotCheckedCoercedValue - because it checked :)
          true -> Value
        end;

      % f. Otherwise, if value does not exist (was not provided in argumentValues:
      % f.i. If defaultValue exists (including null):
      % f.i.1. Add an entry to coercedValues named argName with the value defaultValue.
      undefined -> DefaultValue;
      % f.iii - Otherwise, continue to the next argument definition.
      _ -> null
    end,

    CoercedValues#{ ArgumentName => CoercedValue}
  end, #{}, ArgumentDefinitions).


% http://facebook.github.io/graphql/#sec-Executing-Operations
execute_query(Query, Schema, VariableValues, InitialValue, Fragments, Context) ->
  QueryType = maps:get(query, Schema),
  SelectionSet = maps:get(selectionSet, Query),
%%  Data = execute_selection_set(SelectionSet, QueryType, InitialValue, VariableValues, Fragments, Context),
  Parallel = false,  % FIXME: enable parallel when we can
  {T, Data} = timer:tc(fun execute_selection_set/7, [SelectionSet, QueryType, InitialValue, VariableValues, Fragments, Context, Parallel]),
  io:format("EXECUTE SELECTION SET TIMER: ~p~n", [T]),
  #{
    data => Data,
    errors => []
  }.

% http://facebook.github.io/graphql/#sec-Executing-Selection-Sets
execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Fragments, Context)->
  execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Fragments, Context, false).

execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Fragments, Context, Parallel)->
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
    FieldType = graphql_schema:get_field_type(Field),

    ResponseValue = executeField(ObjectType, ObjectValue, Fields, FieldType, VariableValues, Fragments, Context),
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

executeField(ObjectType, ObjectValue, [Field|_]=Fields, FieldType, VariableValues, Fragments, Context)->
  ArgumentValues = coerceArgumentValues(ObjectType, Field, VariableValues),
  FieldName = get_field_name(Field),
  case resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context) of
    {ResolvedValue, OverwritenContext} ->
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, Fragments, OverwritenContext);
    ResolvedValue ->
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, Fragments, Context)
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
    {arity, 2} -> Resolver(ObjectValue, ArgumentValues);
    {arity, 3} -> Resolver(ObjectValue, ArgumentValues, Context)
  end.

% TODO: complete me http: //facebook.github.io/graphql/#CompleteValue()
completeValue(FieldType, Fields, Result, VariablesValues, Fragments, Context)->
  case FieldType of
    [InnerType] ->
      case is_list(Result) of
        false -> throw({error, result_validation, <<"Non list result for list field type">>});
        true ->
          lists:map(fun(ResultItem) ->
            completeValue(InnerType, Fields, ResultItem, VariablesValues, Fragments, Context)
          end, Result)
      end;
    {object, ObjectTypeFun} ->
      case Result of
        null -> null;
        _ ->
          ObjectType = ObjectTypeFun(),
          SubSelectionSet = mergeSelectionSet(Fields),
          execute_selection_set(#{selections => SubSelectionSet}, ObjectType, Result, VariablesValues, Fragments, Context)
      end;
    _ -> Result
  end.

mergeSelectionSet(Fields)->
  lists:foldl(fun(Field, SelectionSet) ->
    FieldSelectionSet = maps:get(selectionSet, Field, null),
    case FieldSelectionSet of
      null -> SelectionSet;
      #{selections := Selections} -> SelectionSet ++ Selections
    end
  end, [], Fields).