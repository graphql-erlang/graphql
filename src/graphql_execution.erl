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
  case Operation of
    #{<<"operation">> := <<"query">>} ->
      execute_query(Operation, Schema, CoercedVariableValues, InitialValue, Context);
    #{<<"operation">> := WantedOperation} ->
      throw({error, execute, <<"Currently operation ", WantedOperation/binary, " does not support">>})
  end.

% throw validation error when operation not found or document define multiple
get_operation(Document, OperationName)->
  Definitions = maps:get(<<"definitions">>, Document, []),
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
get_operation_from_definitions([#{<<"kind">> := <<"OperationDefinition">>, <<"operation">> := OperationName} = Operation|Tail], null, _)->
  get_operation_from_definitions(Tail, OperationName, Operation);
% when we meet another operation that named like we already founded
get_operation_from_definitions([#{<<"kind">> := <<"OperationDefinition">>, <<"operation">> := OperationName}|_], OperationName, _)->
  {error, <<"Document defines multiple operations, otherwise the document is expected to only contain a single operation">>};
get_operation_from_definitions([_|Tail], OperationName, Operation)->
  get_operation_from_definitions(Tail, OperationName, Operation).


% TODO: implement me http://facebook.github.io/graphql/#CoerceVariableValues()
coerceVariableValues(Schema, Operation, VariableValues)->
  #{}.

coerce_value_type(#{<<"kind">> := <<"IntValue">>, <<"value">> := Value})->
  binary_to_integer(Value);
coerce_value_type(#{<<"kind">> := <<"StringValue">>, <<"value">> := Value})->
  Value;
coerce_value_type(#{<<"kind">> := <<"BooleanValue">>, <<"value">> := Value})
  when is_boolean(Value)-> Value.


% TODO: complete me http://facebook.github.io/graphql/#CoerceArgumentValues()
coerceArgumentValues(ObjectType, Field, VariableValues) ->
  Arguments = maps:get(<<"arguments">>, Field),
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
        <<"name">> := #{},
        <<"value">> := Value0
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
execute_query(Query, Schema, VariableValues, InitialValue, Context) ->
  QueryType = maps:get(query, Schema),
  SelectionSet = maps:get(<<"selectionSet">>, Query),
  Data = execute_selection_set(SelectionSet, QueryType, InitialValue, VariableValues, Context),
  #{
    data => Data,
    errors => []
  }.

% http://facebook.github.io/graphql/#sec-Executing-Selection-Sets
execute_selection_set(SelectionSet, ObjectType, ObjectValue, VariableValues, Context)->
  GroupedFieldSet = collect_fields(ObjectType, SelectionSet, VariableValues),
  maps:fold(fun(ResponseKey, Fields, ResultMap)->
    % 6.3 - 3.a. Let fieldName be the name of the first entry in fields.
    #{<<"value">> := FieldName} = maps:get(<<"name">>, lists:nth(1, Fields)),
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

    ResponseValue = executeField(ObjectType, ObjectValue, Fields, FieldType, VariableValues, Context),
    ResultMap#{ ResponseKey => ResponseValue }

  end, #{}, GroupedFieldSet).

% TODO: does not support directives and fragments(3.a, 3.b, 3.d, 3.e): http://facebook.github.io/graphql/#CollectFields()
collect_fields(ObjectType, SelectionSet, VariableValues) ->
  Selections = maps:get(<<"selections">>, SelectionSet),
  lists:foldl(fun(Selection, GroupedFields)->
    case Selection of
      #{<<"kind">> := <<"Field">>} -> % 3.c
        ResponseKey = get_response_key_from_selection(Selection),
        GroupForResponseKey = maps:get(ResponseKey, GroupedFields, []),

        GroupedFields#{
          ResponseKey => [Selection|GroupForResponseKey]
        }

    end
  end, #{}, Selections).

get_response_key_from_selection(#{<<"alias">> := null, <<"name">> := #{<<"value">> := Key}}) -> Key;
get_response_key_from_selection(#{<<"alias">> := #{<<"value">> := Key}}) -> Key.

executeField(ObjectType, ObjectValue, [Field|_]=Fields, FieldType, VariableValues, Context)->
  ArgumentValues = coerceArgumentValues(ObjectType, Field, VariableValues),
  FieldName = get_field_name(Field),
  case resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context) of
    {ResolvedValue, OverwritenContext} ->
      io:format("WARNING. Overwriting context in fiend: ~p~n", [FieldName]),
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, OverwritenContext);
    ResolvedValue ->
      completeValue(FieldType, Fields, ResolvedValue, VariableValues, Context)
  end.


get_field_name(#{<<"name">> := #{<<"value">> := FieldName}}) -> FieldName.

get_field_arguments(Field)->
  case maps:get(<<"arguments">>, Field) of
    null -> [];
    Args -> Args
  end.

get_field_argument_by_name(ArgumentName, Field)->
  Arguments = get_field_arguments(Field),
  case lists:takewhile(
    fun(#{<<"name">> := #{ <<"value">> := ElemName }}) -> ElemName =:= ArgumentName end, Arguments)
  of
    [Argument] ->  Argument;
    _ -> undefined
  end.

resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context)->
  Resolver = graphql_schema:get_field_resolver(FieldName, ObjectType),
  case erlang:fun_info(Resolver, arity) of
    {arity, 2} -> Resolver(ObjectValue, ArgumentValues);
    {arity, 3} -> Resolver(ObjectValue, ArgumentValues, Context)
  end.

% TODO: complete me http: //facebook.github.io/graphql/#CompleteValue()
completeValue(FieldType, Fields, Result, VariablesValues, Context)->
  case FieldType of
    [InnerType] ->
      case is_list(Result) of
        false -> throw({error, result_validation, <<"Non list result for list field type">>});
        true ->
          lists:map(fun(ResultItem) ->
            completeValue(InnerType, Fields, ResultItem, VariablesValues, Context)
          end, Result)
      end;
    {object, ObjectTypeFun} ->
      ObjectType = ObjectTypeFun(),
      SubSelectionSet = mergeSelectionSet(Fields),
      execute_selection_set(#{<<"selections">> => SubSelectionSet}, ObjectType, Result, VariablesValues, Context);

    _ -> Result
  end.

mergeSelectionSet(Fields)->
  lists:foldl(fun(Field, SelectionSet) ->
    FieldSelectionSet = maps:get(<<"selectionSet">>, Field, null),
    case FieldSelectionSet of
      null -> SelectionSet;
      #{<<"selections">> := Selections} -> SelectionSet ++ Selections
    end
  end, [], Fields).