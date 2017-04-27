-module(graphql_execution).
-author("mrchex").

%% API
-export([
  execute/6,
  execute_operation/2
]).

%%print(Text)-> print(Text, []).
print(Text, Args) when is_list(Args) -> io:format(Text ++ "~n", Args).

% Operation name can be null
execute(Schema, Document, OperationName, VariableValues, InitialValue, Context0)->
  Context = Context0#{
    '__schema' => Schema,
    '__fragments' => collect_fragments(Document)
  },
  try executor(Schema, Document, OperationName, VariableValues, InitialValue, Context) of
    Result -> #{data => Result}
  catch
    {error, Type, Msg} ->
      print("Error in ~p! Msg: ~p", [Type, Msg]),
      #{error => Msg, type => Type};
    {field_error, Reason} ->
      #{errors => [Reason]}
  end.


executor(Schema, Document, OperationName, VariableValues, InitialValue, Context0)->
  Operation = get_operation(Document, OperationName),
  Context = Context0#{
    '__variableValues' => coerceVariableValues(Schema, Operation, VariableValues),
    '__operation' => maps:get(operation, Operation),
    '__query' => Operation
  },

  execute_operation(InitialValue, Context).

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
coerceVariableValues(_, Operation, VariableValues)->
  VariableDefinitions = maps:get(variableDefinitions, Operation, []),
  lists:foldl(fun(VariableDefinition, Acc) ->

    #{
      kind := 'Variable',
      name := #{kind := 'Name', value := VarName}
    } = maps:get(variable, VariableDefinition),

    Value = maps:get(VarName, VariableValues, undefined),
    DefaultValue = maps:get(defaultValue, VariableDefinition, undefined),

    CoercedValue = case {Value, DefaultValue} of
      {undefined, undefined} ->
        throw({error, variable, <<"Value for variable: ", VarName/binary, " was not provided">>});
      {undefined, _} -> DefaultValue;
      {_, _} ->
        case maps:get(type, VariableDefinition) of

          #{ kind := 'NamedType', name := #{ value := VarNamedType } } ->
            #{kind => <<VarNamedType/binary, "Value">>,
              value => Value};

          #{ kind := 'ListType' } when Value =:= null -> null;

          #{ kind := 'ListType', type := #{name := #{ value := VarNamedType } } } ->
            #{kind => 'ListValue',
              values => [#{ kind => <<VarNamedType/binary, "Value">>, value => V} || V <- Value]};

          #{ kind := 'NonNullType', type := #{ kind := 'ListType', type := #{name := #{ value := VarNamedType } } } } ->
            #{kind => 'NonNullValue',
              value => #{
                kind => 'ListValue',
                values => [#{ kind => <<VarNamedType/binary, "Value">>, value => V} || V <- Value]
              }};

          #{ kind := 'NonNullType', type := #{name := #{ value := VarNamedType } } }->
            #{kind => 'NonNullValue',
              value => #{ kind => <<VarNamedType/binary, "Value">>, value => Value}}

        end
    end,

    Acc#{ VarName => CoercedValue }

  end, #{}, VariableDefinitions).

% TODO: complete me http://facebook.github.io/graphql/#CoerceArgumentValues()
coerceArgumentValues(ObjectType, Field, Context) ->
  FieldName = get_field_name(Field),
  ArgumentDefinitions = graphql_type_object:get_args(FieldName, ObjectType),
  VariableValues = maps:get('__variableValues', Context),

  maps:fold(fun(ArgumentName, ArgumentDefinition, CoercedValues) ->
    ArgumentType = get_type(ArgumentDefinition, Context),

    % 5 of http://facebook.github.io/graphql/#sec-Coercing-Field-Arguments
    CoercedValue =  case get_field_argument_by_name(ArgumentName, Field) of

      #{  % h.Let coercedValue be the result of coercing value
        kind := 'Argument',
        name := #{kind := 'Name', value := ArgumentName},
        value := Value
      } ->
        case Value of
          #{kind := 'Variable', name := #{ value := VariableName }} ->
            ParseValue = maps:get(parse_value, ArgumentType),
            ParseValue(maps:get(VariableName, VariableValues, null), ArgumentType);
          _ ->
            ParseLiteral = maps:get(parse_literal, ArgumentType),
            ParseLiteral(Value, ArgumentType)
        end;


      % f. Otherwise, if value does not exist (was not provided in argumentValues:
      % f.i. If defaultValue exists (including null):
      % f.i.1. Add an entry to coercedValues named argName with the value defaultValue.
      undefined ->
        case maps:get(default, ArgumentDefinition, undefined) of
          undefined ->
            ParseLiteral = maps:get(parse_literal, ArgumentType),
            ParseLiteral(null, ArgumentType);
          DefaultValue -> DefaultValue
        end

      % f.iii - Otherwise, continue to the next argument definition.
    end,

    CoercedValues#{ ArgumentName => CoercedValue}
  end, #{}, ArgumentDefinitions).


% http://facebook.github.io/graphql/#sec-Executing-Operations
execute_operation(InitialValue, #{'__operation' := Operation, '__schema' := Schema, '__query' := Query} = Context) ->
  QueryType = maps:get(Operation, Schema),
  SelectionSet = maps:get(selectionSet, Query),

  Parallel = false,  % FIXME: enable parallel when we can

  execute_selection_set(SelectionSet, QueryType, InitialValue, Context, Parallel).

% http://facebook.github.io/graphql/#sec-Executing-Selection-Sets
execute_selection_set(SelectionSet, ObjectType, ObjectValue, Context)->
  execute_selection_set(SelectionSet, ObjectType, ObjectValue, Context, false).

execute_selection_set(SelectionSet, ObjectTypeName, ObjectValue, #{'__types' := Types} = Context, Parallel) when is_atom(ObjectTypeName) ->
  ObjectType = maps:get(ObjectTypeName, Types),
  execute_selection_set(SelectionSet, ObjectType, ObjectValue, Context, Parallel);
execute_selection_set(SelectionSet, ObjectType, ObjectValue, Context, Parallel)->
  Fragments = maps:get('__fragments', Context),
  VariableValues = maps:get('__variableValues', Context),

  GroupedFieldSet = collect_fields(ObjectType, SelectionSet, VariableValues, Fragments),

  MapFun = fun({ResponseKey, Fields})->
    % 6.3 - 3.a. Let fieldName be the name of the first entry in fields.
    #{value := FieldName} = maps:get(name, lists:nth(1, Fields)),
    Field = case graphql_type_object:get_field(FieldName, ObjectType) of
      undefined ->
        % Fixme #63
        ObjectTypeName = case maps:get(name, ObjectType) of
          ObjectTypeName0 when is_binary(ObjectTypeName0) -> ObjectTypeName0;
          ObjectTypeName0 when is_atom(ObjectTypeName0) -> atom_to_binary(ObjectTypeName0, utf8)
        end,

        ErrorMsg = <<
          "Field `", FieldName/binary,
          "` does not exist in ObjectType `",
          ObjectTypeName/binary, "`"
        >>,
        throw({error, validation_error, ErrorMsg});
      Field0 -> Field0
    end,

    % TODO: Must be implemented when we learn why its needed and what the point of use case
    % TODO: c.If fieldType is null:
    % TODO:    i.Continue to the next iteration of groupedFieldSet.

    FieldType = get_type(Field, Context),

    ResponseValue = executeField(ObjectType, ObjectValue, Fields, FieldType, Context),
    {ResponseKey, ResponseValue}

  end,

  case Parallel of
    true -> graphql:upmap(MapFun, GroupedFieldSet, 5000);
    false -> lists:map(MapFun, GroupedFieldSet)
  end.

get_type(#{type := #{ofType := Type} = Wrapper}, Types) ->
  case Type of
    null -> Wrapper;
    _ -> Wrapper#{
      ofType => get_type(#{type => Type}, Types)
    }
  end;
get_type(#{type := TypeName}, #{'__types' := Types}) when is_atom(TypeName) ->
  maps:get(TypeName, Types);
get_type(#{type := TypeRef}, _) when is_function(TypeRef) ->
  graphql_type:unwrap_type(TypeRef);
get_type(#{type := Type}, _) when is_map(Type) ->
  Type.

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
  lists:reverse(collect_fields(ObjectType, SelectionSet, VariableValues, Fragments, [])).
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
        end;

      % 3.e
      #{kind := 'InlineFragment'} = Fragment ->
        #{
          typeCondition := #{
            kind := 'NamedType',
            name :=  #{ kind := 'Name', value := FragmentType }
          },
          selectionSet := FragmentSelectionSet
        } = Fragment,
        case does_fragment_type_apply(ObjectType, FragmentType) of
          false -> {GroupedFields, VisitedFragments};  % 3.e.ii
          true ->
            FragmentGroupedField = collect_fields(ObjectType, FragmentSelectionSet, VariableValues, Fragments, VisitedFragments),
            {GroupedFields ++ FragmentGroupedField, VisitedFragments}
        end

    end
  end, {[], VisitedFragments0}, Selections),
  CollectedFields.

does_fragment_type_apply(#{name := ObjectTypeName}, FragmentTypeName) when is_atom(ObjectTypeName) ->
  ObjectTypeNameAtom = atom_to_binary(ObjectTypeName, utf8),
  ObjectTypeNameAtom =:= FragmentTypeName;
does_fragment_type_apply(#{name := Name}, Name) -> true;
does_fragment_type_apply(_, _) -> false.

get_response_key_from_selection(#{alias := #{value := Key}}) -> Key;
get_response_key_from_selection(#{name := #{value := Key}}) -> Key.

executeField(ObjectType, ObjectValue, [Field|_]=Fields, FieldType, Context)->

  ArgumentValues = coerceArgumentValues(ObjectType, Field, Context),
  FieldName = get_field_name(Field),

  case resolveFieldValue(ObjectType, ObjectValue, FieldName, ArgumentValues, Context) of
    {overwrite_context, ResolvedValue, OverwrittenContext} ->
      completeValue(FieldType, Fields, ResolvedValue, OverwrittenContext);
    {ok, ResolvedValue} ->
      completeValue(FieldType, Fields, ResolvedValue, Context);

    {error, Reason} when is_list(Reason) -> throw({field_error, #{message => list_to_binary(Reason)}});  % {error, "msg"}
    {error, Reason} when is_binary(Reason) -> throw({field_error, #{message => Reason}});  % {error, <<"msg">>}
    {error, Reason} when is_map(Reason) -> throw({field_error, Reason});  % {error, #{message => <<"msg">>}}

    ResolvedValue ->
      completeValue(FieldType, Fields, ResolvedValue, Context)
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
  Resolver = graphql_type_object:get_field_resolver(FieldName, ObjectType),
  case erlang:fun_info(Resolver, arity) of
    {arity, 0} -> Resolver();
    {arity, 1} -> Resolver(ObjectValue);
    {arity, 2} -> Resolver(ObjectValue, ArgumentValues);
    {arity, 3} -> Resolver(ObjectValue, ArgumentValues, Context)
  end.

% TODO: complete me http: //facebook.github.io/graphql/#CompleteValue()
completeValue(FieldTypeName, Fields, Result, #{'__types' := Types} = Context) when is_atom(FieldTypeName) ->
  FieldType = maps:get(FieldTypeName, Types),
  completeValue(FieldType, Fields, Result, Context);
completeValue(FieldTypeFun, Fields, Result, Context) when is_function(FieldTypeFun) ->
  FieldType = graphql_type:unwrap_type(FieldTypeFun),
  completeValue(FieldType, Fields, Result, Context);
completeValue(FieldType, Fields, Result, Context) ->

  % TODO: may be need move to some function in each type (serialize, for example)
  case FieldType of
    #{kind := Kind, serialize := Serialize} when
      Kind =:= 'SCALAR' orelse
        Kind =:= 'ENUM' ->
      Serialize(Result, FieldType, Context);

    #{kind := Kind, name := Name} when
      Kind =:= 'OBJECT' orelse
      Kind =:= 'UNION' ->
        case Result of
          null -> null;
          _ ->
            { AbstractFieldType, Result1 } = case Kind of
              'OBJECT' -> {FieldType, Result};
              'UNION' ->
                ResolveType = maps:get(resolve_type, FieldType),
                { ResolvedType, UnwrappedResult }  = ResolveType(Result, FieldType),
                {graphql_type:unwrap_type(ResolvedType), UnwrappedResult}
            end,

            case mergeSelectionSet(Fields) of
              [] ->

                % Fixme #63
                ObjectTypeName = case is_atom(Name) of
                  false -> Name;
                  true -> atom_to_binary(Name, utf8)
                end,

                throw({error, complete_value, <<"No sub selection provided for `", ObjectTypeName/binary ,"`">>});
              SubSelectionSet ->
                execute_selection_set(#{selections => SubSelectionSet}, AbstractFieldType, Result1, Context)
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
            completeValue(InnerTypeFun, Fields, ResultItem, Context)
          end, Result)
      end;

    #{kind := 'NON_NULL', ofType := InnerTypeFun} ->
      case completeValue(InnerTypeFun, Fields, Result, Context) of
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
