-module(graphql_introspection_types).
-include("types.hrl").

%% API
-export([
  get_schema_field/0
]).

get_schema_field()->
  ?FIELD(fun schema/0, "Introspection entrypoint", fun(_,_,#{'__schema' := Schema}) -> Schema end).

schema() -> ?OBJECT('__Schema', "Schema Introspection", #{
  "queryType" => ?FIELD(fun type/0, "Query type", fun(Schema, Args, Context)->
    type_resolve(#{type => maps:get(query, Schema, null)}, Args, Context)
  end),
  "mutationType" => ?FIELD(fun type/0, "Mutation type", fun(Schema, Args, Context)->
    type_resolve(#{type => maps:get(mutation, Schema, null)}, Args, Context)
  end),
  "subscriptionType" => ?FIELD(fun type/0, "Subscription type", fun(Schema, Args, Context)->
    type_resolve(#{type => maps:get(subscription, Schema, null)}, Args, Context)
  end),
  "types" => ?FIELD(?LIST(fun type/0), "List of all availiable types", fun(_, _, #{'__types' := Types})->
    maps:fold(fun(Name, Type, Acc) ->
      [Type#{name => Name}|Acc]
    end, [], Types)
  end),

  "directives" => ?FIELD(?LIST(fun directive/0), "List of supported directives", fun()-> [] end)
}).

type() -> ?OBJECT('__Type', "Type Introspection", #{
  "kind" => ?FIELD(?STRING, null, fun(#{kind := Kind})-> Kind end),

  "ofType" => ?FIELD(fun type/0, null, fun
    (#{ofType := Type}, _, #{'__types' := Types})
        when is_atom(Type) andalso
             Type =/= null
        -> maps:get(Type, Types);

    (#{ofType := Type}, _, _) when is_map(Type) -> Type;
    (_,_,_) -> null
  end),

  "name" => ?FIELD(?STRING, null, fun(#{name := Name})-> Name end),
  "description" => ?FIELD(?STRING, null, fun(#{description := V}) -> V end),

  "fields" => ?FIELD(?LIST(fun field/0), null, #{
    "includeDeprecated" => ?ARG(?BOOLEAN)
  }, fun(Object, #{<<"includeDeprecated">> := IncludeDeprecated})->
    maps:fold(fun(Name, Field, Acc) ->
      case {Name, maps:get(deprecated, Field, false), IncludeDeprecated} of
        {<<"__", _/binary>>, _, _} -> Acc;
        {_, false, _} -> [Field#{name => Name}|Acc];
        {_, true, true} -> [Field#{name => Name}|Acc];
        _ -> Acc
      end
    end, [], maps:get(fields, Object, #{}))
  end),

  % FIXME: implement when inputTypes gonna be
  "inputFields" => ?FIELD(?LIST(?INT), null, fun()->  null end),
  "enumValues" => ?FIELD(?LIST(fun enumValue/0), null, fun
    (#{kind := 'ENUM', enumValues := EnumValues}) -> EnumValues;
    (_) -> null
  end),

  % FIXME: implement when interfaces gonna be
  "possibleTypes" => ?FIELD(?LIST(fun type/0), null, fun
    (#{kind := 'UNION', possibleTypes := PossibleTypes}) ->
      [graphql_type:unwrap_type(X) || X <- PossibleTypes];
    (_) -> null
  end),

  % FIXME: implement when interfaces gonna be
  "interfaces" => ?FIELD(?LIST(?INT), null, fun()-> [] end)
}).

% not full object
directive()-> ?OBJECT('__Directive', "Directive Introspection", #{
  "name" => ?FIELD(?STRING)
}).

field() -> graphql:objectType('__Field', "Field Introspection", #{
  "name" => ?FIELD(?STRING, null, fun(Field)-> maps:get(name, Field) end),
  "description" => ?FIELD(?STRING, null, fun(Field) -> maps:get(description, Field, null) end),

  "args" => ?FIELD(?LIST(fun inputValue/0), null, fun(Field) ->
    case maps:get(args, Field, undefined) of
      undefined -> [];
      Args -> maps:fold(fun(Name, Arg, Acc)->
        [Arg#{name => Name}|Acc]
      end, [], Args)
    end
  end),

  "type" => ?FIELD(fun type/0, null, fun type_resolve/3),
  "isDeprecated" => ?FIELD(?BOOLEAN, null, fun(#{isDeprecated := V}) -> V end),
  "deprecationReason" => ?FIELD(?STRING, null, fun(Field) -> maps:get(deprecationReason, Field, null) end)
}).

inputValue() -> ?OBJECT('__InputValue', "InputValue Introspection", #{
  "name" => ?FIELD(?STRING, null, fun(IV) -> maps:get(name, IV) end),
  "description" => ?FIELD(?STRING, null, fun(IV) -> maps:get(description, IV, null) end),
  "type" => ?FIELD(fun type/0, null, fun type_resolve/3),

  "defaultValue" => ?FIELD(?STRING, "A GraphQL-formatted string representing the default value for this input value.",
    fun(IV) ->
      case maps:get(default, IV, null) of
        null -> null;
        Value -> jsx:encode(Value)
      end
    end
  )
}).

enumValue() -> ?OBJECT('__EnumValue', "Enumerate value", #{
  "name" => ?FIELD(?STRING, null, fun(EV) -> maps:get(name, EV, null) end),
  "description" => ?FIELD(?STRING, null, fun(EV) -> maps:get(description, EV, null) end),
  "isDeprecated" => ?FIELD(?BOOLEAN, null, fun(EV) -> maps:get(isDeprecated, EV, null) end),
  "deprecationReason" => ?FIELD(?STRING, null, fun(EV) -> maps:get(deprecationReason, EV, null) end)
}).


%% helpers

type_resolve(null, _,_)-> null;
type_resolve(#{type := Type}, _, #{'__types' := Types}) when is_atom(Type) ->
  maps:get(Type, Types, null);
type_resolve(#{type := Type}, _, _) ->
  Type.
