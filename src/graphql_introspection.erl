-module(graphql_introspection).
-include("types.hrl").

%% API
-export([
  inject/1
]).

inject(QueryRoot)->
  Fields = maps:get(fields, QueryRoot),

  QueryRoot#{
    fields => Fields#{
      <<"__schema">> => #{
        type => fun schema/0,
        resolver => fun(_,_, #{'__schema' := Schema}) -> Schema end
      }
    }
  }.

schema() -> graphql:objectType(<<"__Schema">>, <<"Schema Introspection">>, #{
  <<"queryType">> => #{
    type => fun type/0,
    resolver => fun(Schema) -> maps:get(query, Schema, null) end
  },
  <<"mutationType">> => #{
    type => fun type/0,
    resolver => fun(Schema) -> maps:get(mutation, Schema, null) end
  },
  <<"subscriptionType">> => #{
    type => fun type/0,
    resolver => fun(Schema) -> maps:get(subscription, Schema, null) end
  },
  <<"types">> => #{
    type => ?LIST(fun type/0),
    resolver => fun(Schema) -> collect_types(Schema) end
  },
  <<"directives">> => #{ % FIXME when directives implemented
    type => ?LIST(fun directive/0),
    resolver => fun() -> [] end
  }
}).

type() -> graphql:objectType(<<"__Type">>, <<"Type Introspection">>, #{
  <<"kind">> => #{
    type => ?STRING,
    resolver => fun(#{kind := Kind}) -> Kind end
  },
  <<"ofType">> => #{
    type => fun type/0,
    resolver => fun(Object) ->
      case maps:get(ofType, Object, null) of
        null -> null;
        Type -> graphql_type:unwrap_type(Type)
      end
    end
  },
  <<"name">> => #{
    type => ?STRING,
    resolver => fun(Object) -> maps:get(name, Object, null) end
  },
  <<"description">> => #{
    type => ?STRING,
    resolver => fun(Object) -> maps:get(description, Object, null) end
  },
  <<"fields">> => #{
    type => ?LIST(fun field/0),
    args => #{
      <<"includeDeprecated">> => #{type => ?BOOLEAN, default => false}
    },
    resolver => fun(Object, #{<<"includeDeprecated">> := IncludeDeprecated}) ->
      maps:fold(fun(Name, Field, Acc) ->
        case {maps:get(deprecated, Field, false), IncludeDeprecated} of
          {false, _} -> [Field#{name => Name}|Acc];
          {true, true} -> [Field#{name => Name}|Acc];
          {true, false} -> Acc
        end
      end, [], maps:get(fields, Object, #{}))
    end
  },
  % FIXME: implement when inputTypes gonna be
  <<"inputFields">> => #{
    type => ?LIST(?INT),
    resolver => fun() -> null end
  },
  % FIXME: implement when interfaces gonna be
  <<"interfaces">> => #{
    type => ?LIST(?INT),
    resolver => fun() -> [] end
  },

  <<"enumValues">> => #{
    type => ?LIST(fun enumValue/0),
    resolver => fun
      (#{kind := 'ENUM', enumValues := EnumValues}) -> EnumValues;
      (_) -> null
    end
  },
  % FIXME: implement when interfaces gonna be
  <<"possibleTypes">> => #{
    type => ?LIST(fun type/0),
    resolver => fun
      (#{kind := 'UNION', possibleTypes := PossibleTypes}) ->
        [graphql_type:unwrap_type(X) || X <- PossibleTypes];
      (_) -> null
    end
  }
}).

directive() -> graphql:objectType(<<"__Directive">>, <<"Directive Introspection">>, #{
  <<"name">> => #{type => ?STRING}
}).

field() -> graphql:objectType(<<"__Field">>, <<"Field Introspection">>, #{
  <<"name">> => #{type => ?STRING, resolver => fun(Field) ->maps:get(name, Field) end},
  <<"description">> => #{type => ?STRING, resolver => fun(Field) -> maps:get(description, Field, null) end},
  <<"args">> => #{
    type => ?LIST(fun inputValue/0),
    resolver => fun(Field) ->
      case maps:get(args, Field, undefined) of
        undefined -> [];
        Args -> maps:fold(fun(Name, Arg, Acc)->
          [Arg#{name => Name}|Acc]
        end, [], Args)
      end
    end
  },
  <<"type">> => #{type => fun type/0, resolver => fun(Field)-> graphql_type:unwrap_type(maps:get(type, Field)) end},
  <<"isDeprecated">> => #{type => ?STRING, resolver => fun(Field) -> maps:get(deprecated, Field, false) end},
  <<"deprecationReason">> => #{type => ?STRING, resolver => fun(Field) -> maps:get(deprecationReason, Field, null) end}
}).

inputValue() -> graphql:objectType(<<"__InputValue">>, <<"InputValue Introspection">>, #{
  <<"name">> => #{type => ?STRING, resolver => fun(IV) -> maps:get(name, IV) end},
  <<"description">> => #{type => ?STRING, resolver => fun(IV) -> maps:get(description, IV, null) end},
  <<"type">> => #{
    type => fun type/0,
    resolver => fun(#{type := Type}) -> graphql_type:unwrap_type(Type) end
  },
  % fixme: type must be equal to object type
  <<"defaultValue">> => #{type => ?INT, resolver => fun(IV) -> maps:get(defaultFIXME, IV, null) end}
}).

enumValue() -> graphql:objectType(<<"EnumValue">>, <<"Enumerate value">>, #{
  <<"name">> => #{type => ?STRING, resolver => fun(EV) -> maps:get(name, EV, null) end},
  <<"description">> => #{type => ?STRING, resolver => fun(EV) -> maps:get(description, EV, null) end},
  <<"isDeprecated">> => #{type => ?BOOLEAN, resolver => fun(EV) -> maps:get(isDeprecated, EV, null) end},
  <<"deprecationReason">> => #{type => ?STRING, resolver => fun(EV) -> maps:get(deprecationReason, EV, null) end}
}).

extract_field_types(Object, IgnoreTypes) ->
  maps:fold(fun(_, #{type := FieldType} = Field, Acc)->
    Type = graphql_type:unwrap_type(FieldType),
    ArgsTypes = case maps:get(args, Field, null) of
      null -> [];
      Args -> extract_field_types(#{fields => Args}, IgnoreTypes)
    end,

    case lists:member(maps:get(name, Type), IgnoreTypes) of
      true -> ArgsTypes ++ Acc;
      false -> ArgsTypes ++ [Type|Acc]
    end
  end, [], maps:get(fields, Object)).

collect_types(Schema) ->
  QueryType = maps:get(query, Schema),
  TypeToCollect = case maps:get(mutation, Schema, null) of
    null -> [QueryType];
    MutationType -> [QueryType, MutationType]
  end,

  {_, Types} = collect_types(TypeToCollect, [], []),

  Types.

collect_types([], VisitedTypes, Acc) -> {VisitedTypes, Acc};
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
      collect_types(TypesTail, VisitedTypes, Acc);

    % collect this edge and add to inner types for inspection
    false ->

      InnerTypes = case Edge of
        #{kind := 'OBJECT'} -> extract_field_types(Edge, [EdgeName|VisitedTypes]);
        #{kind := 'UNION', possibleTypes := PossibleTypes} ->
          [graphql_type:unwrap_type(X) || X <- PossibleTypes];
        _ -> []
      end,

      collect_types(InnerTypes ++ TypesTail, [EdgeName|VisitedTypes], [Edge|Acc])
  end.



