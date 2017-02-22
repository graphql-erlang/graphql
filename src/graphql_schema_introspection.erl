%%%-------------------------------------------------------------------
%%% @author mrchex
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Feb 2017 6:35 PM
%%%-------------------------------------------------------------------
-module(graphql_schema_introspection).
-author("mrchex").

%% API
-export([
  inject/1
]).

inject(QueryRoot)->
  Fields = maps:get(fields, QueryRoot),

  QueryRoot#{
    fields => Fields#{
      <<"__schema">> => #{
        type => {object, fun schema/0},
        resolver => fun(_,_, #{'__schema' := Schema}) -> Schema end
      }
    }
  }.

schema() -> graphql:objectType(<<"__Schema">>, <<"Schema Introspection">>, #{
  <<"queryType">> => #{
    type => {object, fun type/0},
    resolver => fun(Schema) -> maps:get(query, Schema, null) end
  },
  <<"mutationType">> => #{
    type => {object, fun type/0},
    resolver => fun(Schema) -> maps:get(mutation, Schema, null) end
  },
  <<"subscriptionType">> => #{
    type => {object, fun type/0},
    resolver => fun(Schema) -> maps:get(subscription, Schema, null) end
  },
  <<"types">> => #{
    type => [{object, fun type/0}],
    resolver => fun(Schema) -> collect_types(Schema) end
  },
  <<"directives">> => #{ % FIXME when directives implemented
    type => [{object, fun directive/0}],
    resolver => fun() -> [] end
  }
}).

type() -> graphql:objectType(<<"__Type">>, <<"Type Introspection">>, #{
  <<"kind">> => #{
    type => string,
    resolver => fun(#{kind := Kind}) -> Kind end
  },
  <<"ofType">> => #{
    type => {object, fun type/0},
    resolver => fun(Object) -> maps:get(ofType, Object, null) end
  },
  <<"name">> => #{
    type => string,
    resolver => fun(Object) -> maps:get(name, Object, null) end
  },
  <<"description">> => #{
    type => string,
    resolver => fun(Object) -> maps:get(description, Object, null) end
  },
  <<"fields">> => #{
    type => [{object, fun field/0}],
    args => #{
      <<"includeDeprecated">> => #{type => boolean, default => false}
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
    type => [integer],
    resolver => fun() -> null end
  },
  % FIXME: implement when interfaces gonna be
  <<"interfaces">> => #{
    type => integer,
    resolver => fun() -> [] end
  },
  % FIXME: implement when enum can be by object
  <<"enumValues">> => #{
    type => [integer],
    resolver => fun() -> [] end
  },
  % FIXME: implement when interfaces gonna be
  <<"possibleTypes">> => #{
    type => integer,
    resolver => fun() -> [] end
  }
}).

directive() -> graphql:objectType(<<"__Directive">>, <<"Directive Introspection">>, #{
  <<"name">> => #{type => string}
}).

field() -> graphql:objectType(<<"__Field">>, <<"Field Introspection">>, #{
  <<"name">> => #{type => string, resolver => fun(Field) -> maps:get(name, Field) end},
  <<"description">> => #{type => string, resolver => fun(Field) -> maps:get(description, Field, null) end},
  <<"args">> => #{
    type => [{object, fun inputValue/0}],
    resolver => fun(Field) ->
      case maps:get(args, Field, undefined) of
        undefined -> [];
        Args -> maps:fold(fun(Name, Arg, Acc)->
          [Arg#{name => Name}|Acc]
        end, [], Args)
      end
    end
  },
  <<"type">> => #{type => {object, fun type/0}, resolver => fun(Field)-> typeRef_to_type(maps:get(type, Field)) end},
  <<"isDeprecated">> => #{type => string, resolver => fun(Field) -> maps:get(deprecated, Field, false) end},
  <<"deprecationReason">> => #{type => string, resolver => fun(Field) -> maps:get(deprecationReason, Field, null) end}
}).

inputValue() -> graphql:objectType(<<"__InputValue">>, <<"InputValue Introspection">>, #{
  <<"name">> => #{type => string, resolver => fun(IV) -> maps:get(name, IV) end},
  <<"description">> => #{type => string, resolver => fun(IV) -> maps:get(description, IV, null) end},
  <<"type">> => #{
    type => {object, fun type/0},
    resolver => fun(#{type := Type}) -> typeRef_to_type(Type) end
  },
  % fixme: type must be equal to object type
  <<"defaultValue">> => #{type => integer, resolver => fun(IV) -> maps:get(defaultFIXME, IV, null) end}
}).


typeRef_to_type(integer)-> #{kind => <<"SCALAR">>, type => integer, name => <<"Int">>};
typeRef_to_type(float)-> #{kind => <<"SCALAR">>, type => float, name => <<"Float">>};
typeRef_to_type(string)-> #{kind => <<"SCALAR">>, type => string, name => <<"String">>};
typeRef_to_type(boolean)-> #{kind => <<"SCALAR">>, type => boolean, name => <<"Boolean">>};
typeRef_to_type({object, ObjectType})-> ObjectType();
typeRef_to_type([Type])-> #{kind => <<"LIST">>, type => [Type], ofType => typeRef_to_type(Type)}.

extract_edge_type(#{type := [Type]}) -> typeRef_to_type(Type);
extract_edge_type(#{type := Type}) -> typeRef_to_type(Type).

extract_field_types(Object, IgnoreTypes) ->
  maps:fold(fun(_, Field, Acc)->
    Type = extract_edge_type(Field),
    case lists:member(maps:get(name, Type), IgnoreTypes) of
      true -> Acc;
      false -> [Type|Acc]
    end
  end, [], maps:get(fields, Object)).

collect_types(Schema) ->
  QueryType = maps:get(query, Schema),

  % TODO: add mutation and subscription when implemented
  {_, Types} = collect_types([QueryType], [], []),

  Types.

collect_types([], VisitedTypes, Acc) -> {VisitedTypes, Acc};
collect_types([Edge|TypesTail], VisitedTypes, Acc)->
  EdgeName = maps:get(name, Edge),

  % check is visited
  case lists:member(EdgeName, VisitedTypes) of
    % skip this edge
    true -> collect_types(TypesTail, VisitedTypes, Acc);

    % collect this edge and add to tail inner types when kind is OBJECT
    false ->
      InnerTypes = case Edge of
        #{kind := <<"OBJECT">>} -> extract_field_types(Edge, VisitedTypes);
        _ -> []
      end,
      collect_types(InnerTypes ++ TypesTail, [EdgeName|VisitedTypes], [Edge|Acc])
  end.



