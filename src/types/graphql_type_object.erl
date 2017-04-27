-module(graphql_type_object).
-include("types.hrl").

%% API
-export([
  type/2, type/3,

  field/1, field/2, field/3, field/4,
  arg/1, arg/2, arg/3,

  get_field/2,
  get_args/2,
  get_type_from_definition/1,
  get_field_resolver/2
]).

-type field() :: map().
-type fields() :: #{
    string() | binary() => field()
  } | null.

-type arg() :: map().
-type args() :: #{ binary() | string() => arg() }.


-type type() :: graphql_type:type() | {graphql_type:type(), graphql_type:optional_string() | null}.
-type name() :: graphql_type:optional_string().
-type description() :: graphql_type:optional_string() | null.
-type resolver() :: function() | null.

-spec type(name(), fields())-> graphql_type:type().
-spec type(name(), description(), fields())-> graphql_type:type().
type(Name, Fields) -> type(Name, null, Fields).
type(Name0, Description, Fields)->
  % Fixme #63 https://github.com/graphql-erlang/graphql/issues/63
  Name = case is_atom(Name0) of
    true -> Name0;
    false -> graphql_type:optional_string(Name0)
  end,

  #{
    kind => 'OBJECT',
    name => Name,
    ofType => null,
    description => graphql_type:optional_string(Description),
    fields => maps:fold(fun(FieldName, Field, Acc) ->

      Key = case is_binary(FieldName) of
        true -> FieldName;
        false -> list_to_binary(FieldName)
      end,

      Acc#{ Key => Field#{
        resolver => coerce_field_resolver(Key, Field)
      }}
    end, #{ <<"__typename">> => field(?STRING, "Name of current type", fun() -> Name end) }, Fields)
  }.

-spec field(Type::type()) -> field().
field(Type) -> field(Type, null).

-spec field(Type::type(), Description::description()) -> field().
field(Type, Description) -> field(Type, Description, null).

-spec field(Type::type(), Description::description(), Resolver::resolver()) -> field().
field(Type, Description, Resolver) -> field(Type, Description, #{}, Resolver).

-spec field(Type::type(), Description::description(), Args::args(), Resolver::resolver()) -> field().
field(Type, Description, Args, Resolver) ->

  % like elm notation ^__^
  #{ kind => 'FIELD'
   , type => Type
   , description => graphql_type:optional_string(Description)
   , args => maps:fold(fun(ArgName, Arg, Acc) ->
        Key = case is_binary(ArgName) of
          true -> ArgName;
          false -> list_to_binary(ArgName)
        end,
        Acc#{ Key => Arg#{ name => Key } }
      end, #{}, Args)
   , resolver => Resolver
   , isDeprecated => false
   , deprecationReason => null
  }.

-spec arg(Type::type()) -> arg().
arg(Type) -> arg(Type, null).

-spec arg(Type::type(), Description::description()) -> arg().
arg(Type, Description) -> arg(Type, null, Description).

-spec arg(Type::type(), DefaultValue::any(), Description::description()) -> arg().
arg(Type, DefaultValue, Description) ->
  #{ kind => 'INPUT_VALUE'
   , type => Type
   , description => graphql_type:optional_string(Description)
   , default => DefaultValue
  }.


get_field(FieldName, ObjectType)->
  Fields = maps:get(fields, ObjectType),
  maps:get(FieldName, Fields, undefined).

get_args(FieldName, ObjectType)->
  FieldDefinition = get_field(FieldName, ObjectType),
  maps:get(args, FieldDefinition, #{}).


get_field_resolver(FieldName, ObjectType)->
  maps:get(resolver, get_field(FieldName, ObjectType)).

coerce_field_resolver(_, #{resolver := Resolver})
  when is_function(Resolver)->
  Resolver;
coerce_field_resolver(FieldName, Field) ->
  case maps:get(resolver, Field, null) of
    null -> fun(ObjectValue) -> default_resolver(FieldName, ObjectValue) end;
    Resolver -> Resolver
  end.

% if this field is scalar type -
default_resolver(FieldName, ObjectValue)->
  case is_map(ObjectValue) of
    true -> maps:get(FieldName, ObjectValue, null);
    false -> proplists:get_value(FieldName, ObjectValue, null)
  end.

get_type_from_definition(Definition) ->
  graphql_type:unwrap_type(maps:get(type, Definition)).
