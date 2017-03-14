# Erlang GraphQL [![Build Status](https://travis-ci.org/graphql-erlang/graphql.svg?branch=master)](https://travis-ci.org/graphql-erlang/graphql)

An Erlang implementation of Facebook's GraphQL.

This is the core GraphQL query parsing and execution engine which goal is to be transport, server and datastore agnostic.


## Status

This is a work in progress, here's todo list:

- [X] Parser for GraphQL
- [X] Schema definition
- [X] Query execution
  - [X] Pre-defined basic scalar types
  - [X] Complex types (List, Object, Union, etc)
  - [X] Arguments
  - [X] Variables
  - [X] Fragments and inline fragments in queries
  - [X] Custom types
  - [ ] Parallel execution
  - [ ] Return maps option
- [X] Mutations
- [ ] Subscriptions
- [X] Introspection
- [ ] Directives
- [ ] "Compile" and validate schema before start the app
- [ ] GraphQL Guard (may be another app)
  - [ ] Calculate query/mutation complexity
  - [ ] White list of queries with big complexity

## Installation

Rebar3 - hex package
```erlang
    {deps, [
      {graphql, "0.2.7", {pkg, graphql_erlang}}
    ]}.
```

Rebar - git with version tag
```erlang
{deps, [
    {graphql, "", {git, "https://github.com/graphql-erlang/graphql.git", {tag, "v0.2.7"}}}
]]
```

## Get started

First - define you schema:
```erlang
-module(graphql_example).
-export([schema/0]).
-include_lib("graphql/include/types.hrl").

schema() -> graphlq:schema(#{
  query => query()
}).

query() -> graphql:objectType(<<"QueryRoot">>, <<"Example Query">>, #{
  <<"greatings">> => #{
    type => ?STRING,
    description => <<"Greating">>,
    args => #{
      <<"name">> => #{
        type => ?NON_NULL(?STRING),
	description => <<"The name of who you'd like to great">>
      }
    },
    resolver => fun(_ParrentObject, Args, _Context) ->
      maps:get(<<"name">>, Args)
    end
  }
}).
```

In handler - call query for schema with optional options:
```erlang
Document = <<"query($name: String!) { greatings(name: $name) }">>,  % or list
Context = #{},  % you can pass request through context or whatever you want use in resolver acress the schema
InitValue = #{},  % root value, passed to query/mutation root resolver
VariableValues = #{  % variables passed with query
  <<"name">> => <<"world">>
},

Result = graphql:execute(graphql_example:schema(), Document, InitValue, Context),

io:format("Result: ~p~n", [Result]).  % #{data => [{<<"name">>, <<"world">>}]}
```

## Types include

We provide macros for simple types definition. Include: `-include_lib("graphql/include/types.hrl").`

All values for types can be `null` except `?NON_NULL` and ObjectType

`?BOOLEAN` - boolean type. Represent atoms true/false.

`?ENUM(Name, Description, EnumValues)` - enumaration. Options is optional Usage:
```erlang
my_enum() -> ?ENUM(<<"MyEnum">>, <<"MyEnumDescription">>, [
  % ?ENUM_VAL(InternalValue, EnumValue, Description, Options) % Options - optional 
  ?ENUM_VAL(1, <<"ONE">>, <<"REPRESENT 1">>),
  ?ENUM_VAL(2, <<"TWO">>, <<"Deprecated exempla">>, #{ isDeprecated => true, deprecationReason => <<"Becouse we can">>})
]).
```

`?FLOAT` and `?INT` - represent float and integer. 

`?LIST(OfType)` - represent list where `OfType` - inner list type. Example: `?LIST(?INT)` - `[Int]`

`?NON_NULL(OfType)` - non nullable type - wrapper over nullable type. Example: `?NON_NULL(?LIST(?INT))` - `[Int]!`

`?STRING` - binary string

`?UNION(Name, Description, PossibleTypes, ResolveTypeFun)` - type can be one of the listed in PossibleTypes. PossibleTypes must be list of ObjectTypes.

Example:

Type definitions:
```erlang
one() -> graphql:objectType(<<"One">>, <<"Has field named one">>, #{
  <<"one">> => #{type => ?INT}
}).

two() -> graphql:objectType(<<"Two">>, <<"Has field named two">>, #{
  <<"two">> => #{type => ?INT}
}).

union_default() -> ?UNION(<<"UnionDefaultResolve">>, <<"Used default type resolver">>, [
  fun one/0,
  fun two/0
]).

union_custom() -> ?UNION(<<"UnionCastomResolve">>, <<"Used custom type resolver">>, [
  fun one/0,
  fun two/0
], fun(Value, _ThisTypeDefinition)->
  Type = case maps:get(<<"one">>, Value, undefined) of
    undefined -> fun two/0;
    _ -> fun one/0
  end,
  {Type, Value}  % resolver should return tuple {ResolvedType, UnwrappedValue}
end).
```

Object Type definition:
```erlang
test_unions() -> graphql:objectType(<<"TestUnions">>, <<>>, #{
  <<"default_resolver">> => #{
    type => ?LIST(fun unions_default/0),
    
    % default resolver just compares first elem in tuple with possibleTypes in union  
    resolver => fun() ->
      [
        {fun one/0, #{<<"one">> => 1}},
        {fun two/0, #{<<"two">> => 2}},
        {fun one/0, #{<<"one">> => 3}},
      ]
    end
  },
  
  <<"custom_resolver">> => #{
    type => ?LIST(fun unions_custom/0),
    resolver => fun()->
      [
        #{<<"one">> => 1},
        #{<<"two">> => 2},
        #{<<"one">> => 3}
      ]
    end
  }
}).
```

## Object types

The most basic components of a GraphQL schema are object types, which just represent a kind
of object you can fetch from your service, and what fields it has.

Creating object type example:
```erlang

graphql:objectType(<<"Name">>, <<"Description">>, #{
  <<"field_name">> => #{
    type => ?STRING,  % what type of value returned by resolver
    description => <<"Human redable description">>,  % optional
    isDeprecated => true, % optional. Default: false
    deprecationReason => <<"Human redable deprecation reason">>, % used when isDeprecated => true for introspection tools
    args => #{  % map of field arguments
      <<"arg_name">> => #{
        type => ?INT
        % optional: description, isDeprecated        
      }
    },
    resolver => fun() -> <<"resolved string">>
  }
})
```

Resolver - function that resolves field value. Arity and arguments passing:
```erlang
case erlang:fun_info(Resolver, arity) of
  {arity, 0} -> Resolver();
  {arity, 1} -> Resolver(ObjectValue);
  {arity, 2} -> Resolver(ObjectValue, ArgumentValues);
  {arity, 3} -> Resolver(ObjectValue, ArgumentValues, Context)
end.
```

where:

- ObjectValue - result of the parent resolver (or InitialValue for query root)
- ArgumentsValues - map of arguments. When argument not provided in query - value is 'null'
- Context - context from parent resolver.

Resolver can return resolved value witch passed to scalar serialize function if type is scalar or as ObjectValue
for each field in child object type
 
Resolver can also overwrite context for his childrens. In this case resolver may look like:
```erlang
resolver => fun(_,_,Context) ->
  {overvrite_context,
    <<"resolved string">>,
    Context#{
      additional_context_data => <<"Data">>
    }
  }
end
```

NOTE: additional_context_data avaliable only for children object type in query

## Custom scalar types

```erlang
custom_type()-> #{
  kind => 'SCALAR',  % required kind for scalar type
  name => 'IntCustom',  % atom
  ofType => null,  % used for nested data types
  description => <<
    "The `Int` scalar type represents non-fractional signed whole numeric ",
    "values. Int can represent values between -(2^53 - 1) and 2^53 - 1 since ",
    "represented in JSON as double-precision floating point numbers specified ",
    "by [IEEE 754](http://en.wikipedia.org/wiki/IEEE_floating_point)."
  >>,

  % this function serialize resolved value
  % Serialize(Result, FieldType, Context);
  serialize => fun serialize/3,
  
  % this function parse variable value
  % ParseValue(maps:get(VariableName, VariableValues, null), ArgumentType);
  parse_value => fun parse_value/2,
  
  % this function parse value defined in query
  % ParseLiteral(Value, ArgumentType)
  parse_literal => fun parse_literal/2
}.
```

## Special thanks

Thanks graphql-elixir for great lex/yecc erlang parser :)