-include_lib("eunit/include/eunit.hrl").

-module(graphql_parser_test).
-author("mrchex").

base_query1_test()->
  Q = <<"{ base }">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{
    definitions => [#{
      kind => 'OperationDefinition',
      operation => query,
      selectionSet => #{
        kind => 'SelectionSet',
        selections => [#{
          kind => 'Field',
          name => #{
            kind => 'Name',
            value => <<"base">>
          }
        }]
      }
    }],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).

base_query2_test()->
  Q = <<"query { base }">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{
    definitions => [#{
      kind => 'OperationDefinition',
      operation => query,
      selectionSet => #{
        kind => 'SelectionSet',
        selections => [#{
          kind => 'Field',
          name => #{
            kind => 'Name',
            value => <<"base">>
          }
        }]
      }
    }],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).

base_query_named_test()->
  Q = <<"query QueryName { base }">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{
    definitions => [#{
      kind => 'OperationDefinition',
      operation => query,
      name => #{kind => 'Name',value => <<"QueryName">>},
      selectionSet => #{
        kind => 'SelectionSet',
        selections => [#{
          kind => 'Field',
          name => #{
            kind => 'Name',
            value => <<"base">>
          }
        }]
      }
    }],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).

argument_all_scalars_types_test()->
  Q = <<"{ test(a:1 b: 1.2 c: ENUMVALUE d: \"string value\" d: true e: false ) }">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{definitions => [#{kind => 'OperationDefinition',
    operation => query,
    selectionSet => #{kind => 'SelectionSet',
      selections => [#{arguments => [#{kind => 'Argument',
        name => #{kind => 'Name', value => <<"a">>},
        value => #{kind => 'IntValue', value => 1}},
        #{kind => 'Argument',
          name => #{kind => 'Name', value => <<"b">>},
          value => #{kind => 'FloatValue', value => 1.2}},
        #{kind => 'Argument',
          name => #{kind => 'Name', value => <<"c">>},
          value => #{kind => 'EnumValue', value => <<"ENUMVALUE">>}},
        #{kind => 'Argument',
          name => #{kind => 'Name', value => <<"d">>},
          value => #{kind => 'StringValue', value => <<"string value">>}},
        #{kind => 'Argument',
          name => #{kind => 'Name', value => <<"d">>},
          value => #{kind => 'BooleanValue', value => true}},
        #{kind => 'Argument',
          name => #{kind => 'Name', value => <<"e">>},
          value => #{kind => 'BooleanValue', value => false}}],
        kind => 'Field',
        name => #{kind => 'Name', value => <<"test">>}}]}}],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).

variable_definition_test() ->
  Q = <<"query($a: NullableType $b: NonNullType! = 1) { foo(z: $a w: $b) }">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{definitions => [#{kind => 'OperationDefinition',
    operation => query,
    selectionSet => #{kind => 'SelectionSet',
      selections => [#{arguments => [#{kind => 'Argument',
        name => #{kind => 'Name',value => <<"z">>},
        value => #{kind => 'Variable',name => #{kind => 'Name',value => <<"a">>}}},
        #{kind => 'Argument',
          name => #{kind => 'Name',value => <<"w">>},
          value => #{kind => 'Variable',name => #{kind => 'Name',value => <<"b">>}}}],
        kind => 'Field',
        name => #{kind => 'Name',value => <<"foo">>}}]},
    variableDefinitions => [#{kind => 'VariableDefinition',
      type => #{kind => 'NamedType',name => #{kind => 'Name',value => <<"NullableType">>}},
      variable => #{kind => 'Variable',name => #{kind => 'Name',value => <<"a">>}}},
      #{defaultValue => #{kind => 'IntValue',value => 1},
        kind => 'VariableDefinition',
        type => #{kind => 'NonNullType',
          type => #{kind => 'NamedType',name => #{kind => 'Name',value => <<"NonNullType">>}}},
        variable => #{kind => 'Variable',name => #{kind => 'Name',value => <<"b">>}}}]}],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).


directives_test() ->
  Q = <<"{foo @someDirective(when: false)}">>,
  AST = graphql_parser:parse(Q),

  ExpectedAST = #{definitions => [#{kind => 'OperationDefinition',
    operation => query,
    selectionSet => #{kind => 'SelectionSet',
      selections => [#{directives => [#{arguments => [#{kind => 'Argument',
        name => #{kind => 'Name',value => <<"when">>},
        value => #{kind => 'BooleanValue',value => false}}],
        kind => 'Directive',
        name => #{kind => 'Name',value => <<"someDirective">>}}],
        kind => 'Field',
        name => #{kind => 'Name',value => <<"foo">>}}]}}],
    kind => 'Document'},

  ?assertEqual({ok, ExpectedAST}, AST).


fragments_test()->
  Q = <<"query withFragments {
      user {
        friends { ...friendFields }
        mutualFriends { ...friendFields }
      }
    }

    fragment friendFields on User {
      id
    }">>,

  AST = graphql_parser:parse(Q),

  ExpectedAST = #{definitions => [#{kind => 'OperationDefinition',
    name => #{kind => 'Name',value => <<"withFragments">>},
    operation => query,
    selectionSet => #{kind => 'SelectionSet',
      selections => [#{kind => 'Field',
        name => #{kind => 'Name',value => <<"user">>},
        selectionSet => #{kind => 'SelectionSet',
          selections => [#{kind => 'Field',
            name => #{kind => 'Name',value => <<"friends">>},
            selectionSet => #{kind => 'SelectionSet',
              selections => [#{kind => 'FragmentSpread',
                name => #{kind => 'Name',value => <<"friendFields">>}}]}},
            #{kind => 'Field',
              name => #{kind => 'Name',value => <<"mutualFriends">>},
              selectionSet => #{kind => 'SelectionSet',
                selections => [#{kind => 'FragmentSpread',
                  name => #{kind => 'Name',value => <<"friendFields">>}}]}}]}}]}},
    #{kind => 'FragmentDefinition',
      name => #{kind => 'Name',value => <<"friendFields">>},
      selectionSet => #{kind => 'SelectionSet',
        selections => [#{kind => 'Field',name => #{kind => 'Name',value => <<"id">>}}]},
      typeCondition => #{kind => 'NamedType',name => #{kind => 'Name',value => <<"User">>}}}],
    kind => 'Document'},
  
  ?assertEqual({ok, ExpectedAST}, AST).
