-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-module(graphql_mutation_test).


schema() -> ?SCHEMA(#{
  mutation => fun mutation/0
}).

increment_resolver(Obj) -> {ok, Obj + 1}.

mutation()-> ?OBJECT("Mutation", "", #{
  "a" => ?FIELD(?INT, "", fun increment_resolver/1)
}).

first_test() ->
  Document = <<"mutation { a }">>,
  Expect = #{
    <<"a">> => 1
  },

  #{data := Result} = graphql:exec(schema(), Document, #{
    initial => 0
  }),

  ?assertEqual(Expect, Result).