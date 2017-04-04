% This test inspired by issue https://github.com/graphql-erlang/graphql/issues/32

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-module(graphql_context_test).

schema() -> ?SCHEMA(#{
  query => fun query/0
}).

db_resolver(_,_, #{db := DB}) -> DB.

query() -> ?OBJECT("Query", "", #{
  "a" => ?FIELD(?INT, "Db from context", fun db_resolver/3),
  "b" => ?FIELD(fun b/0, "Here context overwriting", fun(_,_,Context)-> {overwrite_context, #{}, Context#{db => 2}} end),
  "c" => ?FIELD(?INT, "Db from context", fun db_resolver/3),
  "d" => ?FIELD(?INT, "Db from context", fun db_resolver/3)
}).

b()-> ?OBJECT("B", "", #{
  "b1" => ?FIELD(?INT, "Db from context", fun db_resolver/3),
  "b2" => ?FIELD(?INT, "Db from context", fun db_resolver/3)
}).

query_test() ->
  Document = <<"{
    a
    b {
      b1
      b2
    }
    c
    d
  }">>,

  Context = #{db => 1},

  Expect = #{
    <<"a">> => 1,
    <<"b">> => #{
      <<"b1">> => 2,
      <<"b2">> => 2
    },
    <<"c">> => 1,
    <<"d">> => 1
  },

  #{ data := Result } = graphql:exec(schema(), Document, #{
    context => Context
  }),

  ?assertEqual(Expect, Result).