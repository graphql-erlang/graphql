-module(graphql_context_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

-define(SERVER, ?MODULE).

all() -> [
  main
].

init_per_suite(Config) ->
  {ok, Pid} = graphql_srv:start_link(fun schema/0, #{
    introspection => false,
    server_name => ?SERVER
  }),
  unlink(Pid),
  [{pid, Pid}|Config].

end_per_suite(Config) ->
  Pid = proplists:get_value(pid, Config),
  exit(Pid, kill),
  Config.


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

main(_Config)->

  Document = <<"{
    a
    b {
      b1
      b2
    }
    c
    d
  }">>,

  Expect = #{data => #{
    <<"a">> => 1,
    <<"b">> => #{
      <<"b1">> => 2,
      <<"b2">> => 2
    },
    <<"c">> => 1,
    <<"d">> => 1
  }},

  Options = #{
    context => #{db => 1}
  },

  RuntimeResult = graphql:exec(schema(), Document, Options),
  CompileResult = gen_server:call(?SERVER, {exec, Document, Options}),

  ?assertEqual(Expect, RuntimeResult),
  ?assertEqual(Expect, CompileResult).