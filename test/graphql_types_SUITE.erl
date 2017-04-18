-module(graphql_types_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

-define(SERVER, ?MODULE).

all() -> [
  int,
  float,
  bool_true, bool_false,
  enum_one, enum_two,
  list,
  string,
  union_a, union_b
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

test(Document, Expect, Options)->
  RuntimeResult = graphql:exec(schema(), Document, Options),
  CompileResult = gen_server:call(?SERVER, {exec, Document, Options}),

  ?assertEqual(Expect, RuntimeResult),
  ?assertEqual(Expect, CompileResult).

create_case(Field, Value)->
  create_case(Field, Value, #{
    Field => Value
  }).

create_case(Field, Value, Initial)->
  Document = <<"{ ", Field/binary ," }">>,
  Expect = #{data => #{
    Field => Value
  }},

  Options = #{
    initial => Initial
  },

  test(Document, Expect, Options).

schema() -> ?SCHEMA(#{
  query => fun query/0
}).

db_resolver(_,_, #{db := DB}) -> DB.

query() -> ?OBJECT('Query', "", #{
  "int" => ?FIELD(?INT),
  "float" => ?FIELD(?FLOAT),
  "bool" => ?FIELD(?BOOLEAN),
  "enum" => ?FIELD(fun enum/0),
  "list" => ?FIELD(?LIST(?INT)),
  "string" => ?FIELD(?STRING),
  "union" => ?FIELD(fun a_or_b/0)
}).

a_or_b() -> ?UNION('AorB', null, [
  fun a/0,
  fun b/0
],fun
  (#{<<"a">> := _} = Data, _) -> {fun a/0, Data};
  (#{<<"b">> := _} = Data, _) -> {fun b/0, Data}
end).

a() -> ?OBJECT('A', null, #{
  "a" => ?FIELD(?INT)
}).

b() -> ?OBJECT('B', null, #{
  "b" => ?FIELD(?INT)
}).

enum() -> ?ENUM('EnumTest', "Test enum", [
  ?ENUM_VAL(1, <<"ONE">>, "One value"),
  ?ENUM_VAL(2, <<"TWO">>, "Two value")
]).

int(_)-> create_case(<<"int">>, 1).
float(_)-> create_case(<<"float">>, 1.2).
bool_true(_) -> create_case(<<"bool">>, true).
bool_false(_) -> create_case(<<"bool">>, false).
enum_one(_) -> create_case(<<"enum">>, <<"ONE">>, #{<<"enum">> => 1}).
enum_two(_) -> create_case(<<"enum">>, <<"TWO">>, #{<<"enum">> => 2}).
list(_) -> create_case(<<"list">>, [1,2,3,4]).
string(_) -> create_case(<<"string">>, <<"test string">>).
union_a(_) ->
  Document = <<"{ union { ... on A { a }} }">>,
  Expect = #{data => #{
    <<"union">> => #{<<"a">> => 1}
  }},
  test(Document, Expect, #{
    initial => #{<<"union">> => #{<<"a">> => 1}}
  }).
union_b(_) ->
  Document = <<"{ union { ... on B { b }} }">>,
  Expect = #{data => #{
    <<"union">> => #{<<"b">> => 2}
  }},
  test(Document, Expect, #{
    initial => #{<<"union">> => #{<<"b">> => 2}}
  }).
