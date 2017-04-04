-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").
-module(graphql_resolver_test).

%% prepare schema

schema()-> ?SCHEMA(#{
  query => fun query/0
}).

query()-> ?OBJECT("Query", "", #{
  "ok" => ?FIELD(?BOOLEAN, "Resolver return {ok, true}", fun()-> {ok, true} end),
  "error" => ?FIELD(?BOOLEAN, "{error, Reason}", fun()-> {error, "Because we can"} end),
  "error_custom" => ?FIELD(?BOOLEAN, "Custom error", fun() ->
    {error, #{
      message => <<"Binary - because whole Reason serialized to json">>,
      line => ?LINE,
      module => ?MODULE
    }}
  end)
}).

%% tests

ok_test()->
  Document = <<"{ ok }">>,
  Expect = #{
    data => #{
      <<"ok">> => true
    }
  },
  assert(Document, Expect).

error_test()->
  Document = <<"{ error }">>,
  Expect = #{
    errors => [#{message => <<"Because we can">>}]
  },
  assert(Document, Expect).

error_custom_test()->
  Document = <<"{ error_custom }">>,
  Expect = #{
    errors => [#{
      message => <<"Binary - because whole Reason serialized to json">>,
      line => 17,
      module => ?MODULE
    }]
  },
  assert(Document, Expect).


%% helper
assert(Document, Expect)->
  {Result, _} = graphql:exec(schema(), Document),
  ?assertEqual(Expect, Result).


