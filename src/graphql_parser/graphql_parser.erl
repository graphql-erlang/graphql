-module(graphql_parser).
-author("mrchex").

%% API
-export([
  parse/1
]).


-spec parse(binary() | maybe_improper_list()) -> {'error', map()} | {'ok', map()}.
parse(Q) when is_binary(Q) ->
  ListQ = binary_to_list(Q),
  parse_list(ListQ);
parse(Q) when is_list(Q)->
  parse_list(Q).

-spec parse_list(maybe_improper_list()) -> {'error', map()} | {'ok', map()}.
parse_list(Document)->
  {ok, Tokens, _} = graphql_lexer:string(Document),
  graphql_parser_yecc:parse(Tokens).