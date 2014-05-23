-module(anvil_utils).

-export([is_remote/1]).
-export([log/2]).

is_remote(Path) when is_binary(Path) ->
  is_remote(binary_to_list(Path));
is_remote(Path) ->
  case http_uri:parse(Path) of
    {error, no_scheme} ->
      false;
    {ok, _} ->
      true
  end.

log(_Opts, Message) ->
  io:format([Message]).
