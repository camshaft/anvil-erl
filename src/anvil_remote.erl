-module(anvil_remote).

-export([upload/3]).

upload(Source, Buildpack, Opts) ->
  anvil_builder:build(<<"/build">>, [
    {<<"buildpack">>, Buildpack},
    {<<"cache">>, fast_key:get(cache, Opts)},
    {<<"env">>, jsx:encode(fast_key:get(env, Opts, []))},
    {<<"source">>, Source},
    {<<"type">>, fast_key:get(type, Opts)}
  ], Opts).
