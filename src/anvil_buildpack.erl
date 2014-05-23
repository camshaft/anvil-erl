-module(anvil_buildpack).

-export([resolve/2]).

resolve(undefined, _) ->
  {ok, ""};
resolve(Buildpack, _Opts) ->
  case anvil_utils:is_remote(Buildpack) of
    true ->
      {ok, Buildpack};
    _ ->
      {todo, Buildpack}
  end.
