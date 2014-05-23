-module(anvil).

-export([start/0]).
-export([build/0]).
-export([build/1]).
-export([build/2]).

start() ->
  ok = application:ensure_started(crypto),
  ok = application:ensure_started(asn1),
  ok = application:ensure_started(public_key),
  ok = application:ensure_started(ssl),
  ok = application:ensure_started(cowlib),
  ok = application:ensure_started(ranch),
  ok = application:ensure_started(gun).

build() ->
  {ok, Dir} = file:get_cwd(),
  build(Dir).

build(Dir) ->
  build(Dir, []).

build(Source, Opts) ->
  %% {ok, Buildpack} = anvil_buildpack:resolve(fast_key:get(buildpack, Opts), Opts),
  Buildpack = <<>>,
  {ok, Conn} = gun:open("api.anvilworks.org", 443, [{type, ssl}]),
  Opts2 = [{connection, Conn}|Opts],
  case anvil_utils:is_remote(Source) of
    true ->
      anvil_remote:upload(Source, Buildpack, Opts2);
    _ ->
      anvil_local:upload(Source, Buildpack, Opts2)
  end.
