-module(anvil_builder).

-export([build/3]).

build(_Path, Body, _Opts) ->
  _Body2 = filter_undefined(Body, []),
  ok.

filter_undefined([], Acc) ->
  lists:reverse(Acc);
filter_undefined([{_, undefined}|Rest], Acc) ->
  filter_undefined(Rest, Acc);
filter_undefined([Param|Rest], Acc) ->
  filter_undefined(Rest, [Param|Acc]).
