-module(anvil_local).

-export([upload/3]).
-export([sync/2]).

-define(CRLF, <<"\r\n">>).

-include_lib("kernel/include/file.hrl").

upload(Source, Buildpack, Opts) when is_binary(Source) ->
  upload(binary_to_list(Source), Buildpack, Opts);
upload(Source, Buildpack, Opts) ->
  {ok, Manifest} = sync(Source, Opts),
  anvil_builder:build(<<"/build">>, [
    {<<"buildpack">>, Buildpack},
    {<<"cache">>, fast_key:get(cache, Opts)},
    {<<"env">>, jsx:encode(fast_key:get(env, Opts, []))},
    {<<"keepalive">>, <<"1">>},
    {<<"manifest">>, jsx:encode(Manifest)},
    {<<"type">>, fast_key:get(type, Opts)}
  ], Opts).

sync(Source, Opts) ->
  {ok, Ignore} = ignore(Source, Opts),
  {ok, Manifest} = find(Source, Ignore, Opts),
  {ok, Missing} = find_missing(Manifest, Opts),
  Length = length(Missing),
  case Length > 0 of
    true ->
      anvil_utils:log(Opts, [<<"Uploading ">>, integer_to_binary(Length), <<" file(s)\n">>]);
    _ ->
      anvil_utils:log(Opts, <<"Using cached files...\n">>)
  end,
  {ok, MissingFiles} = select_missing(Missing, Manifest),
  ok = upload_missing(MissingFiles, Opts),
  {ok, Manifest}.

ignore(_Source, Opts) ->
  %% TODO
  {ok, fast_key:get(ignore, Opts, [".git"])}.

find(Source, _Ignores, Opts) ->
  {ok, filelib:fold_files(Source, ".*", true, fun (File, Acc) ->
    [stat_file(File, relative(Source, File), Opts)|Acc] %% TODO filter from ignores
  end, [])}.

stat_file(Abs, Rel, _Opts) ->
  {ok, Info} = file:read_file_info(Abs, [{time, posix}]),
  Manifest = [
    {name, list_to_binary(Rel)},
    {mtime, Info#file_info.mtime},
    {size, Info#file_info.size},
    {mode, integer_to_binary(Info#file_info.mode, 8)}
  ],
  case Info#file_info.type of
    symlink ->
      %% TODO follow symlink
      Realpath = "",
      [{link, Realpath}|Manifest];
    regular ->
      {ok, Binary} = file:read_file(Abs),
      Hash = calculate_hash(Binary),
      [{hash, Hash},{contents, Binary}|Manifest]
  end.

find_missing(Manifest, Opts) ->
  Conn = fast_key:get(connection, Opts),
  Body = jsx:encode([
    {<<"manifest">>, jsx:encode(remove_contents(Manifest, []))}
  ]),
  Ref = gun:post(Conn, <<"/manifest/diff">>, [{<<"content-type">>, <<"application/json">>}], Body),
  {response, nofin, 200, _} = gun:await(Conn, Ref, 10000),
  {ok, Res} = gun:await_body(Conn, Ref, 10000),
  {ok, jsx:decode(Res)}.

select_missing(Missing, Manifest) ->
  {ok, select_missing(Manifest, sets:from_list(Missing), [])}.

select_missing([], _, Acc) ->
  Acc;
select_missing([File|Manifest], Missing, Acc) ->
  case sets:is_element(fast_key:get(hash, File), Missing) of
    true ->
      select_missing(Manifest, Missing, [File|Acc]);
    _ ->
      select_missing(Manifest, Missing, Acc)
  end.

upload_missing(Missing, Opts) ->
  Conn = fast_key:get(connection, Opts),
  Refs = [begin
    Name = fast_key:get(name, File),
    Contents = fast_key:get(contents, File),
    Hash = fast_key:get(hash, File),
    Headers = [
      {<<"content-type">>, <<"multipart/form-data; boundary=", Hash/binary>>}
    ],
    Body = [
      <<"--">>, Hash, ?CRLF,
      <<"Content-Disposition: form-data; name=\"data\"; filename=\"">>, Name, <<"\"">>, ?CRLF,
      <<"Content-Type: application/octet-stream">>, ?CRLF, ?CRLF,
      Contents,
      <<"--">>, Hash, <<"--">>, ?CRLF
    ],
    Ref = gun:post(Conn, [<<"/file/">>, Hash], Headers, Body),
    {Name, Ref}
  end || File <- Missing],
  handle_results(Refs, Conn, Opts).

handle_results([], _, _) ->
  ok;
handle_results([{_, Ref}|Refs], Conn, Opts) ->
  io:format("TODO ~p~n", [gun:await(Conn, Ref, 10000)]),
  handle_results(Refs, Conn, Opts).

relative([], [$/|File]) ->
  File;
relative([], File) ->
  File;
relative([A,B,C,D,E|Root], [A,B,C,D,E|File]) ->
  relative(Root, File);
relative([A,B,C,D|Root], [A,B,C,D|File]) ->
  relative(Root, File);
relative([A,B,C|Root], [A,B,C|File]) ->
  relative(Root, File);
relative([A,B|Root], [A,B|File]) ->
  relative(Root, File);
relative([C|Root], [C|File]) ->
  relative(Root, File).

calculate_hash(Binary) ->
  Bin = crypto:hash(sha256, Binary),
  << <<Y>> || <<X:4>> <= Bin, Y <- to_hex(integer_to_list(X,16))>>.

to_hex("A") ->
  "a";
to_hex("B") ->
  "b";
to_hex("C") ->
  "c";
to_hex("D") ->
  "d";
to_hex("E") ->
  "e";
to_hex("F") ->
  "f";
to_hex(X) ->
  X.

remove_contents([], Acc) ->
  Acc;
remove_contents([File|Files], Acc) ->
  File2 = lists:keydelete(contents, 1, File),
  remove_contents(Files, [File2|Acc]).
