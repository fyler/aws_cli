-module(aws_cli_s3).

-export([
  cp/2, 
  cp/3,
  cp_r/2, 
  cp_r/3,
  cp_r/4, 
  sync/2,
  sync/3, 
  ls/1, 
  ls/2,
  exist/2,
  exist/3
]).

cp(File, Destination, Options) ->
  Cmd = io_lib:format("aws s3 cp ~s ~s ~s", [acl(Options), File, Destination]),
  aws_cli:run(Cmd, Options).

cp(File, Destination) ->
  cp(File, Destination, #{}).

cp_r(Folder, Destination, Params, Options) ->
  Cmd = io_lib:format("aws s3 cp ~s ~s ~s --recursive ~s", [acl(Options), Folder, Destination, Params]),
  aws_cli:run(Cmd, Options).

cp_r(Folder, Destination, #{} = Options) ->
  cp_r(Folder, Destination, "", Options);

cp_r(Folder, Destination, Params) ->
  cp_r(Folder, Destination, Params, #{}).

cp_r(Folder, Destination) ->
  cp_r(Folder, Destination, "", #{}).

sync(Folder, Destination, Options) ->
  Cmd = io_lib:format("aws s3 sync ~s ~s ~s", [acl(Options), Folder, Destination]),
  aws_cli:run(Cmd, Options).

sync(Folder, Destination) ->
  sync(Folder, Destination, #{}).

ls(Path, Options) ->
  Cmd = io_lib:format("aws s3 ls ~s", [Path]),
  case aws_cli:run_sync(Cmd, Options) of
    {ok, Data} ->
      Data;
    {error, _Data} ->
      error
  end.

ls(Path) ->
  ls(Path, #{}).

exist(Bucket, Prefix, Options) ->
  Cmd = io_lib:format("aws s3api head-object --bucket ~s --key ~s", [Bucket, Prefix]),
  case aws_cli:run_sync(Cmd, Options) of
    {ok, _Data} ->
      true;
    {error, _Data} ->
      false
  end.

exist(Bucket, Prefix) ->
  exist(Bucket, Prefix, #{}).

%%internal functions
acl(#{acl := private}) ->
  "--acl private";

acl(#{acl := public}) ->
  "--acl public-read";

acl(#{acl := authenticated}) ->
  "--acl authenticated-read";

acl(#{}) ->
  "".


 %%tests
 -ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

command_ls_test() ->
  meck:expect(aws_cli, run_sync, fun(Cmd, _) -> {ok, lists:flatten(Cmd)} end),
  ?assertEqual("aws s3 ls bucket", ls("bucket")).

command_test() ->
  meck:expect(aws_cli, run, fun(Cmd, _) -> lists:flatten(Cmd) end),
  ?assertEqual("aws s3 cp  test.txt bucket/folder", cp("test.txt", "bucket/folder")),
  ?assertEqual("aws s3 cp --acl private test.txt bucket/folder", cp("test.txt", "bucket/folder", #{acl => private})),
  ?assertEqual("aws s3 cp  tmp/1 s3://bucket/tmp/1 --recursive ", cp_r("tmp/1", "s3://bucket/tmp/1")),
  ?assertEqual("aws s3 cp  tmp/1 s3://bucket/tmp/1 --recursive --include *.log", cp_r("tmp/1", "s3://bucket/tmp/1", "--include *.log")),
  ?assertEqual("aws s3 cp --acl public-read tmp/1 s3://bucket/tmp/1 --recursive --include *.log", cp_r("tmp/1", "s3://bucket/tmp/1", "--include *.log", #{acl => public})).

command_sync_test() ->
  meck:expect(aws_cli, run, fun(Cmd, _) -> lists:flatten(Cmd) end),
  ?assertEqual("aws s3 sync  tmp/1 s3://bucket/tmp/1", sync("tmp/1", "s3://bucket/tmp/1")),
  ?assertEqual("aws s3 sync --acl authenticated-read tmp/1 s3://bucket/tmp/1", sync("tmp/1", "s3://bucket/tmp/1", #{acl => authenticated})).

-endif.
