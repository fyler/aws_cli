-module(aws_cli_SUITE).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 50000).

-compile(export_all).

init_per_suite(Config) ->
  ulitos_app:ensure_started(aws_cli),
  Config.

end_per_suite(_) ->
  application:stop(erlexec),
  application:stop(aws_cli),
  ok.

init_per_group(unpack, Config) ->
  [{name, "20"}, {to, ""}|Config];

init_per_group(unpack2, Config) ->
  [{name, "21"}, {to, "scorm"}|Config];

init_per_group(_Group, Config) ->
  Config.

end_per_group(_Group, _Config) ->
  ok.

all() ->
  [
    {group, aws_cli}
  ].

groups() ->
  [
    {
      aws_cli, [sequence], 
      [
        {
          success, [sequence],
          [
            sync_success,
            async_success
          ]
        },
        {
          fail, [sequence],
          [
            sync_fail,
            async_fail
          ]
        }
      ]
    }
  ].

group(aws_cli) -> [{timetrap, ?TIMEOUT}];

group(_Group) -> [].

sync_success(_Config) ->
  Res = aws_cli:run_sync("ls", #{}),
  ?assertMatch({ok, _}, Res).

async_success(_Config) ->
  _Pid = aws_cli:run("ls", #{}),
  Result = 
    receive 
      {Res, _} -> Res;
      _ -> error
      after ?TIMEOUT ->
        timeout
    end,
  ?assertEqual(aws_cli_complete,  Result).

sync_fail(_Config) ->
  Res = aws_cli:run_sync("echo 1 >&2", #{}),
  ?assertMatch({error, _}, Res).

async_fail(_Config) ->
  Cmd = io_lib:format("echo ~s>&2", ["1"]),
  _Pid = aws_cli:run(Cmd, #{}),
  Result = 
    receive 
      {Res, _} -> Res;
      _ -> error
      after ?TIMEOUT ->
        timeout
    end,
  ?assertEqual(aws_cli_error,  Result).



