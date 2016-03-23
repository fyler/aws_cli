-module(aws_cli).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(job, {
  ospid = 0 :: integer(),
  pid :: undefined | pid(),
  cmd = "" :: string(),
  stdout = [] :: [binary()],
  stderr = [] :: [binary()],
  from :: undefined | {reference(), pid()} | pid()
}).

-record(state, {
  jobs = #{} :: #{integer() => #job{}},
  pid_to_job = #{} :: #{pid() => integer()}
}).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, run/2, run_sync/2, abort/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
     terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

run(Cmd, Options) ->
  gen_server:call(?SERVER, {run, Cmd, Options}).

run_sync(Cmd, Options) ->
  gen_server:call(?SERVER, {run_sync, Cmd, Options}, maps:get(timeout, Options, 5000)).

abort(OsPid) ->
  gen_server:call(?SERVER, {abort, OsPid}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
  {ok, #state{}}.

handle_call({run, Command, Options}, {From, _Tag}, #state{jobs = Jobs, pid_to_job = PidToJob} = State) ->
  Cmd = lists:flatten(Command),
  {ok, Pid, OsPid} = exec:run_link(Cmd, [stderr, stdout, monitor, {env, environment(Options)}]),
  error_logger:info_msg("aws_cli command:'~s', process: ~d", [Cmd, OsPid]),
  Job = #job{ospid = OsPid, pid = Pid, cmd = Cmd, from = From},
  {reply, OsPid, State#state{jobs = Jobs#{OsPid => Job}, pid_to_job = PidToJob#{Pid => OsPid}}};

handle_call({run_sync, Command, Options}, From, #state{jobs = Jobs, pid_to_job = PidToJob} = State) ->
  Cmd = lists:flatten(Command),
  {ok, Pid, OsPid} = exec:run_link(Cmd, [stderr, stdout, monitor, {env, environment(Options)}]),
  error_logger:info_msg("aws_cli sync command:'~s', process: ~d", [Cmd, OsPid]),
  Job = #job{ospid = OsPid, pid = Pid, cmd = Cmd, from = From},
  {noreply, State#state{jobs = Jobs#{OsPid => Job}, pid_to_job = PidToJob#{Pid => OsPid}}};

handle_call({abort, OsPid}, _From, #state{jobs = Jobs, pid_to_job = PidToJob} = State) ->
  case Jobs of
    #{OsPid := #job{pid = Pid}} ->
      exec:stop(OsPid),
      {reply, ok, State#state{jobs = maps:remove(OsPid, Jobs), pid_to_job = maps:remove(Pid, PidToJob)}};
    #{} ->
      {reply, ok, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({stdout, OsPid, Msg}, #state{jobs = Jobs} = State) ->
  #{OsPid := #job{stdout = Stdout} = Job} = Jobs,
  NewStdout = [Msg|Stdout],
  {noreply, State#state{jobs = Jobs#{OsPid => Job#job{stdout = NewStdout}}}};

handle_info({stderr, OsPid, Msg}, #state{jobs = Jobs} = State) ->
  #{OsPid := #job{stderr = Stderr} = Job} = Jobs,
  NewStderr = [Msg|Stderr],
  {noreply, State#state{jobs = Jobs#{OsPid => Job#job{stderr = NewStderr}}}};

handle_info({'DOWN', _Ref, process, Pid, normal}, #state{jobs = Jobs, pid_to_job = PidToJob} = State) ->
  #{Pid := OsPid} = PidToJob,
  #{OsPid := #job{cmd = Cmd, stdout = Stdout, stderr = Stderr, from = From}} = Jobs, 
  case From of
    {_, _} ->
      case Stderr of
        [] -> 
          gen_server:reply(From, {ok, Stdout});
        _ -> 
          gen_server:reply(From, {error, Stderr}), 
          error_logger:error("aws_cli command ~s failed: ~p", [Cmd, Stderr])
      end;
    _ ->
      case Stderr of
        [] -> 
          From ! {aws_cli_complete, Stdout};
        _ -> 
          From ! {aws_cli_error, Stderr}
      end
  end,
  {noreply, State#state{jobs = maps:remove(OsPid, Jobs), pid_to_job = maps:remove(Pid, PidToJob)}};

handle_info({'DOWN', _Ref, process, Pid, _Status}, #state{jobs = Jobs, pid_to_job = PidToJob} = State) ->
  #{Pid := OsPid} = PidToJob,
  #{OsPid := #job{cmd = Cmd, stderr = Stderr, from = From}} = Jobs, 
  error_logger:error("aws_cli command ~s failed: ~p", [Cmd, Stderr]),
  case From of
    {_, _} ->
      gen_server:reply(From, {error, Stderr});
    _ ->
      From ! {aws_cli_error, Stderr}
  end,
  {noreply, State#state{jobs = maps:remove(OsPid, Jobs), pid_to_job = maps:remove(Pid, PidToJob)}};

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

environment(#{credentials := #{key := Key, secret := Secret, region := Region}}) ->
  [{"AWS_ACCESS_KEY_ID", Key}, {"AWS_SECRET_ACCESS_KEY", Secret}, {"AWS_DEFAULT_REGION", Region}];

environment(#{credentials := #{key := Key, secret := Secret}}) ->
  [{"AWS_ACCESS_KEY_ID", Key}, {"AWS_SECRET_ACCESS_KEY", Secret}];

environment(#{credentials := #{region := Region}}) ->
  [{"AWS_DEFAULT_REGION", Region}];

environment(#{}) ->
  [].

%%tests
 -ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

envrironment_test() ->

  Key = "key", Secret = "secret", Region = "region",

  Env1 = environment(#{credentials => #{key => Key, secret => Secret, region => Region}}),
  ?assertEqual(3, length(Env1)),
  ?assertEqual(Key, proplists:get_value("AWS_ACCESS_KEY_ID", Env1)),
  ?assertEqual(Secret, proplists:get_value("AWS_SECRET_ACCESS_KEY", Env1)),
  ?assertEqual(Region, proplists:get_value("AWS_DEFAULT_REGION", Env1)),

  Env2 = environment(#{credentials => #{key => Key, secret => Secret}}),
  ?assertEqual(2, length(Env2)),
  ?assertEqual(Key, proplists:get_value("AWS_ACCESS_KEY_ID", Env2)),
  ?assertEqual(Secret, proplists:get_value("AWS_SECRET_ACCESS_KEY", Env2)),

  Env3 = environment(#{credentials => #{region => Region}}),
  ?assertEqual(1, length(Env3)),
  ?assertEqual(Region, proplists:get_value("AWS_DEFAULT_REGION", Env3)).

-endif.

