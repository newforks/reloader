%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").


-export([start/0, stop/0]).

% 状态查看
-export([status/0]).

% 手工执行命令
-export([set_check_time/1]).
-export([reload/0]).


-define(SERVER, reloader_server).

%% External API

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start() ->
  application:start(?MODULE).


%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
  application:stop(?MODULE).



%% @doc Set check time
set_check_time(Time) ->
  set_check_time(Time, whereis(?SERVER)).

set_check_time(_, undefined) ->
  io:format("reloader is not started~n");
set_check_time(Time, _) when is_integer(Time) andalso Time >= 0 ->
  gen_server:call(?SERVER, {set_check_time, Time});
set_check_time(_Time, _)  ->
  io:format("[error]check time must be integer and lager than 0 ~n").



%% @doc Manual reload file
reload() ->
  reload(whereis(?SERVER)).

reload(undefined) ->
  io:format("reloader is not started~n");
reload(_IsAlived) ->
  ?SERVER ! doitonce.


%% @doc Get reloader status
status() ->
  status(whereis(?SERVER)).

status(undefined) ->
  io:format("reloader is not started~n");
status(_IsAlived) ->
  gen_server:call(?SERVER, status).



