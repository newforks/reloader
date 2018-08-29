%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").


-export([start/0, stop/0]).

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

set_check_time(Time) when is_integer(Time) andalso Time >= 0 ->
    gen_server:cast(?SERVER, {set_check_time, Time});
set_check_time(_Time)  ->
  error_logger:error_msg("check time must be integer and lager than 0 ~n").

reload() ->
  ?SERVER ! doit.

