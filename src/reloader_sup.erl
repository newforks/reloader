
-module(reloader_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CHILD(I, Args, Type), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

-define(SERVER, reloader_server).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    case application:get_env(reloader, check_time) of
        undefined ->
            {ok, { {one_for_one, 5, 10}, [?CHILD(?SERVER, [], worker)]}};
        {ok, undefined} ->
            {ok, { {one_for_one, 5, 10}, [?CHILD(?SERVER, [], worker)]}};
        {ok, Value} -> 
            {ok, { {one_for_one, 5, 10}, [?CHILD(?SERVER, [Value*1000], worker)]}}
    end.

