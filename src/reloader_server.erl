%%%-------------------------------------------------------------------
%%% @author zhaoweiguo
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 29. 八月 2018 下午8:47
%%%-------------------------------------------------------------------
-module(reloader_server).
-author("zhaoweiguo").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1, reload_all_changed/0]).


-record(state, {
  last,
  tref,
  check_time
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(CheckTime) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CheckTime], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([CheckTime]) ->
  TimerRef = case CheckTime of
               0 ->
                 undefined ;
               _Other ->
                 erlang:send_after(CheckTime*1000, self(), doit)
  end,
  {ok, #state{
    last = stamp(),
    tref = TimerRef,
    check_time = CheckTime
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(status, _From, State) ->
  Reply = {{check_time, State#state.check_time}, {last_reload, State#state.last}},
  {reply, Reply, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({set_check_time, Time}, State) ->
  NewState = set_check_time(Time, State),
  {noreply, NewState};
handle_cast(_Req, State) ->
  {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(doit, #state{
                    check_time = CheckTime
                  } = State) ->
  Now = stamp(),
  try
    _ = doit(State#state.last, Now)
  catch
    _:R ->
      io:format("reload failed R:~w Stack:~p~n",[R, erlang:get_stacktrace()])
  end,
  case CheckTime of
    0 ->
      TimerRef=undefined;
    _ ->
      TimerRef = erlang:send_after(CheckTime*1000, self(), doit)
  end,
  {noreply, State#state{
    last = Now,
    tref = TimerRef
  }};
handle_info(doitonce, State) ->
  Now = stamp(),
  try
    _ = doit(State#state.last, Now)
  catch
    _:R ->
      error_logger:error_msg(
        "reload failed R:~w Stack:~p~n",[R, erlang:get_stacktrace()])
  end,
  {noreply, State#state{
    last = Now
  }};
handle_info(Info, State) ->
  error_logger:warning_msg("unknow info ~p~n", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
%% gen_server termination callback.
terminate(_Reason, #state{
                      tref = undefined
                  }) ->
  ok;
terminate(_Reason, State) ->
  erlang:cancel_timer(State#state.tref),
  %% {ok, cancel} = timer:cancel(State#state.tref),
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}].
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
  [begin code:purge(M), code:load_file(M) end || M <- Modules].

-spec all_changed() -> [atom()].
%% @doc Return a list of beam modules that have changed.
all_changed() ->
  [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

-spec is_changed(atom()) -> boolean().
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
  try
    module_vsn(M:module_info()) =/= module_vsn(code:get_object_code(M))
  catch _:_ ->
    false
  end.

reload_all_changed() ->
  reload_modules(all_changed()).

%% Internal API

module_vsn({M, Beam, _Fn}) ->
  {ok, {M, Vsn}} = beam_lib:version(Beam),
  Vsn;
module_vsn(L) when is_list(L) ->
  {_, Attrs} = lists:keyfind(attributes, 1, L),
  {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
  Vsn.

doit(From, To) ->
  [case file:read_file_info(Filename) of
     {ok, #file_info{mtime = Mtime}} when Mtime >= From, Mtime < To ->
       reload(Module);
     {ok, _} ->
       unmodified;
     {error, enoent} ->
       %% The Erlang compiler deletes existing .beam files if
       %% recompiling fails.  Maybe it's worth spitting out a
       %% warning here, but I'd want to limit it to just once.
       gone;
     {error, Reason} ->
       io:format("Error reading ~s's file info: ~p~n",
         [Filename, Reason]),
       error
   end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
  io:format("Reloading ~p ...", [Module]),
  code:purge(Module),
  case code:load_file(Module) of
    {module, Module} ->
      io:format("[ok].~n"),
      reload;
    {error, Reason} ->
      io:format("[fail]: ~p.~n", [Reason]),
      error
  end.

% @todo 加载并运行单元测试
%%reloadandtest(Module) ->
%%  io:format("Reloading ~p ...", [Module]),
%%  code:purge(Module),
%%  case code:load_file(Module) of
%%    {module, Module} ->
%%      io:format(" ok.~n"),
%%      case erlang:function_exported(Module, test, 0) of
%%        true ->
%%          io:format(" - Calling ~p:test() ...", [Module]),
%%          case catch Module:test() of
%%            ok ->
%%              io:format(" ok.~n"),
%%              reload;
%%            Reason ->
%%              io:format(" fail: ~p.~n", [Reason]),
%%              reload_but_test_failed
%%          end;
%%        false ->
%%          reload
%%      end;
%%    {error, Reason} ->
%%      io:format(" fail: ~p.~n", [Reason]),
%%      error
%%  end.

stamp() ->
  erlang:localtime().

% 没有改变
set_check_time(Time, #state{check_time = Time}=State) ->
  io:format("none has been changed~n"),
  State;
% 由原来定时改为不定时
set_check_time(0, #state{tref = TimerRef}=State) ->
  erlang:cancel_timer(TimerRef),
  NewState = State#state{
    tref = undefined,
    check_time = 0
  },
  io:format("canceled the timer~n"),
  NewState;
% 由原来不定时改为定时
set_check_time(Time, #state{check_time = 0}=State) ->
  TimerRef = erlang:send_after(Time*1000, self(), doit),
  NewState = State#state{
    tref = TimerRef,
    check_time = Time
  },
  io:format("set timer: ~p seconds", [Time]),
  NewState;
% 定时时间变化
set_check_time(Time,  State) ->
  NewState = State#state{
    check_time = Time
  },
  io:format("set timer: ~p seconds", [Time]),
  NewState.
