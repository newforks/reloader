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

-export([start_link/1, start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([all_changed/0]).
-export([is_changed/1]).
-export([reload_modules/1, reload_all_changed/0]).


-record(state, {
  last,
  tref,
  check_time
}).

%% External API
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link(CheckTime) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [CheckTime], []).


%% @spec init([] | [CheckTime]) -> {ok, State}
%% @doc 不执行定时
init([]) ->
  {ok, #state{
    last = stamp(),
    tref = undefined,
    check_time = 0
  }};
%% @doc 执行定时
init([CheckTime]) ->
  TimerRef = erlang:send_after(CheckTime, self(), doit),
  {ok, #state{
    last = stamp(),
    tref = TimerRef,
    check_time = CheckTime
  }}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(_Req, _From, State) ->
  {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
% 没有改变
handle_cast({set_check_time, Time}, #state{check_time = Time}=State) ->
  {noreply, State};
% 由原来定时改为不定时
handle_cast({set_check_time, 0}, #state{tref = TimerRef}=State) ->
  erlang:cancel_timer(TimerRef),
  {noreply, State#state{
    tref = undefined,
    check_time = 0
  }};
% 由原来不定时改为定时
handle_cast({set_check_time, Time}, #state{check_time = 0}=State) ->
  TimerRef = erlang:send_after(Time, self(), doit),
  {noreply, State#state{
    tref = TimerRef,
    check_time = Time*1000
  }};
% 定时时间变化
handle_cast({set_check_time, Time}, State) ->
  {noreply, State#state{
    check_time = Time*1000
  }};
handle_cast(_Req, State) ->
  {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, #state{
                    check_time = CheckTime
                  } = State) ->
  Now = stamp(),
  try
    _ = doit(State#state.last, Now)
  catch
    _:R ->
      error_logger:error_msg(
        "reload failed R:~w Stack:~p~n",[R, erlang:get_stacktrace()])
  end,
  case CheckTime of
    0 ->
      TimerRef=undefined;
    _ ->
      TimerRef = erlang:send_after(CheckTime, self(), doit)
  end,
  {noreply, State#state{
    last = Now,
    tref = TimerRef
  }};
handle_info(Info, State) ->
  error_logger:warning_msg("unknow info ~p~n", [Info]),
  {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, #state{
                      tref = undefined
                  }) ->
  ok;
terminate(_Reason, State) ->
  erlang:cancel_timer(State#state.tref),
  %% {ok, cancel} = timer:cancel(State#state.tref),
  ok.

%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
  {ok, State}.

%% @spec reload_modules([atom()]) -> [{module, atom()} | {error, term()}]
%% @doc code:purge/1 and code:load_file/1 the given list of modules in order,
%%      return the results of code:load_file/1.
reload_modules(Modules) ->
  [begin code:purge(M), code:load_file(M) end || M <- Modules].

%% @spec all_changed() -> [atom()]
%% @doc Return a list of beam modules that have changed.
all_changed() ->
  [M || {M, Fn} <- code:all_loaded(), is_list(Fn), is_changed(M)].

%% @spec is_changed(atom()) -> boolean()
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
       error_logger:error_msg("Error reading ~s's file info: ~p~n",
         [Filename, Reason]),
       error
   end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
  error_logger:info_msg("Reloading ~p ...", [Module]),
  code:purge(Module),
  case code:load_file(Module) of
    {module, Module} ->
      error_logger:info_msg("reload ~w ok.~n", [Module]),
      reload;
    {error, Reason} ->
      error_logger:error_msg("reload fail: ~p.~n", [Reason]),
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

