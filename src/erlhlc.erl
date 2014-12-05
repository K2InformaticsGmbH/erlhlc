-module(erlhlc).

-behaviour(application).
-behaviour(supervisor).

-include_lib("hlc/include/hlc.hrl").

%% API Function Exports
-export([last_now/0, next_now/0, last_ts/0, next_ts/0, loader/1]).

%% Application functions
-export([start/0, stop/0]).

%% Application callbacks
-export([start/2, stop/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    ok = application:start(?MODULE).
start(_StartType, _StartArgs) ->
    supervisor:start_link(
      {local, ?MODULE}, ?MODULE,
      [case application:get_env(clockfun) of
           {ok, FunBin} when is_binary(FunBin) ->               
               try
                   {ok,ErlTokens,_} = erl_scan:string(binary_to_list(FunBin)),
                   {ok,ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
                   {value,Fun,_} = erl_eval:exprs(ErlAbsForm,[]),
                   if is_function(Fun, 0) -> Fun;
                      true -> undefined
                   end
               catch
                   _:_ -> undefined
               end;
           _ -> undefined
       end]).

stop() ->
   ok = application:stop(?MODULE).
stop(_State) ->
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([ClockFun]) ->
    {ok, {{one_for_one, 5, 10},
          [{erlhlc_sync, {erlhlc_sync, start_link, [ClockFun]}, permanent,
            5000, worker, [erlhlc_sync]}]
         }}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc erlang:timestamp() formatting of last_ts/0
-spec last_now() -> erlang:timestamp().
last_now() -> ts2now(last_ts()).
    
%% @doc erlang:timestamp() formatting of next_ts/0
-spec next_now() -> erlang:timestamp().
next_now() -> ts2now(next_ts()).

%% @doc see timestamp in deps/hlc/src/hlc.erl
-spec last_ts() -> hlc:timestamp().
last_ts() ->
    gen_server:call(erlhlc_sync, timestamp).

%% @doc see now in deps/hlc/src/hlc.erl
-spec next_ts() -> hlc:timestamp().
next_ts() ->
    gen_server:call(erlhlc_sync, now).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% @doc converts hlc:timestamp() to erlang:timestamp() format
-spec ts2now(hlc:timestamp()) -> erlang:timestamp().
ts2now(#timestamp{} = T) ->
    Ts = T#timestamp.wall_time + T#timestamp.logical,
    Micro = Ts rem 1000000,
    MegNSec = ((Ts - Micro) div 1000000),
    Sec = MegNSec rem 1000000,
    Mega = (MegNSec - Sec) div 1000000,
    {Mega, Sec, Micro}.

% only for test
loader({Loaders, Delay}) ->
    [begin
         timer:sleep(I * Delay),
         spawn(fun Fire() ->
            erlhlc:next_now(),
            timer:sleep(Delay),
            Fire()
           end)
     end || I <- lists:seq(1, Loaders)];
loader(M) when is_integer(M), M > 3 ->
    F = [{I, M div I} || I <- lists:seq(2, M div 2 - 1), M rem I == 0],
    loader(case F of
               [] -> {1, 1000 div M};
               F when length(F) > 2 ->
                   {P, N} = lists:nth(length(F) div 2, F),
                   {P, N div M * 1000};
               F ->
                   [{P, N}|_] = F,
                   {P, N div M * 1000}
           end);
loader(M) when is_integer(M), M >= 1 ->
    loader({1, 1000 div M}).
