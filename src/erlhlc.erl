-module(erlhlc).

-behaviour(application).
-behaviour(supervisor).

%% API Function Exports
-export([now/0]).

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
          [{erlhlc_sync, {erlhlc_sync, start_link, [ClockFun]}, permanent, 5000,
            worker, [erlhlc_sync]}]
         }}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec now() -> hlc:timestamp().
now() ->
    gen_server:call(erlhlc_sync, now).

