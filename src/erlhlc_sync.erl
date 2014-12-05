-module(erlhlc_sync).
-behaviour(gen_server).

-include_lib("hlc/include/hlc.hrl").

-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(ClockFun) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ClockFun], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([undefined]) ->
    case hlc:init([fun physical_clock/0]) of
        {ok, C} -> {ok, C};
        Error -> Error
    end;
init([ClockFun]) ->
    case hlc:init([ClockFun]) of
        {ok, C} -> {ok, C};
        Error -> Error
    end.

handle_call(now, From, C) ->
    {reply, T, C1} = hlc:handle_call(now, From, C),
    [{?SERVER, Node} ! {update, node(), T} || Node <- nodes()],
    {reply, T, C1};
handle_call(timestamp, _From, C) ->
    {reply, CLT, C} = hlc:handle_call(timestamp, undefined, C),
    {reply, CLT, C}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update, N, RT}, C) ->
    %{reply, CLT, C} = hlc:handle_call(timestamp, undefined, C),
    %case (RT#timestamp.wall_time + RT#timestamp.logical)
    %     - (CLT#timestamp.wall_time + CLT#timestamp.logical) of
    %    TNDiff when TNDiff > 1000 ->
    %        io:format("UPD ~p~n", [TNDiff]);
    %    TNDiff when TNDiff < -1000 ->
    %        io:format("UPD ~p~n", [TNDiff]);
    %    _ -> ok
    %end,
    {reply, _LT, C1} = hlc:handle_call({update, N, RT}, undefined, C),
    {noreply, C1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

physical_clock() ->
    {Mega,Sec,Micro} = erlang:now(),
    (Mega*1000000+Sec)*1000000+Micro.
