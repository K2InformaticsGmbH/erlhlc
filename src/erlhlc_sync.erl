-module(erlhlc_sync).
-behaviour(gen_server).
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
    case hlc:new() of
        {ok, C} -> {ok, C};
        Error -> Error
    end;
init([ClockFun]) ->
    case hlc:new(ClockFun) of
        {ok, C} -> {ok, C};
        Error -> Error
    end.

handle_call(now, _From, C) ->
    T = hlc:now(C),
    ?SERVER ! {flood, T},
    {reply, T, C};
handle_call(timestamp, _From, C) ->
    {reply, hlc:timestamp(C), C}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update, RT}, C) ->
    NewRT = hlc:update(C, RT),
    io:format("update ~p~n", [{RT, NewRT}]),
    {noreply, C};
handle_info({flood, RT}, C) ->
    [{?SERVER, Node} ! {update, RT} || Node <- nodes()],
    {noreply, C}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

