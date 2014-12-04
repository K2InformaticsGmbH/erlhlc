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
    N = hlc:now(C),
    self() ! {flood, N},
    {reply, N, C};
handle_call({update, RT}, _From, C) ->    
    io:format("update ~p~n", [RT]),
    {reply, hlc:update(C, RT), C}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({flood, N}, C) ->
    case nodes() of
        [] -> ok;
        Nodes ->
            {ResL, _BadNodes} = rpc:multicall(Nodes, gen_server, call, [?SERVER, {update, N}]),
            case ResL of
                [] -> ok;
                ResL ->
                    catch hlc:update(C, lists:max(ResL))
            end
    end,
    {noreply, C}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

