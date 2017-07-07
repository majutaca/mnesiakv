%%%-------------------------------------------------------------------
%% @doc mnesiakv public API
%% @end
%%%-------------------------------------------------------------------

-module(mnesiakv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mnesiakv_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
