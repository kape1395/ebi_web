-module(ebi_web_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include("ebi_web.hrl").


%% =============================================================================
%%  API functions
%% =============================================================================


%%
%%
%%
start_link() ->
    supervisor:start_link(?MODULE, {}).



%% =============================================================================
%%  Callbacks
%% =============================================================================


%%
%%
%%
init({}) ->
    {ok, {{one_for_all, 100, 10}, []}}.