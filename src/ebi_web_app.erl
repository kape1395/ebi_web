-module(ebi_web_app).
-behaviour(application).
-export([start/2, stop/1]).


%% =============================================================================
%%  Application callbacks
%% =============================================================================


%%  @doc
%%  Start the application.
%%
start(_StartType, _StartArgs) ->
    {ok, _Pid} = ebi_web_sup:start_link().


%%  @doc
%%  Stop the application.
%%
stop(_State) ->
    ok.



%% =============================================================================
%%  Helper functions.
%% =============================================================================
