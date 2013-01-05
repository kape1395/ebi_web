-module(ebi_web_yaws_appmod).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("ebi_web.hrl").

-define(API_ROOT, "api").
-define(MEDIATYPE_JSON, "application/vnd.kape1395.ebi-v1+json; level=0").
-define(MEDIATYPE_TERM, "application/x-erlang-term").


%% =============================================================================
%%  API functions.
%% =============================================================================

%%
%%
%%
out(Arg) ->
    Uri = yaws_api:request_url(Arg),
    Path = string:tokens(Uri#url.path, "/"),
    Method = yaws_api:http_request_method(Arg#arg.req),
    lager:info("Handling request: path=~p, method=~p", [Path, Method]),
    handle_request(Path, Method, Arg).


%%
%%  Handling of REST style requests.
%%
handle_request([], 'GET', _Arg) ->
    [
        {status, 200},
        {header, {"Link", "<api>; rel=api"}},
        {header, {"Link", "<mgr>; rel=ui"}},
        serve_priv_file("index.html", "text/html")
    ];

handle_request([?API_ROOT], 'GET', _Arg) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ].



%% =============================================================================
%%  Helper functions.
%% =============================================================================

serve_priv_file(FileName, ContentType) ->
    {ok, ThisApp} = application:get_application(?MODULE),
    PrivDir = case code:priv_dir(ThisApp) of
        {error,bad_name} -> "priv"; % To allow testing without creating whole app.
        Dir -> Dir
    end,
    AbsolutePath = lists:flatten(PrivDir ++ "/www/" ++ FileName),
    {ok, Content} = file:read_file(AbsolutePath),
    {content, ContentType, Content}.


