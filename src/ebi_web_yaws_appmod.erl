-module(ebi_web_yaws_appmod).
-compile([{parse_transform, lager_transform}]).
-export([out/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("ebi_core/include/ebi_model.hrl").
-include_lib("yaws/include/yaws_api.hrl").
-include("ebi_web.hrl").

-define(APP, "ebi").
-define(API, "api").
-define(GUI, "gui").
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

%% -----------------------------------------------------------------------------
%%  API
%% -----------------------------------------------------------------------------

handle_request([?APP, ?API], 'GET', _Arg) ->
    [
        {status, 200},
        {header, {"Link", "<model>; rel=models"}},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "biosensor"], 'GET', _Arg) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "biosensor", _BiosensorId], 'GET', _Arg) ->
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode({[]})}
    ];

handle_request([?APP, ?API, "model"], 'GET', _Arg) ->
    {ok, Models} = ebi_store:get_models(all),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ebi_web_model_json:encode(Models))}
    ];

handle_request([?APP, ?API, "model", ModelId], 'GET', _Arg) ->
    {ok, Model} = ebi_store:get_model(ModelId),
    [
        {status, 200},
        {content, ?MEDIATYPE_JSON, jiffy:encode(ebi_web_model_json:encode(Model))}
    ];

%% -----------------------------------------------------------------------------
%%  GUI
%% -----------------------------------------------------------------------------

handle_request([?APP, ?GUI | Tail] = Path, 'GET', Arg) ->
    case Tail of
        [] ->
            Uri = yaws_api:request_url(Arg),
            case lists:last(Uri#url.path) of
                $/ -> serve_priv_file("gui/index.html", "text/html");
                _  -> redirect_to_gui()
            end;
        _ ->
            [?APP | WwwPath] = Path,
            FileName = string:join(WwwPath, "/"),
            serve_priv_file(FileName, yaws_api:mime_type(FileName))
    end;


%% -----------------------------------------------------------------------------
%%  Other paths
%% -----------------------------------------------------------------------------

handle_request([], 'GET', _Arg) ->
    redirect_to_gui();

handle_request([?APP], 'GET', _Arg) ->
    redirect_to_gui();


handle_request(["favicon.ico" = FileName], 'GET', _Arg) ->
    serve_priv_file(FileName, yaws_api:mime_type(FileName));

handle_request(_Path, 'GET', Arg) ->
    [
        {status, 404},
        {ehtml, [{p, [], [
            io_lib:format("404: Page ~p not found.", [yaws_api:request_url(Arg)])
        ]}]}
    ];

handle_request(_Path, _Method, _Arg) ->
    [
        {status, 400}
    ].



%% =============================================================================
%%  Helper functions.
%% =============================================================================


redirect_to_gui() ->
    {redirect, lists:flatten(["/", ?APP, "/", ?GUI, "/"])}.


serve_priv_file(FileName, ContentType) ->
    {ok, ThisApp} = application:get_application(?MODULE),
    PrivDir = case code:priv_dir(ThisApp) of
        {error,bad_name} -> "priv"; % To allow testing without creating whole app.
        Dir -> Dir
    end,
    AbsolutePath = lists:flatten(PrivDir ++ "/www/" ++ FileName),
    {ok, Content} = file:read_file(AbsolutePath),
    {content, ContentType, Content}.


