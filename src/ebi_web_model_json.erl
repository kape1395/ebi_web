-module(ebi_web_model_json).
-compile([{parse_transform, lager_transform}]).
-export([encode/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("ebi_core/include/ebi_model.hrl").


%% =============================================================================
%%  API functions.
%% =============================================================================


encode(undefined) ->
    null;

encode(List) when is_list(List) ->
    [ encode(Element) || Element <- List ];

encode(#model{
        id = Id, ref = Ref, name = Name, description = Desc, status = Status,
        changed = Ch, changed_by = By, definition = Def, parameters = Prms
    }) ->
    {[
        {id, encode_string(Id)},
        {ref, encode_string(Ref)},
        {name, encode_string(Name)},
        {description, encode_string(Desc)},
        {status, Status},
        {changed, encode_tstamp(Ch)},
        {changed_by, encode_string(By)},
        {definition, encode(Def)},
        {parameters, case Prms of
            undefined -> null;
            _ -> lists:map(fun encode_string/1, Prms)
        end}
    ]};

encode(#ebi_model{species = Species, reactions = Reactions, compartments = Compartments}) ->
    {[
        {species,       encode(Species)},
        {reactions,     encode(Reactions)},
        {compartments,  encode(Compartments)}
    ]};

encode(#ebi_species{name = Name, description = Description}) ->
    {[
        {name, encode_string(Name)},
        {description, encode_string(Description)}
    ]};

encode(#ebi_reaction{name = Name, definition = Definition}) ->
    {[
        {name, encode_string(Name)},
        {type, element(1, Definition)},
        {definition, encode(Definition)}
    ]};

encode(#ebi_rdef_mm{substrate = S, product = P, vmax = VMax, km = KM}) ->
    {[
        {substrate, encode_string(S)},
        {product, encode_string(P)},
        {vmax, encode_string(VMax)},
        {km, encode_string(KM)}
    ]};

encode(#ebi_rdef_simple{reagents = R, products = P, rateconst = C}) ->
    F = fun ({Name, Num}) -> {[{species, encode_string(Name)}, {number, Num}]} end,
    {[
        {reagents, [ F(X) || X <- R]},
        {products, [ F(X) || X <- P]},
        {rateconst, encode_string(C)}
    ]}.



%% =============================================================================
%%  Helper functions.
%% =============================================================================


%%
%%
%%
encode_string(undefined) ->
    null;

encode_string(Text) ->
    list_to_binary(Text).


%%
%%
%%
encode_tstamp(undefined) ->
    null;

encode_tstamp({{Y, M, D}, {H, Mi, S}}) ->
    Args = [Y, M, D, H, Mi, S],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", Args),
    erlang:iolist_to_binary(Date);

encode_tstamp(Now) ->
    {_MegaSecs, _Secs, MicroSecs} = Now,
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Now),
    Args = [Y, M, D, H, Mi, S, MicroSecs],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ", Args),
    erlang:iolist_to_binary(Date).

