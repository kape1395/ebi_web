-module(ebi_web_model_json).
-compile([{parse_transform, lager_transform}]).
-export([encode/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("ebi_core/include/ebi_model_native.hrl").


%% =============================================================================
%%  API functions.
%% =============================================================================


encode(List) when is_list(List) ->
    [ encode(Element) || Element <- List ];

encode(#model{id = _Id, name = Name, description = Desc, definition = #model_def{ref = Ref}}) ->
    {[
        {id, 'TODO'},
        {name, encode_string(Name)},
        {description, encode_string(Desc)},
        {definition, encode_string(Ref)}
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

encode_string(Text) ->
    encode_string(Text).

