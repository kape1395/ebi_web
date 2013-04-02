-module(ebi_web_model_json).
-compile([{parse_transform, lager_transform}]).
-export([encode/1, decode/2]).
-export([encode_tstamp/1, decode_tstamp/1]).
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("ebi_core/include/ebi_model.hrl").

%%
%% calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}).
%%
-define(UNIX_BIRTH, 62167219200).
-define(MEGA_SECS, 1000000).


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

encode(#ebi_reaction{name = Name, description = Description, definition = Definition}) ->
    {[
        {name, encode_string(Name)},
        {description, encode_string(Description)},
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
    ]};

encode(#ebi_compartment{name = Name, description = Description, definition = Definition}) ->
    {[
        {name, encode_string(Name)},
        {description, encode_string(Description)},
        {type, element(1, Definition)},
        {definition, encode(Definition)}
    ]};

encode(#ebi_cdef_solution{}) ->
    {[
    ]};

encode(#ebi_cdef_diffusive{}) ->
    {[
    ]};

encode(#ebi_cdef_insulating{}) ->
    {[
    ]};

encode(#ebi_cdef_solid_electrode{}) ->
    {[
    ]}.


%%
%%  Decode
%%
-spec decode(Type :: atom(), Json :: term()) -> term().
decode(_, null) ->
    undefined;

decode(Type, List) when is_list(List) ->
    [ decode(Type, Element) || Element <- List ];

decode(model, {PL}) ->
    #model{
        id          = decode_string(proplists:get_value(id,          PL, null)),
        ref         = decode_string(proplists:get_value(ref,         PL, null)),
        name        = decode_string(proplists:get_value(name,        PL, null)),
        description = decode_string(proplists:get_value(description, PL, null)),
        status      = decode_atom  (proplists:get_value(status,      PL, null)),
        changed     = decode_tstamp(proplists:get_value(changed,     PL, null)),
        changed_by  = decode_string(proplists:get_value(changed_by,  PL, null)),
        definition  = decode(ebi_model, proplists:get_value(definition,  PL, null)),
        parameters  = case proplists:get_value(parameters,  PL, null) of
            null -> undefined;
            Prms -> lists:map(fun decode_string/1, Prms)
        end
    };

decode(ebi_model, {PL}) ->
    #ebi_model{
        species      = decode(ebi_species,     proplists:get_value(species,      PL, null)),
        reactions    = decode(ebi_reaction,    proplists:get_value(reactions,    PL, null)),
        compartments = decode(ebi_compartment, proplists:get_value(compartments, PL, null))
    }.


%% =============================================================================
%%  Helper functions.
%% =============================================================================


%%
%%  Encode everything to a json string.
%%
encode_string(undefined) ->
    null;

encode_string(Text) when is_list(Text) ->
    list_to_binary(Text).


%%
%%  Encode timestamp.
%%
encode_tstamp(undefined) ->
    null;

encode_tstamp(Now = {_MegaSecs, _Secs, MicroSecs}) ->
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_datetime(Now),
    Args = [Y, M, D, H, Mi, S, MicroSecs],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ", Args),
    erlang:iolist_to_binary(Date);

encode_tstamp({{Y, M, D}, {H, Mi, S}}) ->
    Args = [Y, M, D, H, Mi, S],
    Date = io_lib:format("~B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ", Args),
    erlang:iolist_to_binary(Date).


%%
%%  Decode string.
%%
decode_string(null) ->
    undefined;

decode_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).


%%
%%  Decode atom.
%%
decode_atom(null) ->
    undefined;

decode_atom(Atom) when is_atom(Atom) ->
    Atom;

decode_atom(Binary) when is_binary(Binary) ->
    erlang:binary_to_existing_atom(Binary).


%%
%%  Decode timestamp.
%%
decode_tstamp(null) ->
    undefined;

decode_tstamp(<<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary, "T",
                Hour:2/binary, ":", Min:2/binary,   ":", Sec:2/binary, Rest/binary>>) ->
    Date = {
        {binary_to_integer(Year), binary_to_integer(Month), binary_to_integer(Day)},
        {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}
    },
    decode_tstamp(Date, Rest).

decode_tstamp(Date, <<".", MSec:6/binary, "Z">>) ->
    DateSecs = calendar:datetime_to_gregorian_seconds(Date) - ?UNIX_BIRTH,
    {DateSecs div ?MEGA_SECS, DateSecs rem ?MEGA_SECS, binary_to_integer(MSec)};

decode_tstamp(Date, <<"Z">>) ->
    Date.


