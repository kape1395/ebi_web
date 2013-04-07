%
% Copyright 2012 Karolis Petrauskas
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%     http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.
%
-module(ebi_web_model_json_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("ebi_core/include/ebi.hrl").
-include_lib("ebi_core/include/ebi_model.hrl").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  Tests
%%

timestamp_secs_test() ->
    Str = <<"2013-04-02T23:06:17Z">>,
    Date = {{2013, 04, 02}, {23, 06, 17}},
    Str = ebi_web_model_json:encode_tstamp(Date),
    Date = ebi_web_model_json:decode_tstamp(Str),
    ok.

timestamp_msec_test() ->
    Str = <<"2013-04-02T19:39:08.646842Z">>,
    Date = {1364,931548,646842},
    Str = ebi_web_model_json:encode_tstamp(Date),
    Date = ebi_web_model_json:decode_tstamp(Str),
    ok.


encode_decode_test() ->
    EbiModel = #ebi_model{
        species = [
            #ebi_species{name = "S", description = "Substrate"},
            #ebi_species{name = "P", description = "Product"},
            #ebi_species{name = "Eo", description = "Enzyme in the oxidized form"},
            #ebi_species{name = "Er", description = "Enzyme in the reduced form"}
        ],
        reactions = [
            #ebi_reaction{name = "R1", description = "First reaction", definition = #ebi_rdef_mm{
                substrate = "S",
                product = "P",
                vmax = "Vmax",
                km = "KM"
            }},
            #ebi_reaction{name = "R2", description = "Second reaction", definition = #ebi_rdef_simple{
                reagents = [{"Er", 1}, {"S", 1}],
                products = [{"Eo", 1}, {"P", 1}],
                rateconst = "k1"
            }}
        ],
        compartments = [
            #ebi_compartment{
                name = "\\Omega_1",
                description = "Solution",
                definition = #ebi_cdef_solution{
                    species = [#ebi_comp_species{species = "S", concentration = "S0"}],
                    diffusion = "0",
                    nernst_thickness = "d1"
                }
            },
            #ebi_compartment{
                name = "\\Omega_2",
                description = "Enzyme",
                definition = #ebi_cdef_diffusive{
                    species = [#ebi_comp_species{species = "E", diffusion = "0", concentration = "E0"}],
                    diffusion = "De",
                    reactions = ["R2"],
                    thickness = "d2"
                }
            },
            #ebi_compartment{
                name = "\\Omega_3",
                description = "Solid electrode",
                definition = #ebi_cdef_solid_electrode{
                    el_reaction = "R1"
                }
            }
        ]
    },
    Model = #model{
        id = "M-10012",
        ref = "0123456789012345678901234567890123456789",
        name = "Name",
        description = "Desc",
        status = active,
        changed = erlang:now(),
        changed_by = "Jonas Jonaitis",
        definition = EbiModel,
        parameters = ["asd"],
        representations = []
    },
    Json = jiffy:encode(ebi_web_model_json:encode(Model)),
    Model = ebi_web_model_json:decode(jiffy:decode(Json)),
    ok.




