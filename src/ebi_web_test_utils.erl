%
% Copyright 2013 Karolis Petrauskas
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
-module(ebi_web_test_utils).
-export([
    configure/1, start/0, stop/0
]).


%% =============================================================================
%%  API functions.
%% =============================================================================


configure(YawsConfigFile) ->
    application:load(yaws),
    application:set_env(yaws, conf, YawsConfigFile).


start() ->
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(yaws),
    ok = application:start(ebi_web),
    ok.


stop() ->
    ok = application:stop(ebi_web),
    ok = application:stop(yaws),
    ok = application:stop(ssl),
    ok = application:stop(public_key),
    ok = application:stop(crypto),
    ok.

