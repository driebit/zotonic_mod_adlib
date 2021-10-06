%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Driebit BV
%% @doc Adlib API access and support routines.

%% Copyright 2021 Driebit BV
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_adlib).

-mod_title("Adlib support").
-mod_description("Access Adlib collections via their APIs.").

-include("../include/mod_adlib.hrl").

-type adlib_endpoint() :: #adlib_endpoint{}.
-type adlib_result() :: {ok, term()} | {error, term()}.

-type api_arg() :: {atom() | binary() | string(), atom() | binary() | string()}.

-export_type([
    adlib_endpoint/0,
    adlib_result/0
]).
