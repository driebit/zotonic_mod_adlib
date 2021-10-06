%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Driebit BV
%% @doc Map Adlib records to RDF triples.

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

-module(adlib_triple).

-export([
    uri/2,
    is_matching_endpoint/2,
    triples/3
    ]).


-include("../../include/mod_adlib.hrl").


%% @doc Extract the unique resource URI from an Adlib record.
-spec uri( Endpoint, Record ) -> {ok, uri_string:uri_string()} | {error, nomatch}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Record :: map().
uri(Endpoint, #{
        <<"@attributes">> := #{
            <<"priref">> := Priref
        }
    }) ->
    #{
        host := Host,
        path := Path
    } = uri_string:parse(Endpoint#adlib_endpoint.api_url),
    BaseDir = case binary:split(Path, <<"/">>, [ global ]) of
        [ <<>>, <<"wwwopac.", _/binary>> ] -> <<>>;
        [ <<>>, BD | _ ] -> <<"/", BD/binary>>;
        _ -> <<>>
    end,
    Uri = iolist_to_binary([
        "urn:adlib:", Host, BaseDir,
        $/, Endpoint#adlib_endpoint.database,
        $/, Priref
    ]),
    {ok, Uri};
uri(_Endpoint, _Record) ->
    {error, nomatch}.


%% @doc Check if the URI belongs to the given Adlib endpoint.
-spec is_matching_endpoint( Uri, Endpoint ) -> boolean()
    when Uri :: uri_string:uri_string(),
         Endpoint :: mod_adlib:adlib_endpoint().
is_matching_endpoint(<<"urn:adlib:", Uri/binary>>, Endpoint) ->
    #{
        host := Host,
        path := Path
    } = uri_string:parse(Endpoint#adlib_endpoint.api_url),
    BaseDir = case binary:split(Path, <<"/">>, [ global ]) of
        [ <<>>, <<"wwwopac.", _/binary>> ] -> <<>>;
        [ <<>>, BD | _ ] -> BD;
        _ -> <<>>
    end,
    Database1 = z_convert:to_binary(Endpoint#adlib_endpoint.database),
    case binary:split(Uri, <<"/">>, [ global ]) of
        [ Host, BaseDir, Database1, _Id ] -> true;
        _ -> false
    end;
is_matching_endpoint(_Uri, _Endpoint) ->
    false.


%% @doc Extract RDF triples from an Adlib record.
-spec triples( Endpoint, Record, Context ) -> {ok, list( map() )} | {error, term()}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Record :: map(),
         Context :: z:context().
triples(Endpoint, Record, Context) ->
    {ok, []}.

