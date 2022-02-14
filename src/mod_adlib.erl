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
-type priref() :: binary() | integer().

-type api_arg() :: {atom() | binary() | string(), atom() | binary() | string()}.

-export_type([
    adlib_endpoint/0,
    adlib_result/0,
    priref/0
]).


-export([
    event/2,

    observe_rsc_import_fetch/2,

    fetch_record/3,

    import_record/3,
    import_all_async/2,
    import_all/2,
    import_since/3,

    endpoints/1,

    triples/3,

    uri/3,
    uri_to_endpoint/2,
    name_to_endpoint/2
    ]).

% Internal export for async jobs and callbacks
-export([
    import_all_async_task/2,
    import_all_loop/4
    ]).


-define(MEDIA_FETCH_TIMEOUT, 10*60*1000).
-define(MEDIA_FETCH_MAX_LENGTH, 500*1000*1000).

-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").


event(#submit{ message=import_all }, Context) ->
    case z_acl:is_allowed(use, mod_adlib, Context) of
        true ->
            Endpoint = z_context:get_q(<<"endpoint">>, Context),
            import_all_async(Endpoint, Context),
            z_render:growl(?__("Started import of Adlib database.", Context), Context);
        false ->
            z_render:growl(?__("You are not allowed to use mod_adlib.", Context), Context)
    end.


%% @doc Fetch the triples of an adlib endpoint.
observe_rsc_import_fetch(#rsc_import_fetch{ uri = <<"adlib:", _/binary>> = Uri }, Context) ->
    case uri_to_endpoint(Uri, Context) of
        {ok, Endpoint} ->
            Priref = adlib_rdf:uri_to_priref(Uri),
            try
                case triples(Endpoint, Priref, Context) of
                    {ok, Triples} ->
                        {ok, #{
                            <<"uri">> => adlib_rdf:uri(Endpoint, Priref),
                            <<"rdf_triples">> => Triples
                        }};
                    {error, Reason} = Error ->
                        ?zWarning(
                            "Adlib: import triples mapping failed from endpoint '~s' priref '~p': ~p (uri ~s)",
                            [ Endpoint#adlib_endpoint.name, Priref, Reason, Uri ],
                            Context),
                        Error
                end
            catch
                Type:Err:Stack ->
                    ?zError("Adlib import error from endpoint '~s' priref '~p' Error: ~p:~p",
                            [ Endpoint#adlib_endpoint.name, Priref, Type, Err ],
                            Context),
                    ?LOG_ERROR("Adlib import error from endpoint '~s' priref '~p' (uri ~s). Error: ~p:~p",
                              [ Endpoint#adlib_endpoint.name, Priref, Uri, Type, Err ],
                              #{ stack => Stack }),
                    {error, Err}
            end;
        {error, _} ->
            undefined
    end;
observe_rsc_import_fetch(#rsc_import_fetch{}, _Context) ->
    undefined.


%% @doc Import all documents from an Adlib endpoint in a background process.
-spec import_all_async( Endpoint, Context ) -> ok | {error, term()}
    when Endpoint :: adlib_endpoint() | binary(),
         Context :: z:context().
import_all_async(Name, Context)  when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> import_all_async(E, Context);
        {error, _} = Error -> Error
    end;
import_all_async(Endpoint, Context) ->
    ?zInfo(
        "Adlib: queue async import from endpoint '~s'",
        [ Endpoint#adlib_endpoint.name ],
        Context),
    ContextAsync = z_context:prune_for_async(Context),
    sidejob_supervisor:spawn(
            zotonic_sidejobs,
            {?MODULE, import_all_async_task, [ Endpoint, ContextAsync ]}),
    ok.

%% @doc Async task for importing all Adlib docs, running as a zotonic_singular_job
%% to prevent multiple imports running at the same time.
-spec import_all_async_task( adlib_endpoint(), z:context() ) -> any().
import_all_async_task(Endpoint, Context) ->
    jobs:run(zotonic_singular_job, fun() -> import_all(Endpoint, Context) end).


-spec import_all( Endpoint, Context ) -> {ok, Stats} | {error, term()}
    when Endpoint :: adlib_endpoint() | binary(),
         Context :: z:context(),
         Stats :: map().
import_all(Endpoint, Context) ->
    import_since(Endpoint, undefined, Context).


-spec import_since( Endpoint, Since, Context ) -> {ok, Stats} | {error, term()}
    when Endpoint :: adlib_endpoint() | binary(),
         Since :: m_adlib_api:date(),
         Context :: z:context(),
         Stats :: map().
import_since(Name, Since, Context) when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> import_since(E, Since, Context);
        {error, _} = Error -> Error
    end;
import_since(#adlib_endpoint{} = Endpoint, Since, Context) ->
    ?zInfo(
        "Adlib: started import from endpoint '~s' since ~p",
        [ Endpoint#adlib_endpoint.name, Since ],
        Context),
    Stats = #{
        total => 0,
        import_count => 0,
        error_count => 0,
        errors => []
    },
    import_all_loop(m_adlib_api:fetch_since(Endpoint, Since, Context), Stats, Endpoint, Context).


import_all_loop(done, Stats, _Endpoint, _Context) ->
    {ok, Stats};
import_all_loop({error, _} = Error, _Stats, Endpoint, Context) ->
    ?zError(
        "Adlib: data fetch error from endpoint '~s' error ~p",
        [ Endpoint#adlib_endpoint.name, Error ],
        Context),
    Error;
import_all_loop({ok, {Recs, Cont}}, Stats, Endpoint, Context) ->
    Stats1 = lists:foldl(
        fun(#{ <<"@attributes">> := #{ <<"priref">> := PrirefAttr } }, Acc) ->
            Priref = z_convert:to_integer(PrirefAttr),
            case import_record(Endpoint, Priref, Context) of
                {ok, _Id} ->
                    Acc#{
                        total => maps:get(total, Acc) + 1,
                        import_count => maps:get(import_count, Acc) + 1
                    };
                {error, _} ->
                    Acc#{
                        total => maps:get(total, Acc) + 1,
                        error_count => maps:get(error_count, Acc) + 1,
                        errors => [ Priref | maps:get(errors, Acc) ]
                    }
            end
        end,
        Stats,
        Recs),
    case Cont of
        {next, ContFun} ->
            ?MODULE:import_all_loop(ContFun(), Stats1, Endpoint, Context);
        done ->
            ?zInfo(
                "Adlib: import from endpoint '~s' finished, imported ~p, failed ~p",
                [ Endpoint#adlib_endpoint.name,
                  maps:get(import_count, Stats1),
                  maps:get(error_count, Stats1)
                ],
                Context),
            {ok, Stats1}
    end.


%% @doc Fetch a specific record from an endpoint, the Adlib JSON is returned.
-spec fetch_record( Endpoint, Priref, Context ) -> {ok, AdlibJSON} | {error, term()}
    when Endpoint :: mod_adlib:adlib_endpoint() | binary(),
         Priref :: priref(),
         Context :: z:context(),
         AdlibJSON :: map().
fetch_record(Name, Priref, Context) when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> fetch_record(E, Priref, Context);
        {error, _} = Error -> Error
    end;
fetch_record(#adlib_endpoint{} = Endpoint, Priref, Context) ->
    m_adlib_api:fetch_record(Endpoint, Priref, Context).


%% @doc Import a specific record from an endpoint.
-spec import_record( Endpoint, Priref, Context ) -> {ok, RscId} | {error, term()}
    when Endpoint :: mod_adlib:adlib_endpoint() | binary(),
         Priref :: priref(),
         Context :: z:context(),
         RscId :: m_rsc:resource_id().
import_record(Name, Priref, Context) when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> import_record(E, Priref, Context);
        {error, _} = Error -> Error
    end;
import_record(#adlib_endpoint{} = Endpoint, Priref, Context) ->
    Uri = adlib_rdf:uri(Endpoint, Priref),
    try
        case triples(Endpoint, Priref, Context) of
            {ok, Triples} ->
                Options = [
                    {import_edges, 1},
                    {fetch_options, [
                        {timeout, ?MEDIA_FETCH_TIMEOUT},
                        {max_length, ?MEDIA_FETCH_MAX_LENGTH}
                    ]},
                    {uri_template, adlib_rdf:uri(Endpoint, <<":id">>)}
                ],
                Data = #{
                    <<"rdf_triples">> => Triples,
                    <<"uri">> => adlib_rdf:uri(Endpoint, Priref)
                },
                case m_rsc_import:import(Data, Options, Context) of
                    {ok, {Id, _}} ->
                        ?zDebug(
                            "Adlib: import ok from endpoint '~s' priref '~p' to rsc id ~p (uri ~s)",
                            [ Endpoint#adlib_endpoint.name, Priref, Id, Uri ],
                            Context),
                        {ok, Id};
                    {error, Reason} = Error ->
                        ?zWarning(
                            "Adlib: import error from endpoint '~s' priref '~p': ~p (uri ~s)",
                            [ Endpoint#adlib_endpoint.name, Priref, Reason, Uri],
                            Context),
                        Error
                end;
            {error, Reason} = Error ->
                ?zWarning(
                    "Adlib: import triples mapping failed from endpoint '~s' priref '~p': ~p (uri ~s)",
                    [ Endpoint#adlib_endpoint.name, Priref, Reason, Uri ],
                    Context),
                Error
        end
    catch
        Type:Err:Stack ->
            ?zError("Adlib import error from endpoint '~s' priref '~p' Error: ~p:~p",
                    [ Endpoint#adlib_endpoint.name, Priref, Type, Err ],
                    Context),
            ?LOG_ERROR("Adlib import error from endpoint '~s' priref '~p' (uri ~s). Error: ~p:~p",
                       [ Endpoint#adlib_endpoint.name, Priref, Uri, Type, Err ],
                       #{ stack => Stack }),
            {error, Err}
    end.

-spec triples(Endpoint, Priref, Context) -> {ok, Triples} | {error, term()}
    when Endpoint :: #adlib_endpoint{} | binary(),
         Priref :: binary() | integer(),
         Context :: z:context(),
         Triples :: list( zotonic_rdf:rdf_triple() ).
triples(Name, Priref, Context) when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> triples(E, Priref, Context);
        {error, _} = Error -> Error
    end;
triples(#adlib_endpoint{ triples_fun = Fun } = Endpoint, Priref, Context) ->
    case m_adlib_api:fetch_record(Endpoint, Priref, Context) of
        {ok, Rec} ->
            {ok, Fun(Endpoint, Rec, Context)};
        {error, _} = Error ->
            Error
    end.


%% @doc Collect all endpoints from the site and modules.
-spec endpoints( z:context() ) -> list( adlib_endpoint() ).
endpoints(Context) ->
    lists:flatten( z_notifier:map(#adlib_endpoints{}, Context) ).


%% @doc Find the endpoint for an adlib URI.
-spec uri_to_endpoint( Uri, Context ) -> {ok, adlib_endpoint()} | {error, unknown_endpoint}
    when Uri :: uri_string:uri_string() | undefined,
         Context :: z:context().
uri_to_endpoint(undefined, _Context) ->
    {error, unknown_endpoint};
uri_to_endpoint(Uri, Context) ->
    Es = endpoints(Context),
    uri_to_endpoint_1(Uri, Es).

uri_to_endpoint_1(_Uri, []) ->
    {error, enoent};
uri_to_endpoint_1(Uri, [ E | Es ]) ->
    case adlib_rdf:is_matching_uri(E, Uri) of
        true -> {ok, E};
        false -> uri_to_endpoint_1(Uri, Es)
    end.

%% @doc Find the endpoint for an adlib endpoint name.
-spec name_to_endpoint( Name, Context ) -> {ok, adlib_endpoint()} | {error, unknown_endpoint}
    when Name :: binary(),
         Context :: z:context().
name_to_endpoint(Name, Context) ->
    Es = endpoints(Context),
    name_to_endpoint_1(Name, Es).

name_to_endpoint_1(_Name, []) ->
    {error, unknown_endpoint};
name_to_endpoint_1(Name, [ #adlib_endpoint{ name = Name } = E | _ ]) ->
    {ok, E};
name_to_endpoint_1(Name, [ _ | Es ]) ->
    name_to_endpoint_1(Name, Es).


%% @doc Return the uri for an adlib priref, given the name or the endpoint.
-spec uri( Endpoint, Priref, Context ) -> {ok, Uri} | {error, unknown_endpoint}
    when Endpoint :: binary() | adlib_endpoint(),
         Priref :: priref(),
         Context :: z:context(),
         Uri :: uri_string:uri_string().
uri(Name, Priref, Context) when is_binary(Name) ->
    case name_to_endpoint(Name, Context) of
        {ok, E} -> uri(Priref, E, Context);
        {error, _} = Error -> Error
    end;
uri(#adlib_endpoint{} = E, Priref, _Context) ->
    {ok, adlib_rdf:uri(E, Priref)}.
