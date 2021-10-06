%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Driebit BV
%% @doc Access the Adlib API, fetch JSON data.

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

-module(adlib_api).

-export([
    fetch_record/3,
    fetch_since/3,
    list_databases/2
    ]).

% Exported for continuation function returned from fetch_since.
-export([
    fetch_since_pager/5
    ]).


-include_lib("zotonic_core/include/zotonic.hrl").
-include("../..//include/mod_adlib.hrl").

% Default timeout of z_url_fetch is 20sec, set to 50sec.
-define(FETCH_TIMEOUT, 50000).

% Default limit for Adlib searches
-define(ADLIB_PAGELEN, 100).


-type api_error() :: enoent
                   | eacces
                   | map()      % #{  <<"info">> := binary(), <<"message">> := binary() }
                   | term().

-type date() :: calendar:datetime()
              | calendar:date()
              | binary()
              | string()
              | integer().

-export_type([ api_error/0, date/0 ]).


%% @doc Fetch a single record from an adlib API endpoint.
-spec fetch_record(Priref :: binary() | string() | integer(), mod_adlib:adlib_endpoint(), z:context() ) -> {ok, map()} | {error, api_error()}.
fetch_record(Priref, Endpoint, Context) ->
    #adlib_endpoint{
        api_url = URL,
        database = Database
    } = Endpoint,
    Priref1 = z_convert:to_binary(Priref),
    Params = [
        {output, json},
        {xmltype, grouped},
        {database, Database},
        {limit, 1},
        {startfrom, 1},
        {search, <<"priref=", Priref1/binary>>}
    ],
    case fetch(URL, Params, Context) of
        {ok, #{
            <<"adlibJSON">> := #{
                <<"recordList">> := #{ <<"record">> := [ R ] }
            }
        }} ->
            {ok, R};
        {ok, #{
            <<"adlibJSON">> := #{
                <<"diagnostic">> := #{
                    <<"error">> := Error
                }
            }
        }} ->
            {error, Error};
        {ok, #{
            <<"adlibJSON">> := #{
                <<"diagnostic">> := #{
                    <<"hits">> := Hits
                }
            }
        }} when Hits =:= 0; Hits =:= <<"0">> ->
            {error, enoent};
        {error, _} = Error ->
            Error
    end.


%% @doc Fetch all records since a certain date. This returns the first set of records and
%% a continuation function for the next set.
-spec fetch_since(Since, Endpoint, z:context()) -> {ok, RecsCont} | {error, api_error()}
    when Since :: date(),
         Endpoint :: mod_adlib:adlib_endpoint(),
         RecsCont :: { list(map()), fun(() -> {ok, RecsCont} | {error, api_error()}) }.
fetch_since(Since, Endpoint, Context) ->
    DT = z_datetime:to_datetime(Since),
    case fetch_since_pager(DT, Endpoint, 1, "'Y-m-d H:i:s'", Context) of
        {error, #{ <<"info">> := _}} ->
            %% Detect datetime format used by Adlib server for modified.
            %% Unfortunately, this can differ between Adlib instances.
            %% Ymd is the legacy format.
            fetch_since_pager(DT, Endpoint, 1, "Ymd", Context);
        Result ->
            Result
    end.


%% @doc Continuation function returned from fetch_since/3.
-spec fetch_since_pager(Since, Endpoint, Offset, DateFormat, z:context()) -> {ok, RecsCont} | {error, api_error()}
    when Since :: calendar:datetime(),
         Endpoint :: mod_adlib:adlib_endpoint(),
         Offset :: pos_integer(),
         DateFormat :: string(),
         RecsCont :: { list(map()), fun(() -> {ok, RecsCont} | {error, api_error()}) }.
fetch_since_pager(Since, Endpoint, Offset, DateFormat, Context) ->
    #adlib_endpoint{
        api_url = URL,
        database = Database,
        extra_arguments = ExtraArgs
    } = Endpoint,
    Tz = z_convert:to_list(Endpoint#adlib_endpoint.timezone),
    SinceFormatted = z_dateformat:format(Since, DateFormat, [{tz, Tz}]),
    Search = build_search_args(ExtraArgs),
    Search1 = <<"(modification>=", SinceFormatted/binary, Search/binary, ")">>,
    Params = [
        {output, json},
        {xmltype, grouped},
        {database, Database},
        {search, Search1},
        {startfrom, Offset},
        {limit, ?ADLIB_PAGELEN}
    ],
    case fetch(URL, Params, Context) of
        {ok, #{
            <<"adlibJSON">> := #{
                <<"diagnostic">> := #{
                    <<"hits">> := Hits
                },
                <<"recordList">> := #{ <<"record">> := Rs }
            }
        }} ->
            Hits1 = z_convert:to_integer(Hits),
            Next = if
                Hits1 > ?ADLIB_PAGELEN + Offset ->
                    Offset1 = ?ADLIB_PAGELEN + Offset,
                    {next, fun() ->
                        ?MODULE:fetch_since_pager(Since, Endpoint, Offset1, DateFormat, Context)
                    end};
                true ->
                    done
            end,
            {ok, {Rs, Next}};
        {ok, #{
            <<"adlibJSON">> := #{
                <<"diagnostic">> := #{
                    <<"error">> := Error
                }
            }
        }} ->
            {error, Error};
        {ok, #{
            <<"adlibJSON">> := #{
                <<"diagnostic">> := #{
                    <<"hits">> := Hits
                }
            }
        }} when Hits =:= 0; Hits =:= <<"0">> ->
            {ok, {[], ok}};
        {error, _} = Error ->
            Error
    end.


build_search_args([]) ->
    <<>>;
build_search_args([{K,V}|Rest]) ->
    <<" and ", K/binary, "=", V/binary, (build_search_args(Rest))/binary>>.



%% @doc List all databases at an endpoint or adlib URL.
-spec list_databases( mod_adlib:adlib_endpoint() | uri_string:uri_string(), z:context() ) -> {ok, term()} | {error, term()}.
list_databases(#adlib_endpoint{ api_url = Url }, Context) ->
    list_databases(Url, Context);
list_databases(Url, Context) ->
    Params = [
        {output, json},
        {xmltype, grouped},
        {command, listdatabases}
    ],
    case fetch(Url, Params, Context) of
        {ok, #{
            <<"adlibJSON">> := #{
                <<"recordList">> := #{ <<"record">> := Rs }
            }
        }} ->
            Rs1 = lists:map(
                fun(#{
                        <<"database">> := [ Db ],
                        <<"datasource">> := [ Source ]
                    }) ->
                    #{
                        database => Db,
                        datasource => Source
                    }
                end, Rs),
            {ok, Rs1};
        {error, _} = Error ->
            Error
    end.


%% @doc Fetch data from an Adlib API endpoint.
fetch(URL, Params, Context) ->
    Params1 = lists:map(
        fun({K, V}) ->
            K1 = z_url:url_encode(K),
            V1 = z_url:url_encode(V),
            <<K1/binary, $=, V1/binary>>
        end,
        Params),
    Params2 = iolist_to_binary( lists:join($&, Params1) ),
    URL1 = <<URL/binary, $?, Params2/binary>>,
    case z_fetch:fetch(URL1, [ {timeout, ?FETCH_TIMEOUT} ], Context) of
        {ok, {_FinalUrl, _Hs, _Length, <<>>}} ->
            [];
        {ok, {_FinalUrl, _Hs, _Length, Body}} ->
            try
                {ok, jsx:decode(Body, [return_maps])}
            catch
                error:badarg ->
                    ?zError("Adlib client illegal return for endpoint ~s",
                            [ URL ], Context),
                    lager:error("Could not decode Adlib response: ~p", [Body]),
                    []
            end;
        {error, {401, _FinalUrl, _Hs, _Length, _Body}} ->
            ?zError("Adlib client 401 for endpoint ~s",
                    [ URL ], Context),
            {error, eacces};
        {error, {403, _FinalUrl, _Hs, _Length, _Body}} ->
            ?zError("Adlib client 403 for endpoint ~s",
                    [ URL ], Context),
            {error, eacces};
        {error, {404, _FinalUrl, _Hs, _Length, _Body}} ->
            ?zError("Adlib client 404 for endpoint ~s",
                    [ URL ], Context),
            {error, enoent};
        {error, {410, _FinalUrl, _Hs, _Length, _Body}} ->
            ?zError("Adlib client 410 for endpoint ~s",
                    [ URL ], Context),
            {error, gone};
        {error, {Code, _FinalUrl, _Hs, _Length, _Body}} ->
            ?zError("Adlib client error ~p for endpoint ~s",
                    [ Code, URL ], Context),
            {error, Code};
        {error, Result} = Error ->
            ?zError("Adlib client error for endpoint ~s: ~p",
                    [URL, Result], Context),
            Error
    end.


