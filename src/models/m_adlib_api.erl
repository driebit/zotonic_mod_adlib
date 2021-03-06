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

-module(m_adlib_api).

-export([
    m_get/3,

    fetch_record/3,
    fetch_since/3,
    list_databases/2,

    image_url/2,
    image_url_scaled/4
    ]).

% Exported for continuation function returned from fetch_since.
-export([
    fetch_since_pager/4
    ]).

-behaviour(zotonic_model).

-include_lib("kernel/include/logger.hrl").
-include_lib("zotonic_core/include/zotonic.hrl").
-include("../../include/mod_adlib.hrl").

% Default timeout of z_url_fetch is 20sec, set to 50sec.
-define(FETCH_TIMEOUT, 60000).

% Image fetch timeout is 10 minutes.
-define(FETCH_IMAGE_TIMEOUT, 600000).

% Default limit for Adlib searches
-define(ADLIB_PAGELEN, 50).


-type api_error() :: enoent
                   | eacces
                   | map()      % #{  <<"info">> := binary(), <<"message">> := binary() }
                   | term().

-type date() :: calendar:datetime()
              | calendar:date()
              | binary()
              | string()
              | integer()
              | undefined.

-export_type([ api_error/0, date/0 ]).


%% @doc API interface for Adlib.
-spec m_get( list(), zotonic_model:opt_msg(), z:context()) -> zotonic_model:return().
m_get([ <<"list_databases">> | Rest ], #{ payload := Payload }, Context) ->
    case z_acl:is_allowed(use, mod_adlib, Context) of
        true ->
            case maps:get(<<"api_url">>, Payload, undefined) of
                AdlibUrl when is_binary(AdlibUrl) ->
                    Endpoint = #adlib_endpoint{
                        api_url = AdlibUrl,
                        database = <<"collect">>
                    },
                    case list_databases(Endpoint, Context) of
                        {ok, List} ->
                            {ok, {List, Rest}};
                        {error, _} = Error ->
                            Error
                    end;
                _ ->
                    {error, adlib_url_missing}
            end;
        false ->
            {error, eacces}
    end;
m_get([ <<"endpoints">> | Rest ], _, Context) ->
    case z_acl:is_allowed(use, mod_adlib, Context) of
        true ->
            Es = lists:map(
                fun(E) ->
                    #{
                        <<"name">> => E#adlib_endpoint.name,
                        <<"database">> => E#adlib_endpoint.database
                    }
                end,
                lists:sort( mod_adlib:endpoints(Context) )),
            {ok, {Es, Rest}};
        false ->
            {error, eacces}
    end.


%% @doc Fetch a single record from an adlib API endpoint.
-spec fetch_record(mod_adlib:adlib_endpoint(), Priref :: binary() | string() | integer(), z:context() ) -> {ok, map()} | {error, api_error()}.
fetch_record(Endpoint, Priref, Context) ->
    #adlib_endpoint{
        api_url = URL,
        database = Database
    } = Endpoint,
    Priref1 = z_convert:to_binary(Priref),
    Params = [
        {database, Database},
        {limit, 1},
        {startfrom, 1},
        {search, <<"priref=", Priref1/binary>>}
    ],
    case fetch(URL, Params, Context) of
        {ok, #{
            <<"adlibxml">> := [
                #{ <<"recordlist">> := [
                    #{ <<"record">> := [ R ] }
                ] }
            ]
        }} ->
            {ok, R};
        {ok, #{
            <<"adlibxml">> := [
                #{ <<"diagnostic">> := [ #{<<"error">> := [ Error ] } ]}
            ]
        }} ->
            ?LOG_ERROR("Adlib: Error fetching record ~s (~s ~s): ~p",
                       [ Priref1, URL, Database, Error ]),
            {error, Error};
        {ok, #{
            <<"adlibxml">> := [
                #{ <<"diagnostic">> := [ #{ <<"hits">> := [ <<"0">> ] } ]}
            ]
        }} ->
            ?LOG_ERROR("Adlib: Error fetching record ~s (~s ~s): not found",
                       [ Priref1, URL, Database ]),
            {error, enoent};
        {error, Reason} = Error ->
            ?LOG_ERROR("Adlib: Error fetching record ~s (~s ~s): ~p",
                       [ Priref1, URL, Database, Reason ]),
            Error
    end.


%% @doc Fetch all records since a certain date. This returns the first set of records and
%% a continuation function for the next set. This returns a minimal record with only the priref.
%% Use the fetch_record function to fetch the complete record.
%%
%% More info: http://api.adlibsoft.com/api/functions/search
-spec fetch_since(Endpoint, Since, z:context()) -> {ok, RecsCont} | {error, api_error()}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Since :: date(),
         RecsCont :: { list(map()), fun(() -> {ok, RecsCont} | {error, api_error()}) | done }.
fetch_since(Endpoint, Since, Context) ->
    DT = case Since of
        None when None =:= undefined;
                  None =:= <<>>;
                  None =:= "";
                  None =:= 0 ->
            undefined;
        _ ->
            z_datetime:to_datetime(Since)
    end,
    fetch_since_pager(Endpoint, DT, 1, Context).


%% @doc Continuation function returned from fetch_since/3. Only returns the priref for
%% all records, use fetch_record to obtain the complete record.
%%
%% More info: http://api.adlibsoft.com/api/functions/search
-spec fetch_since_pager(Endpoint, Since, Offset, z:context()) -> {ok, RecsCont} | {error, api_error()}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Since :: calendar:datetime() | undefined,
         Offset :: pos_integer(),
         RecsCont :: { list(map()), fun(() -> {ok, RecsCont} | {error, api_error()}) }.
fetch_since_pager(Endpoint, Since, Offset, Context) ->
    #adlib_endpoint{
        api_url = URL,
        database = Database,
        extra_arguments = ExtraArgs,
        date_format = DateFormat
    } = Endpoint,
    Search = build_search_args(ExtraArgs),
    Search1 = case Since of
        undefined ->
            Search;
        _ ->
            Tz = z_convert:to_list(Endpoint#adlib_endpoint.timezone),
            SinceFormatted = z_dateformat:format(Since, DateFormat, [{tz, Tz}]),
             [ <<"modification>=", SinceFormatted/binary>> | Search ]
    end,
    Search2 = case iolist_to_binary(lists:join(" and ", Search1)) of
        <<>> -> <<"all">>;
        S -> <<"(", S/binary, ")">>
    end,
    Params = [
        {xmltype, grouped},
        {database, Database},
        {search, Search2},
        {startfrom, Offset},
        {fields, "priref"},
        {limit, ?ADLIB_PAGELEN}
    ],
    case fetch(URL, Params, Context) of
        {ok, #{
            <<"adlibxml">> := [
                #{
                    <<"diagnostic">> := [ #{ <<"hits">> := [ Hits ] } ],
                    <<"recordlist">> := [
                        #{
                            <<"record">> := Rs
                        }
                    ]
                }
            ]
        }} ->
            Hits1 = z_convert:to_integer(Hits),
            Next = if
                Hits1 > ?ADLIB_PAGELEN + Offset ->
                    Offset1 = ?ADLIB_PAGELEN + Offset,
                    {next, fun() ->
                        ?MODULE:fetch_since_pager(Endpoint, Since, Offset1, Context)
                    end};
                true ->
                    done
            end,
            ?LOG_NOTICE("Adlib: fetch records since ~p: ~p found, offset ~p (~s ~s)",
                        [ Since, Hits1, Offset, URL, Database ]),
            {ok, {Rs, Next}};
        {ok, #{
            <<"adlibxml">> := [
                #{ <<"diagnostic">> := [ #{<<"error">> := [ Error ] } ] }
            ]
        }} ->
            ?LOG_ERROR("Adlib: Error fetch records since ~p, offset ~p (~s ~s): ~p",
                       [ Since, Offset, URL, Database, Error ]),
            {error, Error};
        {ok, #{
            <<"adlibxml">> := [
                #{ <<"diagnostic">> := [ #{ <<"hits">> := [ <<"0">> ] } ] }
            ]
        }} ->
            ?LOG_NOTICE("Adlib: fetch records since ~p: ~p found, offset ~p (~s ~s)",
                       [ Since, 0, Offset, URL, Database ]),
            {ok, {[], done}};
        {error, Reason} = Error ->
            ?LOG_ERROR("Adlib: Error fetch records since ~p, offset ~p (~s ~s): ~p",
                       [ Since, Offset, URL, Database, Reason ]),
            Error
    end.


build_search_args([]) ->
    [];
build_search_args([{K,V}|Rest]) ->
    K1 = z_convert:to_binary(K),
    V1 = z_convert:to_binary(V),
    [ <<K1/binary, "=", V1/binary>> | build_search_args(Rest) ].



%% @doc List all databases at an endpoint or adlib URL.
-spec list_databases( mod_adlib:adlib_endpoint() | uri_string:uri_string(), z:context() ) -> {ok, term()} | {error, term()}.
list_databases(#adlib_endpoint{ api_url = Url }, Context) ->
    list_databases(Url, Context);
list_databases(Url, Context) ->
    Params = [
        {xmltype, grouped},
        {command, listdatabases}
    ],
    case fetch(Url, Params, Context) of
        {ok, #{
            <<"adlibxml">> := [
                #{
                    <<"recordlist">> := [ #{ <<"record">> := Rs } ]
                }
            ]
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

%% @doc Generate the image_url for a named image.
%% See http://api.adlibsoft.com/api/functions/getcontent
-spec image_url( Endpoint, Filename ) -> {ok, Url}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Filename :: binary() | string(),
         Url :: binary().
image_url(#adlib_endpoint{ api_url = Url }, Filename) ->
    {ok, iolist_to_binary([
        Url,
        "?command=getcontent",
        "&server=images",
        "&value=", z_url:url_encode(Filename)
        ])}.

%% @doc Generate the image_url for a named image with a max bounding box. The returned
%% Image fits in a square with the given size, it will not be upscaled. The image
%% format will be JPEG.
%% As the Adlib API does not support fetching the image size we download the imahe
%% to determine its bounding box. From there we generate the requested URL.
%% See http://api.adlibsoft.com/api/functions/getcontent
-spec image_url_scaled( Endpoint, Filename, MaxSize, Context ) -> {ok, UrlProps} | {error, term()}
    when Endpoint :: mod_adlib:adlib_endpoint(),
         Filename :: binary() | string(),
         UrlProps :: map(),
         MaxSize :: non_neg_integer(),
         Context :: z:context().
image_url_scaled(#adlib_endpoint{ api_url = Url }, Filename, MaxSize, Context) ->
    FullImageUrl = iolist_to_binary([
        Url,
        "?command=getcontent",
        "&server=images",
        "&value=", z_url:url_encode(Filename)
        ]),
    Options = [
        {fetch_options, [ {timeout, ?FETCH_IMAGE_TIMEOUT} ]}
    ],
    case m_media:download_file(FullImageUrl, Options, Context) of
        {ok, TmpFile, _} ->
            case z_media_identify:identify_file_direct(TmpFile, <<"download">>) of
                {ok, #{
                    <<"width">> := Width,
                    <<"height">> := Height,
                    <<"mime">> := <<"image/", _/binary>> = Mime
                }} ->
                    file:delete(TmpFile),
                    {W, H} = scale(MaxSize, Width, Height),
                    Format = format(Mime, W, H),
                    ImageUrl = iolist_to_binary([
                            Url,
                            "?command=getcontent",
                            "&server=images",
                            "&width=", integer_to_list(W),
                            "&height=", integer_to_list(H),
                            "&format=", Format,
                            "&value=", z_url:url_encode(Filename)
                            ]),
                    {ok, #{
                        <<"url">> => ImageUrl,
                        <<"mime">> => Mime,
                        <<"width">> => Width,
                        <<"height">> => Height
                    }};
                {ok, #{
                    <<"mime">> := Mime
                }} ->
                    % Not an image, use original data URL
                    file:delete(TmpFile),
                    {ok, #{
                        <<"url">> => FullImageUrl,
                        <<"mime">> => Mime,
                        <<"width">> => undefined,
                        <<"height">> => undefined
                    }};
                {error, _} = Error ->
                    file:delete(TmpFile),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

scale(Max, Width, Height) when Max > Height, Max > Width ->
    {Width, Height};
scale(Max, Width, Height) when Width >= Height, Width > Max ->
    {Max, erlang:trunc(Height * (Max / Width))};
scale(Max, Width, Height) ->
    {erlang:trunc(Width * (Max / Height)), Max}.

format(_, W, _) when W > 1000 -> <<"jpg">>;
format(_, _, H) when H > 1000 -> <<"jpg">>;
format(<<"image/png">>, _, _) -> <<"png">>;
format(<<"image/gif">>, _, _) -> <<"png">>;
format(<<"image/tiff">>, _, _) -> <<"png">>;
format(_, _, _) -> <<"jpg">>.

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
                {ok, adlib_xml:parse(Body)}
            catch
                error:badarg ->
                    ?zError("Adlib client illegal return for endpoint ~s",
                            [ URL ], Context),
                    ?LOG_ERROR("Could not decode Adlib response: ~p", [Body]),
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


