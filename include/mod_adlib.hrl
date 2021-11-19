%% Definitions for Adlib access routines.

% The date_format is the datetime format used by Adlib server for modified.
% Unfortunately, this can differ between Adlib instances.
% "Ymd" is the legacy format.

-record(adlib_endpoint, {
    name :: binary(),
    api_url :: uri_string:uri_string(),
    database :: binary(),
    extra_arguments = [] :: list( mod_adlib:api_arg() ),
    date_format = "'Y-m-d H:i:s'" :: string(),
    timezone = <<"UTC">> :: binary(),
    triples_fun = undefined :: fun( (mod_adlib:adlib_endpoint(), map(), z:context())
                                 -> {ok, [ zotonic_rdf:rdf_triple() ]} | {error, term()})
                            | undefined
}).

% Notification to collect all adlib endpoints from the site and modules.
% Type: map
% Return: [ mod_adlib:adlib_endpoint() ]
-record(adlib_endpoints, {}).
