%% Definitions for Adlib access routines.

-record(adlib_endpoint, {
    name :: binary(),
    api_url :: uri_string:uri_string(),
    database :: binary(),
    extra_arguments = [] :: list( mod_adlib:api_arg() ),
    timezone = <<"UTC">>
}).
