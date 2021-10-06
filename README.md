Adlib support for Zotonic
=========================

[Adlib](http://www.adlibsoft.nl). is a collection organization and management tool for museums and libraries.

This [Zotonic](http://www.zotonic.com) module adds support routines for accessing and importing these collections into a Zotonic system.


## API functions

The remote Adlib endpoint is configured using a record:

```erlang
#adlib_endpoint{
    name = <<"My Foobar database">>,
    api_url = <<"https://foobar.adlibhosting.com/mydatabase/wwwopac.ashx">>,
    database = <<"collect">>,
    extra_arguments = [],
    timezone = <<"UTC">>
}

```

The API URL is the base URL for all API calls.

The database is one of the databases available on the remote system.


### Listing all databases in Adlib

A list of databases can be requested:


```erlang
adlib_api:list_databases(<<"https://foobar.adlibhosting.com/mydatabase/wwwopac.ashx">>, z:c(yoursite)).

```

This returns a list of databases:

```erlang
{ok,[#{database => <<"fullcatalogue">>,
       datasource => <<"document>fullcatalogue">>},
     #{database => <<"books">>,
       datasource => <<"document>book">>},
     #{database => <<"collect">>,
       datasource => <<"document>collect">>},
     #{database => <<"dublincore">>,
       datasource => <<"document>resource">>},
     #{database => <<"thesau">>,
       datasource => <<"thesau">>}]}

```


### Fetching a single record

Records are fetched using their `priref` id.

Let `E` be a configured `#adlib_endpoint{}` then this fetches a specific record:

```erlang
adlib_api:fetch_record(10594, E, z:c(yoursite)).
```

Returns:

```erlang
{ok, #{<<"@attributes">> =>
          #{<<"created">> => <<"2021-05-16T15:19:38">>,
            <<"modification">> => <<"2021-05-20T16:32:20">>,
            <<"priref">> => <<"10594">>,
            <<"selected">> => <<"False">>},
      <<"Alternative_number">> =>
          [#{<<"alternative_number">> => [<<"Carte di Castello 13">>]}],

    (..)
}}

```

If the record is not found then `{error, enoent}` is returned.


### Fetching all records since a date

For syncing the Adlib content with Zotonic there is a function to fetch all records
modified since a certain date (in UTC).

The date can be a relative date `"-1 week"`, a timestamp `1633526615`, a date
`{2021, 10, 5}`, a datetime `{{2021, 10, 5}, {12, 0, 0}}` or a string with a date 
`"2021-10-05 12:00:00"`.

The `timezone` of the Adlib endpoint is used to convert the date to the date/time used
by Adlib. Adlib uses local time in their output, where the timezone depends on the
configuration of the Adlib server.

```erlang
adlib_api:fetch_since(<<"-1 month">>, E, z:c(yoursite)).
```

Returns:

```erlang
{ok, {List, Next}}

```

Where `List` is a list of Adlib records and `Next` is the continuation function.
`Next` can be:

- `done`
- `{next, Fun}`

The `Fun` can be called as `Fun()` and will return the next result.

The records are sorted by _priref_.


### Fetching all records

To fetch all records, pass an empty date to the `fetch_since`:

```erlang
adlib_api:fetch_since(<<"-1 month">>, E, z:c(yoursite)).
```

The records are sorted by _priref_.


## Mapping Adlib records to RDF triples.

There are basic routines to map an Adlib record to RDF statements.


### Generate an unique URN for an Adlib record

To identify a resource, an unique URN is needed. This routine generates the unique URN using
the Adlib endpoint specification and the _priref_ in the Adlib record.

```erlang
adlib_rdf:uri(Endpoint, Record).
```

Returns something like:

```erlang
{ok, <<"urn:adlib:foobar.adlibhosting.com/mydatabase/collect/10594">>}

```

### Check if an URN matches an endpoint

To find the endpoint for an Adlib URN you can check the URN against an Endpoint:

```erlang
adlib_rdf:is_matching_uri(Endpoint, <<"urn:adlib:foobar.adlibhosting.com/mydatabase/collect/10594">>).
```

This returns a boolean (in this case `true`).




