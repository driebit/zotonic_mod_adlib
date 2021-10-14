%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2021 Driebit BV
%% @doc Parse XML to a JSON-alike Erlang map.

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

-module(adlib_xml).

-export([
    parse/1,

    test/0
    ]).

%% @doc Parse an XML document to a JSON compatible map. Attributes will be added
%% as keys in an <tt>@attributes</tt> key. Elements will be mapped to keys with value lists.
-spec parse( XML :: binary() ) -> {ok, map()} | {error, term()}.
parse(XML) when is_binary(XML) ->
    io:format("~s~n~n~n", [ XML ]),
    case z_html_parse:parse(XML) of
        {ok, Tree} ->
            io:format("~p~n~n~n~n", [ Tree ]),
            tree_to_map(Tree, #{});
        {error, _} = Error ->
            Error
    end;
parse({_, _, _} = Tree) ->
    tree_to_map(Tree, #{}).

tree_to_map({Tag, [], Elts}, TagMap) when is_list(Elts) ->
    case lists:any(fun is_element/1, Elts) of
        true ->
            EltMap = lists:foldr(
                fun tree_to_map/2,
                #{},
                Elts),
            TagMap#{
                Tag => [ EltMap | maps:get(Tag, TagMap, []) ]
            };
        false ->
            % Leave node, sub elements are values
            Vs = map_values(Elts),
            TagMap#{
                Tag => lists:flatten([ Vs | maps:get(Tag, TagMap, []) ])
            }
    end;
tree_to_map({Tag, Attrs, Elts}, TagMap) when is_list(Elts) ->
    case lists:any(fun is_element/1, Elts) of
        true ->
            EltMap = lists:foldr(
                fun tree_to_map/2,
                #{},
                Elts),
            EltMap1 = EltMap#{
                <<"@attributes">> => maps:from_list(Attrs)
            },
            TagMap#{
                Tag => [ EltMap1 | maps:get(Tag, TagMap, []) ]
            };
        false ->
            % Leave node, but with attributes
            V = #{
                <<"@attributes">> => maps:from_list(Attrs),
                <<"value">> => map_values(Elts)
            },
            TagMap#{
                Tag => [ V | maps:get(Tag, TagMap, []) ]
            }
    end;
tree_to_map(_, TagMap) ->
    TagMap.

map_values(Vs) ->
    lists:filtermap(fun map_value/1, Vs).

map_value(B) when is_binary(B) -> {true, B};
map_value(_) -> false.

is_element({Tag, Attrs, Elts}) when is_binary(Tag), is_list(Attrs), is_list(Elts) ->
    true;
is_element(_) ->
    false.


test() ->
    parse( {<<"Documentation">>,[],
       [{<<"documentation.author">>,[],[<<"anoniem/anonymous">>]},
        {<<"documentation.notes">>,[{<<"lang">>,<<>>}],[]},
        {<<"documentation.page_reference">>,[],[]},
        {<<"documentation.shelfmark">>,[],[]},
        {<<"documentation.title">>,[],
         [{<<"title">>,[],[<<"Landmonsterrollen">>]},
          {<<"title">>,[],[<<"Landmonsterrollen">>]},
          {<<"year_of_publication">>,[],[<<"1691-1790">>]}]},
        {<<"documentation.title.lead_word">>,[{<<"lang">>,<<>>}],[]},
        {<<"documentation.title.lref">>,[],[<<"900000009">>]}]}).


