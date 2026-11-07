-module(blog_post_metadata).
-include_lib("kura/include/kura.hrl").

-export([fields/0]).
-export([changeset/2]).

fields() ->
    [
        #kura_field{name = seo_title, type = string},
        #kura_field{name = seo_description, type = string},
        #kura_field{name = cover_image_url, type = string},
        #kura_field{name = reading_minutes, type = integer}
    ].

changeset(Data, Params) ->
    CS = kura_changeset:cast(?MODULE, Data, Params, [
        seo_title, seo_description, cover_image_url, reading_minutes
    ]),
    CS1 = kura_changeset:validate_length(CS, seo_title, [{max, 70}]),
    CS2 = kura_changeset:validate_length(CS1, seo_description, [{max, 160}]),
    CS3 = kura_changeset:validate_format(CS2, cover_image_url, ~"^https://"),
    kura_changeset:validate_number(CS3, reading_minutes, [{greater_than, 0}]).
