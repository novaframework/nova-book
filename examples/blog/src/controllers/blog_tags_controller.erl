-module(blog_tags_controller).
-fallback_controller(blog_api_fallback_controller).

-export([list/1, show/1, create/1, update/1, delete/1]).

list(_Req) ->
    {ok, Tags} = blog_tags:list_tags(),
    {json, #{tags => [blog_api_json:tag(Tag) || Tag <- Tags]}}.

show(#{bindings := #{~"id" := Id}}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id, [posts]),
        {json, blog_api_json:tag(Tag)}
    end.

create(#{json := Params}) ->
    maybe
        {ok, #{id := Id} = Tag} ?= blog_tags:create_tag(Params),
        Location = iolist_to_binary([~"/api/tags/", integer_to_binary(Id)]),
        {json, 201, #{~"location" => Location}, blog_api_json:tag(Tag)}
    end.

update(#{bindings := #{~"id" := Id}, json := Params}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id),
        {ok, Updated} ?= blog_tags:update_tag(Tag, Params),
        {json, blog_api_json:tag(Updated)}
    end.

delete(#{bindings := #{~"id" := Id}}) ->
    maybe
        {ok, Tag} ?= blog_tags:get_tag(Id),
        {ok, _} ?= blog_tags:delete_tag(Tag),
        {status, 204}
    end.
